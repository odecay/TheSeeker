pub mod arc_attack;
pub mod particles;

use std::mem;

use bevy::log::tracing_subscriber::filter::targets;
use theseeker_engine::assets::animation::SpriteAnimation;
use theseeker_engine::gent::Gent;
use theseeker_engine::physics::{
    update_sprite_colliders, Collider, LinearVelocity, PhysicsSet, PhysicsWorld, GROUND,
};
use theseeker_engine::script::ScriptPlayer;

use super::enemy::EnemyGfx;
use super::gentstate::Facing;
use super::player::PlayerGfx;
use super::player::{FocusAbility, FocusState};
use crate::game::player::{Passive, PlayerStateSet};
use crate::game::{attack::particles::AttackParticlesPlugin, gentstate::Dead};
use crate::game::{
    enemy::{Defense, Enemy, EnemyStateSet},
    player::Passives,
};
use crate::prelude::*;
use crate::{
    camera::CameraRig,
    game::attack::arc_attack::{arc_projectile, Projectile},
};

pub struct AttackPlugin;

impl Plugin for AttackPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(AttackParticlesPlugin);
        app.add_gametick_event::<DamageInfo>();
        app.init_resource::<KillCount>();
        app.add_systems(
            GameTickUpdate,
            (
                arc_projectile,
                // attack_damage,
                (
                    determine_attack_targets,
                    apply_attack_modifications,
                    apply_attack_damage,
                    kill_on_damage,
                )
                    .chain(),
                attack_tick,
                despawn_projectile,
                attack_cleanup,
                damage_flash,
                knockback,
            )
                .chain()
                .after(update_sprite_colliders)
                .after(PlayerStateSet::Behavior)
                .after(EnemyStateSet::Behavior)
                .before(PlayerStateSet::Collisions)
                .before(EnemyStateSet::Collisions),
        );
    }
}
#[derive(Component)]
pub struct Health {
    pub current: u32,
    pub max: u32,
}

///Component applied to Gfx entity sibling of Gent which has been damaged
#[derive(Component)]
pub struct DamageFlash {
    pub current_ticks: u32,
    pub max_ticks: u32,
}

#[derive(Bundle)]
pub struct AttackBundle {
    pub attack: Attack,
    pub collider: Collider,
    //transform?
}

#[derive(Component)]
pub struct Attack {
    pub current_lifetime: u32,
    pub max_lifetime: u32,
    pub damage: u32,
    /// Maximum number of targets that can be hit by this attack at once.
    pub max_targets: u32,
    pub attacker: Entity,
    /// Includes every single instance of damage that was applied.
    /// (even against the same enemy)
    pub damaged: Vec<DamageInfo>,
    // pub damaged: HashSet<(Entity, DamageInfo)>,
    /// Tracks which entities collided with the attack, and still remain in contact.
    /// Not stored in damage info, because the collided entities might be
    /// different from the entities that damage is applied. (due to max_targets)
    pub collided: HashSet<Entity>,

    /// Unique entities that were in contact with collider and took damage.
    /// and are still in contact with the attack collider.
    pub damaged_set: HashSet<Entity>,

    /// Unique entities that were in contact with collider and were valid targets.
    /// and are still in contact with the attack collider.
    pub target_set: HashSet<Entity>,

    /// used to track if multiple hits are in the same attack or not
    pub new_group: bool,

    ///
    pub can_backstab: bool,

    /// set true if the hit can crit
    pub can_crit: bool,
}
impl Attack {
    /// Lifetime is in game ticks
    pub fn new(lifetime: u32, attacker: Entity) -> Self {
        Attack {
            current_lifetime: 0,
            max_lifetime: lifetime,
            damage: 20,
            max_targets: 3,
            attacker,
            // damaged: Vec::new(),
            damaged: Default::default(),
            collided: Default::default(),
            damaged_set: Default::default(),
            target_set: Default::default(),
            new_group: false,
            can_backstab: false,
            can_crit: false,
        }
    }
}

/// Event sent when damage is applied
#[derive(Event, Clone, Copy, Hash, Eq, PartialEq)]
pub struct DamageInfo {
    /// Entity that got damaged
    pub entity: Entity,
    /// The tick it was damaged
    pub tick: u64,
    /// Amount of damage that was actually applied
    pub amount: u32,
    pub crit: bool,
}

///Component added to attack entity to indicate it causes knockback
#[derive(Component, Default)]
pub struct Pushback {
    pub direction: f32,
    pub strength: f32,
}

///Component added to an entity damaged by a pushback attack
#[derive(Component, Default, Debug)]
pub struct Knockback {
    pub ticks: u32,
    pub max_ticks: u32,
    pub direction: f32,
    pub strength: f32,
}

impl Knockback {
    pub fn new(direction: f32, strength: f32, max_ticks: u32) -> Self {
        Knockback {
            ticks: 0,
            max_ticks,
            direction,
            strength,
        }
    }
}

//checks nearest entities, modifies attacks targets
pub fn determine_attack_targets(
    mut attack_query: Query<(
        Entity,
        &GlobalTransform,
        &mut Attack,
        &Collider,
    )>,
    // damageable_query: Query<(Entity, &GlobalTransform), (With<Collider>, With<Health>, With<Gent>)>,
    damageable_query: Query<&GlobalTransform, (With<Collider>, With<Health>, With<Gent>)>,
    spatial_query: Res<PhysicsWorld>,
) {
    for (entity, transform, mut attack, collider) in attack_query.iter_mut() {
        let mut newly_collided: HashSet<Entity> = HashSet::default();
        let intersections = spatial_query.intersect(
            transform.translation().xy(),
            collider.0.shape(),
            collider
                .0
                .collision_groups()
                .with_filter(collider.0.collision_groups().filter | GROUND),
            Some(entity),
        );
        // let intersections_empty = intersections.is_empty();
        let mut targets = intersections
            .into_iter()
            // Filters out everything that's not damageable or one of the nearest max_targets entities to attack
            .filter_map(|colliding_entity| {
                if let Ok(damageable_transform) = damageable_query.get(colliding_entity) {
                    newly_collided.insert(entity);
                    let dist = damageable_transform
                        .translation()
                        .xy()
                        .distance_squared(transform.translation().xy());
                    Some((colliding_entity, dist))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        targets.sort_by(|(_, dist1), (_, dist2)| dist1.total_cmp(dist2));
        // let targets_empty = targets.is_empty();

        // Get the closest targets
        let valid_targets = targets
            .into_iter()
            .take(attack.max_targets as usize - attack.damaged_set.len())
            .map(|(e, _)| e)
            .collect::<Vec<_>>();

        for entity in valid_targets.iter() {
            //if we already damaged this entity
            if attack.damaged_set.contains(entity) {
                continue;
            };

            //if the entity is not damageable
            if damageable_query.get(*entity).is_err() {
                continue;
            }
            // let Ok(dmgbl_trnsfrm) = damageable_query.get(*entity) else {
            //     continue;
            // };

            //TODO: make sure this is correct entity
            //TODO: change to target_set?
            // attack.damaged_set.insert(*entity);
            attack.target_set.insert(*entity);
        }
    }
}

pub fn apply_attack_modifications(
    mut attack_query: Query<&mut Attack, Without<Gent>>,
    mut attacker_query: Query<
        (
            Option<&mut Crits>,
            Option<&mut FocusAbility>,
            Option<&Passives>,
        ),
        With<Gent>,
    >,
) {
    for mut attack in attack_query.iter_mut() {
        //if there are no targets we dont want to do anything
        if attack.target_set.is_empty() {
            continue;
        }
        //if there is an attacker, apply relevant buffs
        if let Ok((maybe_crits, maybe_focus, maybe_passives)) =
            attacker_query.get_mut(attack.attacker)
        {
            // crit multiplier
            if let Some(mut crit) = maybe_crits {
                if crit.next_hit_is_critical {
                    attack.damage = (attack.damage as f32 * crit.crit_damage_multiplier) as u32;
                    //
                    attack.can_crit = true;
                    crit.next_hit_is_critical = false;
                }
                //TODO: come back if something is broken with crit damage
                //move to application of damage?
                if attack.new_group {
                    crit.counter += 1;
                }
            }

            //focus multiplier
            if let Some(mut focus) = maybe_focus {
                if focus.state != FocusState::InActive {
                    attack.damage *= 2;
                    focus.state = FocusState::Applied
                }
            }

            //passives
            if let Some(passives) = maybe_passives {
                //backstab
                //check this later on application of damage for each enemy
                if passives.contains(&Passive::Backstab) {
                    attack.can_backstab = true;
                }
            }
        }
    }
}

pub fn apply_attack_damage(
    mut attack_query: Query<(
        &mut Attack,
        &GlobalTransform,
        Option<&Pushback>,
        Option<&Projectile>,
    )>,
    mut target_query: Query<
        (
            Entity,
            &mut Health,
            &GlobalTransform,
            &Facing,
            Has<Defense>,
        ),
        With<Gent>,
    >,
    mut damage_events: EventWriter<DamageInfo>,
    mut commands: Commands,
    time: Res<GameTime>,
) {
    for (mut attack, transform, maybe_pushback, maybe_projectile) in attack_query.iter_mut() {
        let target_set = mem::take(&mut attack.target_set);
        //do i want to do this?
        let damaged_set = mem::take(&mut attack.damaged_set);
        // let target_set = target_set.difference(&damaged_set);
        for target in target_set.difference(&damaged_set) {
            if let Ok((t_entity, mut health, t_transform, t_facing, is_defending)) =
                target_query.get_mut(*target)
            {
                let mut damage = attack.damage;
                //modify damage based on target specific qualities
                if attack.can_backstab {
                    let is_backstab = match *t_facing {
                        Facing::Right => t_transform.translation().x > transform.translation().x,
                        Facing::Left => t_transform.translation().x < transform.translation().x,
                    };
                    if is_backstab {
                        damage *= 2;
                    }
                }
                if is_defending {
                    damage /= 4;
                }

                let crit = attack.can_crit;

                //apply damage to the targets health
                health.current = health.current.saturating_sub(damage);
                let damage_info = DamageInfo {
                    entity: t_entity,
                    tick: time.tick(),
                    amount: damage,
                    crit,
                };
                //send a damage event and add the damage info to the attack
                attack.damaged.push(damage_info);
                // attack.damaged.insert((t_entity, damage_info));
                attack.damaged_set.insert(t_entity);
                damage_events.send(damage_info);

                //apply Knockback
                if let Some(pushback) = maybe_pushback {
                    commands.entity(t_entity).insert(Knockback::new(
                        pushback.direction,
                        pushback.strength,
                        16,
                    ));
                }
            }
        }
        attack.damaged_set.extend(damaged_set);
    }
}

pub fn despawn_projectile(
    query: Query<(Entity, &Attack), With<Projectile>>,
    mut commands: Commands,
) {
    for (entity, attack) in query.iter() {
        if attack.current_lifetime > 1 && !attack.damaged_set.is_empty() {
            //TODO: ensure projectiles have a max_targets of 1/or however many is correct
            // if attack.current_lifetime > 1 && attack.damaged_set.len() == attack.max_targets as usize {
            // Note: purposefully does not despawn child entities, nor remove the
            // reference, so that child particle systems have the option of lingering
            commands.entity(entity).despawn();
        }
    }
}

pub fn kill_on_damage(
    query: Query<(Entity, &Health), With<Gent>>,
    mut damage_events: EventReader<DamageInfo>,
    mut commands: Commands,
) {
    for damage_info in damage_events.read() {
        if let Ok((entity, health)) = query.get(damage_info.entity) {
            if health.current == 0 {
                commands.entity(entity).insert(Dead::default());
            }
        }
    }
}

pub fn attack_damage(
    spatial_query: Res<PhysicsWorld>,
    mut query: Query<(
        Entity,
        &GlobalTransform,
        &mut Attack,
        &Collider,
        Option<&Pushback>,
        Option<&Projectile>,
    )>,
    mut damageable_query: Query<(
        Entity,
        &mut Health,
        &Collider,
        &Gent,
        &GlobalTransform,
        &Facing,
        Has<Defense>,
        Has<Enemy>,
    )>,

    passives: Query<&Passives, Without<Attack>>,
    mut crits: Query<(&mut Crits), Without<Attack>>,
    mut focus: Query<(&mut FocusAbility), Without<Attack>>,
    mut gfx_query: Query<Entity, Or<(With<PlayerGfx>, With<EnemyGfx>)>>,
    mut commands: Commands,
    mut rig: ResMut<CameraRig>,
    time: Res<GameTime>,
) {
    for (entity, pos, mut attack, attack_collider, maybe_pushback, maybe_projectile) in
        query.iter_mut()
    {
        let mut newly_collided: HashSet<Entity> = HashSet::default();
        let intersections = spatial_query.intersect(
            pos.translation().xy(),
            attack_collider.0.shape(),
            attack_collider
                .0
                .collision_groups()
                .with_filter(attack_collider.0.collision_groups().filter | GROUND),
            Some(entity),
        );
        let intersections_empty = intersections.is_empty();
        let mut targets = intersections
            .into_iter()
            // Filters out everything that's not damageable or one of the nearest max_targets entities to attack
            .filter_map(|colliding_entity| {
                if let Ok((_, _, _, _, dmgbl_pos, _, _, _)) = damageable_query.get(colliding_entity)
                {
                    newly_collided.insert(entity);
                    let dist = dmgbl_pos
                        .translation()
                        .xy()
                        .distance_squared(pos.translation().xy());
                    Some((colliding_entity, dist))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        targets.sort_by(|(_, dist1), (_, dist2)| dist1.total_cmp(dist2));
        let targets_empty = targets.is_empty();
        // Get the closest targets
        let valid_targets = targets
            .into_iter()
            .take(attack.max_targets as usize)
            .map(|(e, _)| e)
            .collect::<Vec<_>>();

        for entity in valid_targets.iter() {
            if attack.damaged_set.contains(entity) {
                continue;
            };

            let Ok((
                entity,
                mut health,
                collider,
                gent,
                dmgbl_trnsfrm,
                facing,
                is_defending,
                is_enemy,
            )) = damageable_query.get_mut(*entity)
            else {
                continue;
            };

            attack.damaged_set.insert(entity);
            let p = &Passives::default();

            let attacker_passives = passives.get(attack.attacker).unwrap_or(p);

            //apply damage modifiers before applying damage
            //defense modifier
            let mut damage_dealt = if is_defending {
                attack.damage / 4
            } else {
                attack.damage
            };

            //crit multiplier
            let mut was_critical = false;
            if let Ok((mut crit)) = crits.get_mut(attack.attacker) {
                if crit.next_hit_is_critical {
                    damage_dealt = (damage_dealt as f32 * crit.crit_damage_multiplier) as u32;
                    was_critical = true;
                    println!("critical damage!")
                }
                if attack.new_group {
                    crit.counter += 1;
                    println!("crit_counter: {}", crit.counter);
                }
            }
            //focus multiplier
            if let Ok((mut focus)) = focus.get_mut(attack.attacker) {
                if focus.state != FocusState::InActive {
                    damage_dealt *= 2;
                    focus.state = FocusState::Applied
                }
            }

            //apply attack damage
            health.current = health.current.saturating_sub(damage_dealt);
            // attack.damaged.push(DamageInfo {
            //     entity,
            //     tick: time.tick(),
            //     amount: damage_dealt,
            //     crit: was_critical,
            // });

            //insert a DamageFlash to flash for 1 animation frame/8 ticks
            if let Ok(anim_entity) = gfx_query.get_mut(gent.e_gfx) {
                commands.entity(anim_entity).insert(DamageFlash {
                    current_ticks: 0,
                    max_ticks: 8,
                });
            }
            //kill
            if health.current == 0 {
                //heal, if heal passive is active
                if attacker_passives.contains(&Passive::Absorption) {
                    if let Ok((_, mut attacker_health, _, _, _, _, _, _)) =
                        damageable_query.get_mut(attack.attacker)
                    {
                        attacker_health.current += 60;
                    }
                }

                commands.entity(entity).insert(Dead::default());
                //apply more screenshake if an enemies health becomes depleted by this attack
                if is_enemy {
                    rig.trauma = 0.4;
                }
            } else if rig.trauma < 0.3 && is_enemy {
                //apply screenshake on damage to enemy
                rig.trauma = 0.3
            }
            //apply Knockback
            if let Some(pushback) = maybe_pushback {
                commands.entity(entity).insert(Knockback::new(
                    pushback.direction,
                    pushback.strength,
                    16,
                ));
            }

            if attack.new_group {
                attack.new_group = false;
            }
        }

        // Removes entities from collided and damaged_set that are not in newly_collided
        let Attack {
            collided,
            damaged_set,
            ..
        } = attack.as_mut();
        for e in collided.difference(&newly_collided) {
            damaged_set.remove(e);
        }
        *collided = newly_collided;
        // Handle the edge case where newly collided *and* collided might not have damaged
        // set's contents
        if targets_empty {
            damaged_set.clear();
            if !attack.new_group {
                attack.new_group = true;

                if let Ok((mut focus)) = focus.get_mut(attack.attacker) {
                    if focus.state == FocusState::Applied {
                        focus.state = FocusState::InActive;
                        if let Ok((crit)) = crits.get(attack.attacker) {
                            // If the hit *was* critical (since this runs before the
                            // code that resets the critical hit indicator)
                            if crit.next_hit_is_critical {
                                focus.recharge = 10.0;
                            }
                        }
                    }
                }

                //determine if next hit will be critical
                if let Ok((mut crit)) = crits.get_mut(attack.attacker) {
                    crit.next_hit_is_critical = false;
                    let next_hit_counter = crit.counter + 1;
                    if next_hit_counter % 17 == 0 || next_hit_counter % 19 == 0 {
                        crit.next_hit_is_critical = true;
                    //if the crit passive is active
                    } else if let Ok(a_passives) = passives.get(attack.attacker) {
                        if let Some(crit_passive) = a_passives.get(&Passive::CritResolve) {
                            println!("{:?}", crit);
                            if let Ok((_, mut attacker_health, _, _, _, _, _, _)) =
                                damageable_query.get_mut(attack.attacker)
                            {
                                if attacker_health.current as f32
                                    <= attacker_health.max as f32 * 0.2
                                    && (next_hit_counter % 3 == 0 || next_hit_counter % 5 == 0)
                                {
                                    crit.next_hit_is_critical = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        if maybe_projectile.is_some() && !intersections_empty && attack.current_lifetime > 1 {
            // Note: purposefully does not despawn child entities, nor remove the
            // reference, so that child particle systems have the option of lingering
            commands.entity(entity).despawn();
        }
    }
}

//maybe should not modify velocity directly but add knockback, but this makes it behave differently
//in states which dont set velocity every frame
fn knockback(
    mut query: Query<(
        Entity,
        &mut Knockback,
        &mut LinearVelocity,
        Has<Defense>,
    )>,
    mut commands: Commands,
) {
    for (entity, mut knockback, mut velocity, is_defending) in query.iter_mut() {
        knockback.ticks += 1;
        if !is_defending {
            velocity.x += knockback.direction * knockback.strength;
        }
        if knockback.ticks > knockback.max_ticks {
            velocity.x = 0.;
            commands.entity(entity).remove::<Knockback>();
        }
    }
}

fn apply_damage_flash(
    sprite_query: Query<
        Entity,
        (
            With<Sprite>,
            Or<(With<EnemyGfx>, With<PlayerGfx>)>,
            Without<Gent>,
            //Without<DamageFlash>,
        ),
    >,
    gent_query: Query<&Gent>,
    mut damage_events: EventReader<DamageInfo>,
    mut commands: Commands,
) {
    for damage_info in damage_events.read() {
        if let Ok(gent) = gent_query.get(damage_info.entity) {
            if let Ok(entity) = sprite_query.get(gent.e_gfx) {
                commands.entity(entity).insert(DamageFlash {
                    current_ticks: 0,
                    max_ticks: 8,
                });
            }
        }
    }
}

fn damage_flash(mut query: Query<(Entity, &mut Sprite, &mut DamageFlash)>, mut commands: Commands) {
    for (entity, mut sprite, mut damage_flash) in query.iter_mut() {
        sprite.color = Color::rgb(2.5, 2.5, 2.5);

        if damage_flash.current_ticks == damage_flash.max_ticks {
            commands.entity(entity).remove::<DamageFlash>();
            sprite.color = Color::rgb(1., 1., 1.);
        }
        damage_flash.current_ticks += 1;
    }
}

fn attack_tick(mut query: Query<&mut Attack>) {
    for mut attack in query.iter_mut() {
        attack.current_lifetime += 1;
    }
}

fn attack_cleanup(query: Query<(Entity, &Attack)>, mut commands: Commands) {
    for (entity, attack) in query.iter() {
        if attack.current_lifetime >= attack.max_lifetime {
            commands.entity(entity).despawn();
        }
    }
}

/// Resource which tracks total number of enemies killed
/// incremented in despawn_dead()
#[derive(Resource, Debug, Default, Deref, DerefMut)]
pub struct KillCount(pub u32);

/// Allows the entity to apply critical strikes.
/// Crits in TheSeeker are special, and trigger once
/// every 17th and 19th successful hits
#[derive(Component, Default, Debug)]
pub struct Crits {
    next_hit_is_critical: bool,
    /// Counts how many successful hits since the 19th hit
    counter: u32,
    /// Yes
    crit_damage_multiplier: f32,
}

impl Crits {
    pub fn new(multiplier: f32) -> Self {
        Self {
            next_hit_is_critical: false,
            counter: 0,
            crit_damage_multiplier: multiplier,
        }
    }
}

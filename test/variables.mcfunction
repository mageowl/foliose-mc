
# test/my_function.mcfunction
scoreboard players set f0l0 test.helix_local 42
scoreboard players add f0l0 test.helix_local 1
tellraw @a ["", {"text": "[test] ", "color": "#9848fa"}, "my_variable = ", {"score": {"value": "f0l0", "objective": "test.helix_local"}}]
execute store result storage test:hlx_temp a0 short 1 run scoreboard plaers get f0l0 test.helix_local
execute at @s run function test:_t1 with storage test:hlx_temp
execute at @n[tag=test.helix.f0l0] run tp @n[tag=test.helix.f0l0] ~ ~5 ~
tag @n[tag=test.helix.f0l0] remove test.helix.f0l0

# test/_t1.mcfunction
$summon minecraft:creeper ~ ~ ~ {Fuse: $(a0), Tags: ["test.helix.f0l0"]}

from gym.envs.registration import register

register(
    id='optical-tables-v0',
    entry_point='gym_optical_tables.envs:OpticalTablesEnv',
)

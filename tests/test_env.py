import pytest
import numpy as np

import gym
import gym_optical_tables

def test_results():
    ENV_PARAMS = {'initial_spdc': 1, \
                  'num_modes': 4, \
                  'max_l': 2, \
                  'dp_phase': 1, \
                  'coincidence_count': 4, \
                  'trigger_mode': 4, \
                  'episode_length': 12
                 }
    env = gym.make('optical-tables-v0', **ENV_PARAMS)

    # check number of actions
    assert env.num_actions == 30

    # check specific result: Leach interferometer
    actions = [3, 7, 28, 3]

    env.reset()
    for a in actions:
        o, r, done, info = env.step(a)
    
    assert r == 1
    assert (np.array(info["SRVs"]) == np.array([[3,3,2]])).all()

    # check ending
    for i in range(4, 12):
        action = np.random.choice(env.num_actions)
        _, _, done, _ = env.step(action)

    assert done == True

    env.close()

import numpy as np
import sys, os

sys.path.append(os.path.join(os.path.dirname(__file__), '../gym_optical_tables/envs'))

from optical_tables_env import OpticalTablesEnv
ENV_PARAMS = {'initial_spdc': 1, \
              'num_modes': 4, \
              'max_l': 2, \
              'dp_phase': 1, \
              'coincidence_count': 4, \
              'trigger_mode': 4, \
              'episode_length': 12, \
              'kernel_path': '' # possibly need to add r'path/to/WolframKernel.exe'
             }

env = OpticalTablesEnv(**ENV_PARAMS)
print(f"All optical elements: {env.read_toolbox()}")

for i in range(10):
    observation = env.reset()
    done = False
    counter = 0
    while not done:
        counter += 1
        action = np.random.choice(env.num_actions)
        observation, reward, done, info = env.step(action)
        if reward > 0:
            print(f"Current observation: {observation}")
            print(f"Current reward: {reward}")
            print(f"Current SRVs: {info['SRVs']}")
            print()

env.close()
print("done")

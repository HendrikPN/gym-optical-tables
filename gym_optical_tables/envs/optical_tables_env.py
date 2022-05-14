import gym
from gym import spaces

import numpy as np
import importlib.resources as resources
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl, wlexpr

import gym_optical_tables.libraries

class OpticalTablesEnv(gym.Env):
	metadata = {'render.modes': ['human']}

	def __init__(self, **kwargs):
		"""
		OpenAI gym environment for quantum optical experiments.
		Given an initial photonic state in the OAM basis over n+1 modes, the goal
		is to create high-dimensional multipartite entanglement between photons.

		NOTE: By default, this assumes that `WolframLanguageSession()` finds your 
			  WolframKernel automatically. Instead, you might need to explicitly give 
			  the path as kwarg {'kernel_path': r'path/to/WolframKernel.exe'}. 
			  (Note the use of a raw string to avoid escapes.)

		Args:
			**kwargs:
				initial_spdc (int): Max OAM for the initial SPDC state. Default: 1.
				num_modes (int): Number of modes. Default: 4.
				max_l (int): Max OAM shift for holograms. Default: 2.
				dp_phase (int): Phase factor for dove prism calculated as exp(I pi l/n).
								n=x means dove prisms with n=1,...,x are included.
								Default: 1.
				coincidence_count (int): The coincidence count for photons.
										 Default: 4.
				trigger_mode (int): The mode that is measured to trigger entanglement.
									Default: 1.
				episode_length (int): The max length of one episode. Default: 12.
				kernel_path (str): The path to your WolframKernel. Default ''.
		"""
		if 'initial_spdc' in kwargs and type(kwargs['initial_spdc']) is int:
			setattr(self, 'initial_spdc', kwargs['initial_spdc'])
		else:
			setattr(self, 'initial_spdc', 1)
		if 'num_modes' in kwargs and type(kwargs['num_modes']) is int:
			setattr(self, 'num_modes', kwargs['num_modes'])
		else:
			setattr(self, 'num_modes', 4)
		if 'max_l' in kwargs and type(kwargs['max_l']) is int:
			setattr(self, 'max_l', kwargs['max_l'])
		else:
			setattr(self, 'max_l', 2)
		if 'dp_phase' in kwargs and type(kwargs['dp_phase']) is int:
			setattr(self, 'dp_phase', kwargs['dp_phase'])
		else:
			setattr(self, 'dp_phase', 1)
		if 'coincidence_count' in kwargs and type(kwargs['coincidence_count']) is int:
			setattr(self, 'coincidence_count', kwargs['coincidence_count'])
		else:
			setattr(self, 'coincidence_count', 4)
		if 'trigger_mode' in kwargs and type(kwargs['trigger_mode']) is int:
			setattr(self, 'trigger_mode', kwargs['trigger_mode'])
		else:
			setattr(self, 'trigger_mode', 1)
		if 'episode_length' in kwargs and type(kwargs['episode_length']) is int:
			setattr(self, 'episode_length', kwargs['episode_length'])
		else:
			setattr(self, 'episode_length', 12)
		if 'kernel_path' in kwargs and type(kwargs['kernel_path']) is str:
			setattr(self, 'kernel_path', kwargs['kernel_path'])
		else:
			setattr(self, 'kernel_path', '')

		# checking for errors
		if self.trigger_mode > self.num_modes:
			raise ValueError('Your trigger mode does not exist. '
							 +f'{self.trigger_mode} > number of modes.')

		# Start Wolfram language session.
		if len(self.kernel_path) == 0:
			self.session = WolframLanguageSession()
		else:
			self.session = WolframLanguageSession(self.kernel_path)

		# Collect environment parameters
		parameters = wlexpr('{\"MaxL\" -> %i, ' % (self.max_l) + \
							'\"DPphase\" -> %i, '  % (self.dp_phase) + \
							'\"NumberOfModes\" -> %i, '  % (self.num_modes) + \
							'\"InitialSPDC\" -> %i, '  % (self.initial_spdc) + \
							'\"CoincidenceCount\" -> %i, '  % (self.coincidence_count) + \
							'\"TriggerMode\" -> %i, '  % (self.trigger_mode) + \
							'\"EpisodeLength\" -> %i}'  % (self.episode_length))

		# Get drivers in WolframLanguageSession
		driver_file_path = resources.path(gym_optical_tables.libraries, "GymOpticalTables.m")
		self.session.evaluate(
							  wlexpr(f'Get["{driver_file_path.as_posix()}"]')
							  )

		# Start environment within WolframLanguageSession
		self.session.evaluate(
							  wlexpr(f'env = DeviceOpen["GymOpticalTables", {parameters}]')
							 )

		#:class:`spaces.Tuple`: OpenAI gym class defining space of observations.
		encoded_action_space = [spaces.Discrete(5) for i in range(self.episode_length)]
		self.observation_space = spaces.Tuple(encoded_action_space)

		#int: Maximum number of actions (the number of actions in the beginning)
		self.num_actions = len(self.session.evaluate(
										wlexpr('env[\"ActionSpace\"]')
									))

		#:class:`spaces.Discrete`: OpenAI gym class defining space of actions. 
		#                          Actions are just indices.
		self.action_space = spaces.Discrete(self.num_actions)

	def reset(self):
		"""
		Resets the environment to an initial state and returns an initial
		observation.

		Returns:
			observation (np.ndarray): Adjacency matrix as numpy vector.
		"""
		resetExpr = wlexpr('DeviceExecute[env, \"Reset\"]')
		self.reward = 0
		self.done = False
		self.observation = self.session.evaluate(resetExpr)['ObservedState']
		self.info = {'SRVs': []}

		return np.array(self.observation)

	def step(self, action):
		"""
		Run one timestep of the environment's dynamics. When end of
		episode is reached, you are responsible for calling `reset()`
		to reset this environment's state.
		Accepts an action and returns a tuple (observation, reward, done, info).

		The action is chosen among optical elements. 
		A reward of +1 is given every time a coincidence detection leads to a
		high-dimensional multipartite entangled state.
		Additional info is given as to which SRV has been reached (excluding (4,3,3)).

		An episode ends after a fixed number of time steps.

		Args:
			action (int): An action index provided by the agent.
		Returns:
			observation (np.ndarray): Agent's observation of the environment.
			reward (float) : Amount of reward returned after previous action
			done (bool): Whether the episode has ended, 
						 in which case further step() calls will return 
						 undefined results.
			info (dict): Contains auxiliary information about the SRVs.
		"""
		# perform action on Mathematica device
		stepExpr = wlexpr(f'DeviceExecute[env, \"Step\", {action}]')
		output = self.session.evaluate(stepExpr)

		# save for use in next call of `step()` (for reverse)
		self.observation = np.array(output['ObservedState'])
		self.reward = output['Reward']
		self.done = output['Ended']
		srvs = output['Info']['SRVs']
		self.info = {"SRVs": list(srvs)}


		return self.observation, self.reward, self.done, self.info
	
	def render(self, mode='human'):
		"""
		Renders the environment.

		Args:
			mode (str): The mode to render with. Default: 'human'
		"""
		raise NotImplementedError("This environment does not support "+ \
									"rendering yet.")

	def close(self):
		"""
		Performs any necessary cleanup in order to close the environment.
		Here, this means to close the WolframLanguageSession.
		"""
		self.session.terminate()

	# ----------------- extra methods -----------------------------------------

	def encode_actions(self, actions):
		"""
		Given an set of actions, returns an encoded set of actions.

		Actions are encoded as follows:
		+ Beam splitter on modes a and b: [1, a, b, 0, 0]
		+ Mirror on mode a: [2, a, 0, 0, 0]
		+ Hologram on mode a shifting by l: [3, a, 0, l, 0]
		+ Dove prism on mode a with angle e^(i pi l/n): [4, a, 0, 0, n]

		TODO: Do not use Mathematica to encode actions.

		Args:
			actions (list): The list of actions to be encoded.

		Returns:
			encoded_a (np.ndarray): The encoded actions.
		"""
		action_str = str(actions)[1:][:-1]
		encoded_a = self.session.evaluate(
										  wlexpr('DeviceExecute[env, \"EncodeActions\",  {{%s}}]' % (action_str))
										  )

		return np.array(encoded_a)

	def read_observation(self):
		"""
		Presents the current observation as human readable string of optical elements.

		Returns:
			o (str): String of optical elements describing the current observation.
		"""
		o = self.session.evaluate(
								  wlexpr('ToString[DeviceExecute[env, \"DecodeObservation\", {DeviceRead[env][\"ObservedState\"]}]]')
								  )
		return o

	def read_toolbox(self):
		"""
		Presents the toolbox of all actions as human readable strinf of optical elements.

		Returns:
			toolbox (str): String of optical elements describing all actions.
		"""

		toolbox = self.session.evaluate(
										wlexpr('env[\"Toolbox\"]')
										)

		return toolbox

	def read_actions(self, actions):
		"""
		Presents a set of actions as human readable strinf of optical elements.

		Args:
			actions (list): The list of actions to be encoded.

		Returns:
			elements (str): String of optical elements describing all actions.
		"""

		toolbox = read_toolbox()
		elements = []

		for a in actions:
			elements.append(toolbox[a])

		return elements


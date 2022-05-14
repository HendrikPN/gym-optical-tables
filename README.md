# Quantum Network Routing Environment for Reinforcement Learning

An *OpenAI* `gym` environment where an reinforcement learning (RL) agent acts on 
an optical table setup to create high-dimensional multipartite entanglement in 
the OAM basis of photons.

This environment adheres to the 
[OpenAI gym standard](https://github.com/openai/gym/blob/master/docs/creating-environments.md) 
for RL environments.

## Environment Description

The environment is an optical experiment where an agent can place optical elements
such as a beam spliters, dove prism, mirrors and holograms.

The agents goal is to find states with high-dimensional multipartite entanglement.
This entanglement is characterized by a [Schmidt-rank vector](https://link.aps.org/doi/10.1103/PhysRevLett.110.030501)
(a,b,c) where at least 3 numbers are greater than 1. The agents receive a reward 
of 1 whenever such as state is found. (N.B. We exclude (4,3,3) states as they are too easy to create.)

## Get Started

Either install the package as described below, or just use the environment
locally as exemplified in `/examples/random_game.py`. 
Importantly, note the [requirements](#requirements) below.

If you choose to install the package, you may import the environment with 
default settings as follows,

```
import gym
import gym_optical_tables

env = gym.make('optical-tables-v0')
```

However, this assumes that `WolframLanguageSession()` finds your 
WolframKernel automatically. Instead, you might need to explicitly give 
the path as kwarg 

```
import gym
import gym_optical_tables

ENV_PARAMS = {'kernel_path': r'path/to/WolframKernel.exe'}. 
env = gym.make('optical-tables-v0', **ENV_PARAMS)
```

(Note the use of a raw string to avoid escapes.)

## Installation

You can get and install the package via github and pip:

```
git clone -b master https://github.com/HendrikPN/gym-optical-tables.git
cd gym-optical-tables
pip install --user -e .
```

## Requirements

This environment relies on Mathematica 13 as backend. You need a WolframKernel to run this environment.
The requirements are as follows (version numbers are not exclusive but are the ones that this has been tested on):

+ python 3.10.4
+ numpy 1.22.3
+ wolframclient 1.1.7
+ gym 0.23.1

## Mathematica vs. Python

This environment is a python wrapper for a Mathematica environment. If you would like to use the 
Mathematica library directly use `GymOpticalTables.m` (which depends on `OpticalElements.m`) as Mathematica package.
You find these files in `./gym_optical_tables/libraries/`.
Then, you can use `GymOpticalTables.m` in the same way as (`SimulatedCartPole`)[https://reference.wolfram.com/language/ref/device/SimulatedCartPole.html].

## To-Do

+ Handle exceptions (e.g., no Mathematica kernel,...)

## What's New

+ 2022-05-14 Initial Commit. Hello world :)

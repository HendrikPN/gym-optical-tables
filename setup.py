from setuptools import setup, find_packages

setup(name='gym_optical_tables',
      version='0.0.1',
      install_requires=['gym', 'numpy', 'wolframclient'],
      packages=find_packages(exclude=['tests']),
      package_data={'': ['gym_optical_tables/libraries/*']},
      include_package_data=True
)

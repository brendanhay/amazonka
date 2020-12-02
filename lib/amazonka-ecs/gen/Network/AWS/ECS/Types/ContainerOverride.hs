{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerOverride where

import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The overrides that should be sent to a container. An empty container override can be passed in. An example of an empty container override would be @{"containerOverrides": [ ] }@ . If a non-empty container override is specified, the @name@ parameter must be included.
--
--
--
-- /See:/ 'containerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { _coCommand ::
      !(Maybe [Text]),
    _coEnvironment :: !(Maybe [KeyValuePair]),
    _coEnvironmentFiles :: !(Maybe [EnvironmentFile]),
    _coResourceRequirements ::
      !(Maybe [ResourceRequirement]),
    _coMemory :: !(Maybe Int),
    _coName :: !(Maybe Text),
    _coCpu :: !(Maybe Int),
    _coMemoryReservation :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coEnvironmentFiles' - A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
--
-- * 'coResourceRequirements' - The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
--
-- * 'coMemory' - The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
--
-- * 'coName' - The name of the container that receives the override. This parameter is required if any override is specified.
--
-- * 'coCpu' - The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- * 'coMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
containerOverride ::
  ContainerOverride
containerOverride =
  ContainerOverride'
    { _coCommand = Nothing,
      _coEnvironment = Nothing,
      _coEnvironmentFiles = Nothing,
      _coResourceRequirements = Nothing,
      _coMemory = Nothing,
      _coName = Nothing,
      _coCpu = Nothing,
      _coMemoryReservation = Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\s a -> s {_coCommand = a}) . _Default . _Coerce

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
coEnvironment :: Lens' ContainerOverride [KeyValuePair]
coEnvironment = lens _coEnvironment (\s a -> s {_coEnvironment = a}) . _Default . _Coerce

-- | A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
coEnvironmentFiles :: Lens' ContainerOverride [EnvironmentFile]
coEnvironmentFiles = lens _coEnvironmentFiles (\s a -> s {_coEnvironmentFiles = a}) . _Default . _Coerce

-- | The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
coResourceRequirements :: Lens' ContainerOverride [ResourceRequirement]
coResourceRequirements = lens _coResourceRequirements (\s a -> s {_coResourceRequirements = a}) . _Default . _Coerce

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
coMemory :: Lens' ContainerOverride (Maybe Int)
coMemory = lens _coMemory (\s a -> s {_coMemory = a})

-- | The name of the container that receives the override. This parameter is required if any override is specified.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\s a -> s {_coName = a})

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
coCpu :: Lens' ContainerOverride (Maybe Int)
coCpu = lens _coCpu (\s a -> s {_coCpu = a})

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
coMemoryReservation :: Lens' ContainerOverride (Maybe Int)
coMemoryReservation = lens _coMemoryReservation (\s a -> s {_coMemoryReservation = a})

instance FromJSON ContainerOverride where
  parseJSON =
    withObject
      "ContainerOverride"
      ( \x ->
          ContainerOverride'
            <$> (x .:? "command" .!= mempty)
            <*> (x .:? "environment" .!= mempty)
            <*> (x .:? "environmentFiles" .!= mempty)
            <*> (x .:? "resourceRequirements" .!= mempty)
            <*> (x .:? "memory")
            <*> (x .:? "name")
            <*> (x .:? "cpu")
            <*> (x .:? "memoryReservation")
      )

instance Hashable ContainerOverride

instance NFData ContainerOverride

instance ToJSON ContainerOverride where
  toJSON ContainerOverride' {..} =
    object
      ( catMaybes
          [ ("command" .=) <$> _coCommand,
            ("environment" .=) <$> _coEnvironment,
            ("environmentFiles" .=) <$> _coEnvironmentFiles,
            ("resourceRequirements" .=) <$> _coResourceRequirements,
            ("memory" .=) <$> _coMemory,
            ("name" .=) <$> _coName,
            ("cpu" .=) <$> _coCpu,
            ("memoryReservation" .=) <$> _coMemoryReservation
          ]
      )

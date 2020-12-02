{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DockerVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DockerVolumeConfiguration where

import Network.AWS.ECS.Types.Scope
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
--
--
--
-- /See:/ 'dockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { _dvcDriverOpts ::
      !(Maybe (Map Text (Text))),
    _dvcDriver :: !(Maybe Text),
    _dvcScope :: !(Maybe Scope),
    _dvcLabels ::
      !(Maybe (Map Text (Text))),
    _dvcAutoprovision :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DockerVolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcDriverOpts' - A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- * 'dvcDriver' - The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- * 'dvcScope' - The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
--
-- * 'dvcLabels' - Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- * 'dvcAutoprovision' - If this value is @true@ , the Docker volume is created if it does not already exist.
dockerVolumeConfiguration ::
  DockerVolumeConfiguration
dockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { _dvcDriverOpts = Nothing,
      _dvcDriver = Nothing,
      _dvcScope = Nothing,
      _dvcLabels = Nothing,
      _dvcAutoprovision = Nothing
    }

-- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
dvcDriverOpts :: Lens' DockerVolumeConfiguration (HashMap Text (Text))
dvcDriverOpts = lens _dvcDriverOpts (\s a -> s {_dvcDriverOpts = a}) . _Default . _Map

-- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
dvcDriver :: Lens' DockerVolumeConfiguration (Maybe Text)
dvcDriver = lens _dvcDriver (\s a -> s {_dvcDriver = a})

-- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
dvcScope :: Lens' DockerVolumeConfiguration (Maybe Scope)
dvcScope = lens _dvcScope (\s a -> s {_dvcScope = a})

-- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
dvcLabels :: Lens' DockerVolumeConfiguration (HashMap Text (Text))
dvcLabels = lens _dvcLabels (\s a -> s {_dvcLabels = a}) . _Default . _Map

-- | If this value is @true@ , the Docker volume is created if it does not already exist.
dvcAutoprovision :: Lens' DockerVolumeConfiguration (Maybe Bool)
dvcAutoprovision = lens _dvcAutoprovision (\s a -> s {_dvcAutoprovision = a})

instance FromJSON DockerVolumeConfiguration where
  parseJSON =
    withObject
      "DockerVolumeConfiguration"
      ( \x ->
          DockerVolumeConfiguration'
            <$> (x .:? "driverOpts" .!= mempty)
            <*> (x .:? "driver")
            <*> (x .:? "scope")
            <*> (x .:? "labels" .!= mempty)
            <*> (x .:? "autoprovision")
      )

instance Hashable DockerVolumeConfiguration

instance NFData DockerVolumeConfiguration

instance ToJSON DockerVolumeConfiguration where
  toJSON DockerVolumeConfiguration' {..} =
    object
      ( catMaybes
          [ ("driverOpts" .=) <$> _dvcDriverOpts,
            ("driver" .=) <$> _dvcDriver,
            ("scope" .=) <$> _dvcScope,
            ("labels" .=) <$> _dvcLabels,
            ("autoprovision" .=) <$> _dvcAutoprovision
          ]
      )

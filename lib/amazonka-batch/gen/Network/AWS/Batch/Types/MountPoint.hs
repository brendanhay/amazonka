{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.MountPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.MountPoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on a Docker volume mount point that is used in a job's container properties. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the Docker Remote API and the @--volume@ option to docker run.
--
--
--
-- /See:/ 'mountPoint' smart constructor.
data MountPoint = MountPoint'
  { _mpContainerPath :: !(Maybe Text),
    _mpSourceVolume :: !(Maybe Text),
    _mpReadOnly :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MountPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpContainerPath' - The path on the container at which to mount the host volume.
--
-- * 'mpSourceVolume' - The name of the volume to mount.
--
-- * 'mpReadOnly' - If this value is @true@ , the container has read-only access to the volume; otherwise, the container can write to the volume. The default value is @false@ .
mountPoint ::
  MountPoint
mountPoint =
  MountPoint'
    { _mpContainerPath = Nothing,
      _mpSourceVolume = Nothing,
      _mpReadOnly = Nothing
    }

-- | The path on the container at which to mount the host volume.
mpContainerPath :: Lens' MountPoint (Maybe Text)
mpContainerPath = lens _mpContainerPath (\s a -> s {_mpContainerPath = a})

-- | The name of the volume to mount.
mpSourceVolume :: Lens' MountPoint (Maybe Text)
mpSourceVolume = lens _mpSourceVolume (\s a -> s {_mpSourceVolume = a})

-- | If this value is @true@ , the container has read-only access to the volume; otherwise, the container can write to the volume. The default value is @false@ .
mpReadOnly :: Lens' MountPoint (Maybe Bool)
mpReadOnly = lens _mpReadOnly (\s a -> s {_mpReadOnly = a})

instance FromJSON MountPoint where
  parseJSON =
    withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            <$> (x .:? "containerPath")
            <*> (x .:? "sourceVolume")
            <*> (x .:? "readOnly")
      )

instance Hashable MountPoint

instance NFData MountPoint

instance ToJSON MountPoint where
  toJSON MountPoint' {..} =
    object
      ( catMaybes
          [ ("containerPath" .=) <$> _mpContainerPath,
            ("sourceVolume" .=) <$> _mpSourceVolume,
            ("readOnly" .=) <$> _mpReadOnly
          ]
      )

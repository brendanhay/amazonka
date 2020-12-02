{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HostVolumeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostVolumeProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on a container instance bind mount host volume.
--
--
--
-- /See:/ 'hostVolumeProperties' smart constructor.
newtype HostVolumeProperties = HostVolumeProperties'
  { _hvpSourcePath ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostVolumeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hvpSourcePath' - When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported. If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
hostVolumeProperties ::
  HostVolumeProperties
hostVolumeProperties =
  HostVolumeProperties' {_hvpSourcePath = Nothing}

-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported. If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
hvpSourcePath :: Lens' HostVolumeProperties (Maybe Text)
hvpSourcePath = lens _hvpSourcePath (\s a -> s {_hvpSourcePath = a})

instance FromJSON HostVolumeProperties where
  parseJSON =
    withObject
      "HostVolumeProperties"
      (\x -> HostVolumeProperties' <$> (x .:? "sourcePath"))

instance Hashable HostVolumeProperties

instance NFData HostVolumeProperties

instance ToJSON HostVolumeProperties where
  toJSON HostVolumeProperties' {..} =
    object (catMaybes [("sourcePath" .=) <$> _hvpSourcePath])

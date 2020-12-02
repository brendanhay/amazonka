{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Volume where

import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A data volume used in a task definition. For tasks that use the Amazon Elastic File System (Amazon EFS), specify an @efsVolumeConfiguration@ . For Windows tasks that use Amazon FSx for Windows File Server file system, specify a @fsxWindowsFileServerVolumeConfiguration@ . For tasks that use a Docker volume, specify a @DockerVolumeConfiguration@ . For tasks that use a bind mount host volume, specify a @host@ and optional @sourcePath@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using Data Volumes in Tasks> .
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vDockerVolumeConfiguration ::
      !(Maybe DockerVolumeConfiguration),
    _vFsxWindowsFileServerVolumeConfiguration ::
      !(Maybe FSxWindowsFileServerVolumeConfiguration),
    _vName :: !(Maybe Text),
    _vEfsVolumeConfiguration :: !(Maybe EFSVolumeConfiguration),
    _vHost :: !(Maybe HostVolumeProperties)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vDockerVolumeConfiguration' - This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
--
-- * 'vFsxWindowsFileServerVolumeConfiguration' - This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
--
-- * 'vName' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- * 'vEfsVolumeConfiguration' - This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
--
-- * 'vHost' - This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running. Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
volume ::
  Volume
volume =
  Volume'
    { _vDockerVolumeConfiguration = Nothing,
      _vFsxWindowsFileServerVolumeConfiguration = Nothing,
      _vName = Nothing,
      _vEfsVolumeConfiguration = Nothing,
      _vHost = Nothing
    }

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
vDockerVolumeConfiguration :: Lens' Volume (Maybe DockerVolumeConfiguration)
vDockerVolumeConfiguration = lens _vDockerVolumeConfiguration (\s a -> s {_vDockerVolumeConfiguration = a})

-- | This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
vFsxWindowsFileServerVolumeConfiguration :: Lens' Volume (Maybe FSxWindowsFileServerVolumeConfiguration)
vFsxWindowsFileServerVolumeConfiguration = lens _vFsxWindowsFileServerVolumeConfiguration (\s a -> s {_vFsxWindowsFileServerVolumeConfiguration = a})

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\s a -> s {_vName = a})

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
vEfsVolumeConfiguration :: Lens' Volume (Maybe EFSVolumeConfiguration)
vEfsVolumeConfiguration = lens _vEfsVolumeConfiguration (\s a -> s {_vEfsVolumeConfiguration = a})

-- | This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running. Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
vHost :: Lens' Volume (Maybe HostVolumeProperties)
vHost = lens _vHost (\s a -> s {_vHost = a})

instance FromJSON Volume where
  parseJSON =
    withObject
      "Volume"
      ( \x ->
          Volume'
            <$> (x .:? "dockerVolumeConfiguration")
            <*> (x .:? "fsxWindowsFileServerVolumeConfiguration")
            <*> (x .:? "name")
            <*> (x .:? "efsVolumeConfiguration")
            <*> (x .:? "host")
      )

instance Hashable Volume

instance NFData Volume

instance ToJSON Volume where
  toJSON Volume' {..} =
    object
      ( catMaybes
          [ ("dockerVolumeConfiguration" .=) <$> _vDockerVolumeConfiguration,
            ("fsxWindowsFileServerVolumeConfiguration" .=)
              <$> _vFsxWindowsFileServerVolumeConfiguration,
            ("name" .=) <$> _vName,
            ("efsVolumeConfiguration" .=) <$> _vEfsVolumeConfiguration,
            ("host" .=) <$> _vHost
          ]
      )

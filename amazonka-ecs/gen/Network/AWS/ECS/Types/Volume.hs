{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Volume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Volume where

import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.HostVolumeProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A data volume used in a task definition. For tasks that use the Amazon
-- Elastic File System (Amazon EFS), specify an @efsVolumeConfiguration@.
-- For Windows tasks that use Amazon FSx for Windows File Server file
-- system, specify a @fsxWindowsFileServerVolumeConfiguration@. For tasks
-- that use a Docker volume, specify a @DockerVolumeConfiguration@. For
-- tasks that use a bind mount host volume, specify a @host@ and optional
-- @sourcePath@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using Data Volumes in Tasks>.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | The name of the volume. Up to 255 letters (uppercase and lowercase),
    -- numbers, and hyphens are allowed. This name is referenced in the
    -- @sourceVolume@ parameter of container definition @mountPoints@.
    name :: Prelude.Maybe Prelude.Text,
    -- | This parameter is specified when you are using Docker volumes. Docker
    -- volumes are only supported when you are using the EC2 launch type.
    -- Windows containers only support the use of the @local@ driver. To use
    -- bind mounts, specify the @host@ parameter instead.
    dockerVolumeConfiguration :: Prelude.Maybe DockerVolumeConfiguration,
    -- | This parameter is specified when you are using Amazon FSx for Windows
    -- File Server file system for task storage.
    fsxWindowsFileServerVolumeConfiguration :: Prelude.Maybe FSxWindowsFileServerVolumeConfiguration,
    -- | This parameter is specified when you are using an Amazon Elastic File
    -- System file system for task storage.
    efsVolumeConfiguration :: Prelude.Maybe EFSVolumeConfiguration,
    -- | This parameter is specified when you are using bind mount host volumes.
    -- The contents of the @host@ parameter determine whether your bind mount
    -- host volume persists on the host container instance and where it is
    -- stored. If the @host@ parameter is empty, then the Docker daemon assigns
    -- a host path for your data volume. However, the data is not guaranteed to
    -- persist after the containers associated with it stop running.
    --
    -- Windows containers can mount whole directories on the same drive as
    -- @$env:ProgramData@. Windows containers cannot mount directories on a
    -- different drive, and mount point cannot be across drives. For example,
    -- you can mount @C:\\my\\path:C:\\my\\path@ and @D:\\:D:\\@, but not
    -- @D:\\my\\path:C:\\my\\path@ or @D:\\:C:\\my\\path@.
    host :: Prelude.Maybe HostVolumeProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'volume_name' - The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. This name is referenced in the
-- @sourceVolume@ parameter of container definition @mountPoints@.
--
-- 'dockerVolumeConfiguration', 'volume_dockerVolumeConfiguration' - This parameter is specified when you are using Docker volumes. Docker
-- volumes are only supported when you are using the EC2 launch type.
-- Windows containers only support the use of the @local@ driver. To use
-- bind mounts, specify the @host@ parameter instead.
--
-- 'fsxWindowsFileServerVolumeConfiguration', 'volume_fsxWindowsFileServerVolumeConfiguration' - This parameter is specified when you are using Amazon FSx for Windows
-- File Server file system for task storage.
--
-- 'efsVolumeConfiguration', 'volume_efsVolumeConfiguration' - This parameter is specified when you are using an Amazon Elastic File
-- System file system for task storage.
--
-- 'host', 'volume_host' - This parameter is specified when you are using bind mount host volumes.
-- The contents of the @host@ parameter determine whether your bind mount
-- host volume persists on the host container instance and where it is
-- stored. If the @host@ parameter is empty, then the Docker daemon assigns
-- a host path for your data volume. However, the data is not guaranteed to
-- persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as
-- @$env:ProgramData@. Windows containers cannot mount directories on a
-- different drive, and mount point cannot be across drives. For example,
-- you can mount @C:\\my\\path:C:\\my\\path@ and @D:\\:D:\\@, but not
-- @D:\\my\\path:C:\\my\\path@ or @D:\\:C:\\my\\path@.
newVolume ::
  Volume
newVolume =
  Volume'
    { name = Prelude.Nothing,
      dockerVolumeConfiguration = Prelude.Nothing,
      fsxWindowsFileServerVolumeConfiguration =
        Prelude.Nothing,
      efsVolumeConfiguration = Prelude.Nothing,
      host = Prelude.Nothing
    }

-- | The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. This name is referenced in the
-- @sourceVolume@ parameter of container definition @mountPoints@.
volume_name :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | This parameter is specified when you are using Docker volumes. Docker
-- volumes are only supported when you are using the EC2 launch type.
-- Windows containers only support the use of the @local@ driver. To use
-- bind mounts, specify the @host@ parameter instead.
volume_dockerVolumeConfiguration :: Lens.Lens' Volume (Prelude.Maybe DockerVolumeConfiguration)
volume_dockerVolumeConfiguration = Lens.lens (\Volume' {dockerVolumeConfiguration} -> dockerVolumeConfiguration) (\s@Volume' {} a -> s {dockerVolumeConfiguration = a} :: Volume)

-- | This parameter is specified when you are using Amazon FSx for Windows
-- File Server file system for task storage.
volume_fsxWindowsFileServerVolumeConfiguration :: Lens.Lens' Volume (Prelude.Maybe FSxWindowsFileServerVolumeConfiguration)
volume_fsxWindowsFileServerVolumeConfiguration = Lens.lens (\Volume' {fsxWindowsFileServerVolumeConfiguration} -> fsxWindowsFileServerVolumeConfiguration) (\s@Volume' {} a -> s {fsxWindowsFileServerVolumeConfiguration = a} :: Volume)

-- | This parameter is specified when you are using an Amazon Elastic File
-- System file system for task storage.
volume_efsVolumeConfiguration :: Lens.Lens' Volume (Prelude.Maybe EFSVolumeConfiguration)
volume_efsVolumeConfiguration = Lens.lens (\Volume' {efsVolumeConfiguration} -> efsVolumeConfiguration) (\s@Volume' {} a -> s {efsVolumeConfiguration = a} :: Volume)

-- | This parameter is specified when you are using bind mount host volumes.
-- The contents of the @host@ parameter determine whether your bind mount
-- host volume persists on the host container instance and where it is
-- stored. If the @host@ parameter is empty, then the Docker daemon assigns
-- a host path for your data volume. However, the data is not guaranteed to
-- persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as
-- @$env:ProgramData@. Windows containers cannot mount directories on a
-- different drive, and mount point cannot be across drives. For example,
-- you can mount @C:\\my\\path:C:\\my\\path@ and @D:\\:D:\\@, but not
-- @D:\\my\\path:C:\\my\\path@ or @D:\\:C:\\my\\path@.
volume_host :: Lens.Lens' Volume (Prelude.Maybe HostVolumeProperties)
volume_host = Lens.lens (\Volume' {host} -> host) (\s@Volume' {} a -> s {host = a} :: Volume)

instance Prelude.FromJSON Volume where
  parseJSON =
    Prelude.withObject
      "Volume"
      ( \x ->
          Volume'
            Prelude.<$> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "dockerVolumeConfiguration")
            Prelude.<*> ( x
                            Prelude..:? "fsxWindowsFileServerVolumeConfiguration"
                        )
            Prelude.<*> (x Prelude..:? "efsVolumeConfiguration")
            Prelude.<*> (x Prelude..:? "host")
      )

instance Prelude.Hashable Volume

instance Prelude.NFData Volume

instance Prelude.ToJSON Volume where
  toJSON Volume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("name" Prelude..=) Prelude.<$> name,
            ("dockerVolumeConfiguration" Prelude..=)
              Prelude.<$> dockerVolumeConfiguration,
            ( "fsxWindowsFileServerVolumeConfiguration"
                Prelude..=
            )
              Prelude.<$> fsxWindowsFileServerVolumeConfiguration,
            ("efsVolumeConfiguration" Prelude..=)
              Prelude.<$> efsVolumeConfiguration,
            ("host" Prelude..=) Prelude.<$> host
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Volume
  ( Volume (..),

    -- * Smart constructor
    mkVolume,

    -- * Lenses
    vDockerVolumeConfiguration,
    vFsxWindowsFileServerVolumeConfiguration,
    vName,
    vEfsVolumeConfiguration,
    vHost,
  )
where

import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.HostVolumeProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A data volume used in a task definition. For tasks that use the Amazon Elastic File System (Amazon EFS), specify an @efsVolumeConfiguration@ . For Windows tasks that use Amazon FSx for Windows File Server file system, specify a @fsxWindowsFileServerVolumeConfiguration@ . For tasks that use a Docker volume, specify a @DockerVolumeConfiguration@ . For tasks that use a bind mount host volume, specify a @host@ and optional @sourcePath@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using Data Volumes in Tasks> .
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { -- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
    dockerVolumeConfiguration :: Lude.Maybe DockerVolumeConfiguration,
    -- | This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
    fsxWindowsFileServerVolumeConfiguration :: Lude.Maybe FSxWindowsFileServerVolumeConfiguration,
    -- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
    name :: Lude.Maybe Lude.Text,
    -- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
    efsVolumeConfiguration :: Lude.Maybe EFSVolumeConfiguration,
    -- | This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
    --
    -- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
    host :: Lude.Maybe HostVolumeProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- * 'dockerVolumeConfiguration' - This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
-- * 'fsxWindowsFileServerVolumeConfiguration' - This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
-- * 'name' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
-- * 'efsVolumeConfiguration' - This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
-- * 'host' - This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
mkVolume ::
  Volume
mkVolume =
  Volume'
    { dockerVolumeConfiguration = Lude.Nothing,
      fsxWindowsFileServerVolumeConfiguration = Lude.Nothing,
      name = Lude.Nothing,
      efsVolumeConfiguration = Lude.Nothing,
      host = Lude.Nothing
    }

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
--
-- /Note:/ Consider using 'dockerVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDockerVolumeConfiguration :: Lens.Lens' Volume (Lude.Maybe DockerVolumeConfiguration)
vDockerVolumeConfiguration = Lens.lens (dockerVolumeConfiguration :: Volume -> Lude.Maybe DockerVolumeConfiguration) (\s a -> s {dockerVolumeConfiguration = a} :: Volume)
{-# DEPRECATED vDockerVolumeConfiguration "Use generic-lens or generic-optics with 'dockerVolumeConfiguration' instead." #-}

-- | This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
--
-- /Note:/ Consider using 'fsxWindowsFileServerVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFsxWindowsFileServerVolumeConfiguration :: Lens.Lens' Volume (Lude.Maybe FSxWindowsFileServerVolumeConfiguration)
vFsxWindowsFileServerVolumeConfiguration = Lens.lens (fsxWindowsFileServerVolumeConfiguration :: Volume -> Lude.Maybe FSxWindowsFileServerVolumeConfiguration) (\s a -> s {fsxWindowsFileServerVolumeConfiguration = a} :: Volume)
{-# DEPRECATED vFsxWindowsFileServerVolumeConfiguration "Use generic-lens or generic-optics with 'fsxWindowsFileServerVolumeConfiguration' instead." #-}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vName = Lens.lens (name :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Volume)
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
--
-- /Note:/ Consider using 'efsVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEfsVolumeConfiguration :: Lens.Lens' Volume (Lude.Maybe EFSVolumeConfiguration)
vEfsVolumeConfiguration = Lens.lens (efsVolumeConfiguration :: Volume -> Lude.Maybe EFSVolumeConfiguration) (\s a -> s {efsVolumeConfiguration = a} :: Volume)
{-# DEPRECATED vEfsVolumeConfiguration "Use generic-lens or generic-optics with 'efsVolumeConfiguration' instead." #-}

-- | This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vHost :: Lens.Lens' Volume (Lude.Maybe HostVolumeProperties)
vHost = Lens.lens (host :: Volume -> Lude.Maybe HostVolumeProperties) (\s a -> s {host = a} :: Volume)
{-# DEPRECATED vHost "Use generic-lens or generic-optics with 'host' instead." #-}

instance Lude.FromJSON Volume where
  parseJSON =
    Lude.withObject
      "Volume"
      ( \x ->
          Volume'
            Lude.<$> (x Lude..:? "dockerVolumeConfiguration")
            Lude.<*> (x Lude..:? "fsxWindowsFileServerVolumeConfiguration")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "efsVolumeConfiguration")
            Lude.<*> (x Lude..:? "host")
      )

instance Lude.ToJSON Volume where
  toJSON Volume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dockerVolumeConfiguration" Lude..=)
              Lude.<$> dockerVolumeConfiguration,
            ("fsxWindowsFileServerVolumeConfiguration" Lude..=)
              Lude.<$> fsxWindowsFileServerVolumeConfiguration,
            ("name" Lude..=) Lude.<$> name,
            ("efsVolumeConfiguration" Lude..=) Lude.<$> efsVolumeConfiguration,
            ("host" Lude..=) Lude.<$> host
          ]
      )

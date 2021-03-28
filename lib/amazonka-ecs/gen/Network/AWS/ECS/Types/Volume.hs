{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Volume
  ( Volume (..)
  -- * Smart constructor
  , mkVolume
  -- * Lenses
  , vDockerVolumeConfiguration
  , vEfsVolumeConfiguration
  , vFsxWindowsFileServerVolumeConfiguration
  , vHost
  , vName
  ) where

import qualified Network.AWS.ECS.Types.DockerVolumeConfiguration as Types
import qualified Network.AWS.ECS.Types.EFSVolumeConfiguration as Types
import qualified Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration as Types
import qualified Network.AWS.ECS.Types.HostVolumeProperties as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A data volume used in a task definition. For tasks that use the Amazon Elastic File System (Amazon EFS), specify an @efsVolumeConfiguration@ . For Windows tasks that use Amazon FSx for Windows File Server file system, specify a @fsxWindowsFileServerVolumeConfiguration@ . For tasks that use a Docker volume, specify a @DockerVolumeConfiguration@ . For tasks that use a bind mount host volume, specify a @host@ and optional @sourcePath@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using Data Volumes in Tasks> .
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { dockerVolumeConfiguration :: Core.Maybe Types.DockerVolumeConfiguration
    -- ^ This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
  , efsVolumeConfiguration :: Core.Maybe Types.EFSVolumeConfiguration
    -- ^ This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
  , fsxWindowsFileServerVolumeConfiguration :: Core.Maybe Types.FSxWindowsFileServerVolumeConfiguration
    -- ^ This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
  , host :: Core.Maybe Types.HostVolumeProperties
    -- ^ This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
  , name :: Core.Maybe Core.Text
    -- ^ The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Volume' value with any optional fields omitted.
mkVolume
    :: Volume
mkVolume
  = Volume'{dockerVolumeConfiguration = Core.Nothing,
            efsVolumeConfiguration = Core.Nothing,
            fsxWindowsFileServerVolumeConfiguration = Core.Nothing,
            host = Core.Nothing, name = Core.Nothing}

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify the @host@ parameter instead.
--
-- /Note:/ Consider using 'dockerVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDockerVolumeConfiguration :: Lens.Lens' Volume (Core.Maybe Types.DockerVolumeConfiguration)
vDockerVolumeConfiguration = Lens.field @"dockerVolumeConfiguration"
{-# INLINEABLE vDockerVolumeConfiguration #-}
{-# DEPRECATED dockerVolumeConfiguration "Use generic-lens or generic-optics with 'dockerVolumeConfiguration' instead"  #-}

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage.
--
-- /Note:/ Consider using 'efsVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEfsVolumeConfiguration :: Lens.Lens' Volume (Core.Maybe Types.EFSVolumeConfiguration)
vEfsVolumeConfiguration = Lens.field @"efsVolumeConfiguration"
{-# INLINEABLE vEfsVolumeConfiguration #-}
{-# DEPRECATED efsVolumeConfiguration "Use generic-lens or generic-optics with 'efsVolumeConfiguration' instead"  #-}

-- | This parameter is specified when you are using Amazon FSx for Windows File Server file system for task storage.
--
-- /Note:/ Consider using 'fsxWindowsFileServerVolumeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFsxWindowsFileServerVolumeConfiguration :: Lens.Lens' Volume (Core.Maybe Types.FSxWindowsFileServerVolumeConfiguration)
vFsxWindowsFileServerVolumeConfiguration = Lens.field @"fsxWindowsFileServerVolumeConfiguration"
{-# INLINEABLE vFsxWindowsFileServerVolumeConfiguration #-}
{-# DEPRECATED fsxWindowsFileServerVolumeConfiguration "Use generic-lens or generic-optics with 'fsxWindowsFileServerVolumeConfiguration' instead"  #-}

-- | This parameter is specified when you are using bind mount host volumes. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vHost :: Lens.Lens' Volume (Core.Maybe Types.HostVolumeProperties)
vHost = Lens.field @"host"
{-# INLINEABLE vHost #-}
{-# DEPRECATED host "Use generic-lens or generic-optics with 'host' instead"  #-}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Core.Maybe Core.Text)
vName = Lens.field @"name"
{-# INLINEABLE vName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Volume where
        toJSON Volume{..}
          = Core.object
              (Core.catMaybes
                 [("dockerVolumeConfiguration" Core..=) Core.<$>
                    dockerVolumeConfiguration,
                  ("efsVolumeConfiguration" Core..=) Core.<$> efsVolumeConfiguration,
                  ("fsxWindowsFileServerVolumeConfiguration" Core..=) Core.<$>
                    fsxWindowsFileServerVolumeConfiguration,
                  ("host" Core..=) Core.<$> host, ("name" Core..=) Core.<$> name])

instance Core.FromJSON Volume where
        parseJSON
          = Core.withObject "Volume" Core.$
              \ x ->
                Volume' Core.<$>
                  (x Core..:? "dockerVolumeConfiguration") Core.<*>
                    x Core..:? "efsVolumeConfiguration"
                    Core.<*> x Core..:? "fsxWindowsFileServerVolumeConfiguration"
                    Core.<*> x Core..:? "host"
                    Core.<*> x Core..:? "name"

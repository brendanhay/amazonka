{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
  ( FSxWindowsFileServerVolumeConfiguration (..),

    -- * Smart constructor
    mkFSxWindowsFileServerVolumeConfiguration,

    -- * Lenses
    fswfsvcFileSystemId,
    fswfsvcRootDirectory,
    fswfsvcAuthorizationConfig,
  )
where

import qualified Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This parameter is specified when you are using <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html Amazon FSx for Windows File Server> file system for task storage.
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFSxWindowsFileServerVolumeConfiguration' smart constructor.
data FSxWindowsFileServerVolumeConfiguration = FSxWindowsFileServerVolumeConfiguration'
  { -- | The Amazon FSx for Windows File Server file system ID to use.
    fileSystemId :: Types.String,
    -- | The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
    rootDirectory :: Types.String,
    -- | The authorization configuration details for the Amazon FSx for Windows File Server file system.
    authorizationConfig :: Types.FSxWindowsFileServerAuthorizationConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FSxWindowsFileServerVolumeConfiguration' value with any optional fields omitted.
mkFSxWindowsFileServerVolumeConfiguration ::
  -- | 'fileSystemId'
  Types.String ->
  -- | 'rootDirectory'
  Types.String ->
  -- | 'authorizationConfig'
  Types.FSxWindowsFileServerAuthorizationConfig ->
  FSxWindowsFileServerVolumeConfiguration
mkFSxWindowsFileServerVolumeConfiguration
  fileSystemId
  rootDirectory
  authorizationConfig =
    FSxWindowsFileServerVolumeConfiguration'
      { fileSystemId,
        rootDirectory,
        authorizationConfig
      }

-- | The Amazon FSx for Windows File Server file system ID to use.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcFileSystemId :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Types.String
fswfsvcFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED fswfsvcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcRootDirectory :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Types.String
fswfsvcRootDirectory = Lens.field @"rootDirectory"
{-# DEPRECATED fswfsvcRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | The authorization configuration details for the Amazon FSx for Windows File Server file system.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcAuthorizationConfig :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Types.FSxWindowsFileServerAuthorizationConfig
fswfsvcAuthorizationConfig = Lens.field @"authorizationConfig"
{-# DEPRECATED fswfsvcAuthorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead." #-}

instance Core.FromJSON FSxWindowsFileServerVolumeConfiguration where
  toJSON FSxWindowsFileServerVolumeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("fileSystemId" Core..= fileSystemId),
            Core.Just ("rootDirectory" Core..= rootDirectory),
            Core.Just ("authorizationConfig" Core..= authorizationConfig)
          ]
      )

instance Core.FromJSON FSxWindowsFileServerVolumeConfiguration where
  parseJSON =
    Core.withObject "FSxWindowsFileServerVolumeConfiguration" Core.$
      \x ->
        FSxWindowsFileServerVolumeConfiguration'
          Core.<$> (x Core..: "fileSystemId")
          Core.<*> (x Core..: "rootDirectory")
          Core.<*> (x Core..: "authorizationConfig")

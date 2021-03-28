{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.FileSystemConfig
  ( FileSystemConfig (..)
  -- * Smart constructor
  , mkFileSystemConfig
  -- * Lenses
  , fscDefaultGid
  , fscDefaultUid
  , fscMountPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MountPath as Types

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
-- /See:/ 'mkFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { defaultGid :: Core.Maybe Core.Natural
    -- ^ The default POSIX group ID (GID). If not specified, defaults to @100@ .
  , defaultUid :: Core.Maybe Core.Natural
    -- ^ The default POSIX user ID (UID). If not specified, defaults to @1000@ .
  , mountPath :: Core.Maybe Types.MountPath
    -- ^ The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSystemConfig' value with any optional fields omitted.
mkFileSystemConfig
    :: FileSystemConfig
mkFileSystemConfig
  = FileSystemConfig'{defaultGid = Core.Nothing,
                      defaultUid = Core.Nothing, mountPath = Core.Nothing}

-- | The default POSIX group ID (GID). If not specified, defaults to @100@ .
--
-- /Note:/ Consider using 'defaultGid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscDefaultGid :: Lens.Lens' FileSystemConfig (Core.Maybe Core.Natural)
fscDefaultGid = Lens.field @"defaultGid"
{-# INLINEABLE fscDefaultGid #-}
{-# DEPRECATED defaultGid "Use generic-lens or generic-optics with 'defaultGid' instead"  #-}

-- | The default POSIX user ID (UID). If not specified, defaults to @1000@ .
--
-- /Note:/ Consider using 'defaultUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscDefaultUid :: Lens.Lens' FileSystemConfig (Core.Maybe Core.Natural)
fscDefaultUid = Lens.field @"defaultUid"
{-# INLINEABLE fscDefaultUid #-}
{-# DEPRECATED defaultUid "Use generic-lens or generic-optics with 'defaultUid' instead"  #-}

-- | The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
--
-- /Note:/ Consider using 'mountPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscMountPath :: Lens.Lens' FileSystemConfig (Core.Maybe Types.MountPath)
fscMountPath = Lens.field @"mountPath"
{-# INLINEABLE fscMountPath #-}
{-# DEPRECATED mountPath "Use generic-lens or generic-optics with 'mountPath' instead"  #-}

instance Core.FromJSON FileSystemConfig where
        toJSON FileSystemConfig{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultGid" Core..=) Core.<$> defaultGid,
                  ("DefaultUid" Core..=) Core.<$> defaultUid,
                  ("MountPath" Core..=) Core.<$> mountPath])

instance Core.FromJSON FileSystemConfig where
        parseJSON
          = Core.withObject "FileSystemConfig" Core.$
              \ x ->
                FileSystemConfig' Core.<$>
                  (x Core..:? "DefaultGid") Core.<*> x Core..:? "DefaultUid" Core.<*>
                    x Core..:? "MountPath"

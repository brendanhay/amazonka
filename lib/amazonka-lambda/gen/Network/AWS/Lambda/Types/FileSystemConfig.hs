{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.FileSystemConfig
  ( FileSystemConfig (..)
  -- * Smart constructor
  , mkFileSystemConfig
  -- * Lenses
  , fscArn
  , fscLocalMountPath
  ) where

import qualified Network.AWS.Lambda.Types.FileSystemArn as Types
import qualified Network.AWS.Lambda.Types.LocalMountPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the connection between a Lambda function and an Amazon EFS file system.
--
-- /See:/ 'mkFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { arn :: Types.FileSystemArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
  , localMountPath :: Types.LocalMountPath
    -- ^ The path where the function can access the file system, starting with @/mnt/@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSystemConfig' value with any optional fields omitted.
mkFileSystemConfig
    :: Types.FileSystemArn -- ^ 'arn'
    -> Types.LocalMountPath -- ^ 'localMountPath'
    -> FileSystemConfig
mkFileSystemConfig arn localMountPath
  = FileSystemConfig'{arn, localMountPath}

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscArn :: Lens.Lens' FileSystemConfig Types.FileSystemArn
fscArn = Lens.field @"arn"
{-# INLINEABLE fscArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The path where the function can access the file system, starting with @/mnt/@ .
--
-- /Note:/ Consider using 'localMountPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscLocalMountPath :: Lens.Lens' FileSystemConfig Types.LocalMountPath
fscLocalMountPath = Lens.field @"localMountPath"
{-# INLINEABLE fscLocalMountPath #-}
{-# DEPRECATED localMountPath "Use generic-lens or generic-optics with 'localMountPath' instead"  #-}

instance Core.FromJSON FileSystemConfig where
        toJSON FileSystemConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Arn" Core..= arn),
                  Core.Just ("LocalMountPath" Core..= localMountPath)])

instance Core.FromJSON FileSystemConfig where
        parseJSON
          = Core.withObject "FileSystemConfig" Core.$
              \ x ->
                FileSystemConfig' Core.<$>
                  (x Core..: "Arn") Core.<*> x Core..: "LocalMountPath"

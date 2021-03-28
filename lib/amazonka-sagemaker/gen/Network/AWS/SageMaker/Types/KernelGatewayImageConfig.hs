{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.KernelGatewayImageConfig
  ( KernelGatewayImageConfig (..)
  -- * Smart constructor
  , mkKernelGatewayImageConfig
  -- * Lenses
  , kgicKernelSpecs
  , kgicFileSystemConfig
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FileSystemConfig as Types
import qualified Network.AWS.SageMaker.Types.KernelSpec as Types

-- | The configuration for the file system and kernels in a SageMaker image running as a KernelGateway app.
--
-- /See:/ 'mkKernelGatewayImageConfig' smart constructor.
data KernelGatewayImageConfig = KernelGatewayImageConfig'
  { kernelSpecs :: Core.NonEmpty Types.KernelSpec
    -- ^ The specification of the Jupyter kernels in the image.
  , fileSystemConfig :: Core.Maybe Types.FileSystemConfig
    -- ^ The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KernelGatewayImageConfig' value with any optional fields omitted.
mkKernelGatewayImageConfig
    :: Core.NonEmpty Types.KernelSpec -- ^ 'kernelSpecs'
    -> KernelGatewayImageConfig
mkKernelGatewayImageConfig kernelSpecs
  = KernelGatewayImageConfig'{kernelSpecs,
                              fileSystemConfig = Core.Nothing}

-- | The specification of the Jupyter kernels in the image.
--
-- /Note:/ Consider using 'kernelSpecs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgicKernelSpecs :: Lens.Lens' KernelGatewayImageConfig (Core.NonEmpty Types.KernelSpec)
kgicKernelSpecs = Lens.field @"kernelSpecs"
{-# INLINEABLE kgicKernelSpecs #-}
{-# DEPRECATED kernelSpecs "Use generic-lens or generic-optics with 'kernelSpecs' instead"  #-}

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
-- /Note:/ Consider using 'fileSystemConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgicFileSystemConfig :: Lens.Lens' KernelGatewayImageConfig (Core.Maybe Types.FileSystemConfig)
kgicFileSystemConfig = Lens.field @"fileSystemConfig"
{-# INLINEABLE kgicFileSystemConfig #-}
{-# DEPRECATED fileSystemConfig "Use generic-lens or generic-optics with 'fileSystemConfig' instead"  #-}

instance Core.FromJSON KernelGatewayImageConfig where
        toJSON KernelGatewayImageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KernelSpecs" Core..= kernelSpecs),
                  ("FileSystemConfig" Core..=) Core.<$> fileSystemConfig])

instance Core.FromJSON KernelGatewayImageConfig where
        parseJSON
          = Core.withObject "KernelGatewayImageConfig" Core.$
              \ x ->
                KernelGatewayImageConfig' Core.<$>
                  (x Core..: "KernelSpecs") Core.<*> x Core..:? "FileSystemConfig"

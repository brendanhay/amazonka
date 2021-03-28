{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppImageConfigDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppImageConfigDetails
  ( AppImageConfigDetails (..)
  -- * Smart constructor
  , mkAppImageConfigDetails
  -- * Lenses
  , aicdAppImageConfigArn
  , aicdAppImageConfigName
  , aicdCreationTime
  , aicdKernelGatewayImageConfig
  , aicdLastModifiedTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AppImageConfigArn as Types
import qualified Network.AWS.SageMaker.Types.AppImageConfigName as Types
import qualified Network.AWS.SageMaker.Types.KernelGatewayImageConfig as Types

-- | The configuration for running a SageMaker image as a KernelGateway app.
--
-- /See:/ 'mkAppImageConfigDetails' smart constructor.
data AppImageConfigDetails = AppImageConfigDetails'
  { appImageConfigArn :: Core.Maybe Types.AppImageConfigArn
    -- ^ The Amazon Resource Name (ARN) of the AppImageConfig.
  , appImageConfigName :: Core.Maybe Types.AppImageConfigName
    -- ^ The name of the AppImageConfig. Must be unique to your account.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the AppImageConfig was created.
  , kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig
    -- ^ The configuration for the file system and kernels in the SageMaker image.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the AppImageConfig was last modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AppImageConfigDetails' value with any optional fields omitted.
mkAppImageConfigDetails
    :: AppImageConfigDetails
mkAppImageConfigDetails
  = AppImageConfigDetails'{appImageConfigArn = Core.Nothing,
                           appImageConfigName = Core.Nothing, creationTime = Core.Nothing,
                           kernelGatewayImageConfig = Core.Nothing,
                           lastModifiedTime = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigArn :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.AppImageConfigArn)
aicdAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# INLINEABLE aicdAppImageConfigArn #-}
{-# DEPRECATED appImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead"  #-}

-- | The name of the AppImageConfig. Must be unique to your account.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigName :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.AppImageConfigName)
aicdAppImageConfigName = Lens.field @"appImageConfigName"
{-# INLINEABLE aicdAppImageConfigName #-}
{-# DEPRECATED appImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead"  #-}

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdCreationTime :: Lens.Lens' AppImageConfigDetails (Core.Maybe Core.NominalDiffTime)
aicdCreationTime = Lens.field @"creationTime"
{-# INLINEABLE aicdCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The configuration for the file system and kernels in the SageMaker image.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdKernelGatewayImageConfig :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.KernelGatewayImageConfig)
aicdKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# INLINEABLE aicdKernelGatewayImageConfig #-}
{-# DEPRECATED kernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead"  #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdLastModifiedTime :: Lens.Lens' AppImageConfigDetails (Core.Maybe Core.NominalDiffTime)
aicdLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE aicdLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

instance Core.FromJSON AppImageConfigDetails where
        parseJSON
          = Core.withObject "AppImageConfigDetails" Core.$
              \ x ->
                AppImageConfigDetails' Core.<$>
                  (x Core..:? "AppImageConfigArn") Core.<*>
                    x Core..:? "AppImageConfigName"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "KernelGatewayImageConfig"
                    Core.<*> x Core..:? "LastModifiedTime"

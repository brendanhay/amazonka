{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppImageConfigDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppImageConfigDetails
  ( AppImageConfigDetails (..),

    -- * Smart constructor
    mkAppImageConfigDetails,

    -- * Lenses
    aicdAppImageConfigArn,
    aicdAppImageConfigName,
    aicdCreationTime,
    aicdKernelGatewayImageConfig,
    aicdLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AppImageConfigArn as Types
import qualified Network.AWS.SageMaker.Types.AppImageConfigName as Types
import qualified Network.AWS.SageMaker.Types.KernelGatewayImageConfig as Types

-- | The configuration for running a SageMaker image as a KernelGateway app.
--
-- /See:/ 'mkAppImageConfigDetails' smart constructor.
data AppImageConfigDetails = AppImageConfigDetails'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Core.Maybe Types.AppImageConfigArn,
    -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Core.Maybe Types.AppImageConfigName,
    -- | When the AppImageConfig was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The configuration for the file system and kernels in the SageMaker image.
    kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig,
    -- | When the AppImageConfig was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AppImageConfigDetails' value with any optional fields omitted.
mkAppImageConfigDetails ::
  AppImageConfigDetails
mkAppImageConfigDetails =
  AppImageConfigDetails'
    { appImageConfigArn = Core.Nothing,
      appImageConfigName = Core.Nothing,
      creationTime = Core.Nothing,
      kernelGatewayImageConfig = Core.Nothing,
      lastModifiedTime = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigArn :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.AppImageConfigArn)
aicdAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# DEPRECATED aicdAppImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead." #-}

-- | The name of the AppImageConfig. Must be unique to your account.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigName :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.AppImageConfigName)
aicdAppImageConfigName = Lens.field @"appImageConfigName"
{-# DEPRECATED aicdAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdCreationTime :: Lens.Lens' AppImageConfigDetails (Core.Maybe Core.NominalDiffTime)
aicdCreationTime = Lens.field @"creationTime"
{-# DEPRECATED aicdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The configuration for the file system and kernels in the SageMaker image.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdKernelGatewayImageConfig :: Lens.Lens' AppImageConfigDetails (Core.Maybe Types.KernelGatewayImageConfig)
aicdKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# DEPRECATED aicdKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdLastModifiedTime :: Lens.Lens' AppImageConfigDetails (Core.Maybe Core.NominalDiffTime)
aicdLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED aicdLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Core.FromJSON AppImageConfigDetails where
  parseJSON =
    Core.withObject "AppImageConfigDetails" Core.$
      \x ->
        AppImageConfigDetails'
          Core.<$> (x Core..:? "AppImageConfigArn")
          Core.<*> (x Core..:? "AppImageConfigName")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "KernelGatewayImageConfig")
          Core.<*> (x Core..:? "LastModifiedTime")

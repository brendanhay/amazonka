{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelGatewayAppSettings
  ( KernelGatewayAppSettings (..),

    -- * Smart constructor
    mkKernelGatewayAppSettings,

    -- * Lenses
    kgasCustomImages,
    kgasDefaultResourceSpec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CustomImage as Types
import qualified Network.AWS.SageMaker.Types.ResourceSpec as Types

-- | The KernelGateway app settings.
--
-- /See:/ 'mkKernelGatewayAppSettings' smart constructor.
data KernelGatewayAppSettings = KernelGatewayAppSettings'
  { -- | A list of custom SageMaker images that are configured to run as a KernelGateway app.
    customImages :: Core.Maybe [Types.CustomImage],
    -- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the KernelGateway app.
    defaultResourceSpec :: Core.Maybe Types.ResourceSpec
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KernelGatewayAppSettings' value with any optional fields omitted.
mkKernelGatewayAppSettings ::
  KernelGatewayAppSettings
mkKernelGatewayAppSettings =
  KernelGatewayAppSettings'
    { customImages = Core.Nothing,
      defaultResourceSpec = Core.Nothing
    }

-- | A list of custom SageMaker images that are configured to run as a KernelGateway app.
--
-- /Note:/ Consider using 'customImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgasCustomImages :: Lens.Lens' KernelGatewayAppSettings (Core.Maybe [Types.CustomImage])
kgasCustomImages = Lens.field @"customImages"
{-# DEPRECATED kgasCustomImages "Use generic-lens or generic-optics with 'customImages' instead." #-}

-- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the KernelGateway app.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgasDefaultResourceSpec :: Lens.Lens' KernelGatewayAppSettings (Core.Maybe Types.ResourceSpec)
kgasDefaultResourceSpec = Lens.field @"defaultResourceSpec"
{-# DEPRECATED kgasDefaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead." #-}

instance Core.FromJSON KernelGatewayAppSettings where
  toJSON KernelGatewayAppSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomImages" Core..=) Core.<$> customImages,
            ("DefaultResourceSpec" Core..=) Core.<$> defaultResourceSpec
          ]
      )

instance Core.FromJSON KernelGatewayAppSettings where
  parseJSON =
    Core.withObject "KernelGatewayAppSettings" Core.$
      \x ->
        KernelGatewayAppSettings'
          Core.<$> (x Core..:? "CustomImages")
          Core.<*> (x Core..:? "DefaultResourceSpec")

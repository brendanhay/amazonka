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
    kgasDefaultResourceSpec,
    kgasCustomImages,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CustomImage
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The KernelGateway app settings.
--
-- /See:/ 'mkKernelGatewayAppSettings' smart constructor.
data KernelGatewayAppSettings = KernelGatewayAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the KernelGateway app.
    defaultResourceSpec :: Lude.Maybe ResourceSpec,
    -- | A list of custom SageMaker images that are configured to run as a KernelGateway app.
    customImages :: Lude.Maybe [CustomImage]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KernelGatewayAppSettings' with the minimum fields required to make a request.
--
-- * 'defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the KernelGateway app.
-- * 'customImages' - A list of custom SageMaker images that are configured to run as a KernelGateway app.
mkKernelGatewayAppSettings ::
  KernelGatewayAppSettings
mkKernelGatewayAppSettings =
  KernelGatewayAppSettings'
    { defaultResourceSpec = Lude.Nothing,
      customImages = Lude.Nothing
    }

-- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the KernelGateway app.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgasDefaultResourceSpec :: Lens.Lens' KernelGatewayAppSettings (Lude.Maybe ResourceSpec)
kgasDefaultResourceSpec = Lens.lens (defaultResourceSpec :: KernelGatewayAppSettings -> Lude.Maybe ResourceSpec) (\s a -> s {defaultResourceSpec = a} :: KernelGatewayAppSettings)
{-# DEPRECATED kgasDefaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead." #-}

-- | A list of custom SageMaker images that are configured to run as a KernelGateway app.
--
-- /Note:/ Consider using 'customImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgasCustomImages :: Lens.Lens' KernelGatewayAppSettings (Lude.Maybe [CustomImage])
kgasCustomImages = Lens.lens (customImages :: KernelGatewayAppSettings -> Lude.Maybe [CustomImage]) (\s a -> s {customImages = a} :: KernelGatewayAppSettings)
{-# DEPRECATED kgasCustomImages "Use generic-lens or generic-optics with 'customImages' instead." #-}

instance Lude.FromJSON KernelGatewayAppSettings where
  parseJSON =
    Lude.withObject
      "KernelGatewayAppSettings"
      ( \x ->
          KernelGatewayAppSettings'
            Lude.<$> (x Lude..:? "DefaultResourceSpec")
            Lude.<*> (x Lude..:? "CustomImages" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON KernelGatewayAppSettings where
  toJSON KernelGatewayAppSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultResourceSpec" Lude..=) Lude.<$> defaultResourceSpec,
            ("CustomImages" Lude..=) Lude.<$> customImages
          ]
      )

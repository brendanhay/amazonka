{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CustomImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CustomImage
  ( CustomImage (..),

    -- * Smart constructor
    mkCustomImage,

    -- * Lenses
    ciImageName,
    ciAppImageConfigName,
    ciImageVersionNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AppImageConfigName as Types
import qualified Network.AWS.SageMaker.Types.ImageName as Types

-- | A custom SageMaker image. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image> .
--
-- /See:/ 'mkCustomImage' smart constructor.
data CustomImage = CustomImage'
  { -- | The name of the CustomImage. Must be unique to your account.
    imageName :: Types.ImageName,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Types.AppImageConfigName,
    -- | The version number of the CustomImage.
    imageVersionNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomImage' value with any optional fields omitted.
mkCustomImage ::
  -- | 'imageName'
  Types.ImageName ->
  -- | 'appImageConfigName'
  Types.AppImageConfigName ->
  CustomImage
mkCustomImage imageName appImageConfigName =
  CustomImage'
    { imageName,
      appImageConfigName,
      imageVersionNumber = Core.Nothing
    }

-- | The name of the CustomImage. Must be unique to your account.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciImageName :: Lens.Lens' CustomImage Types.ImageName
ciImageName = Lens.field @"imageName"
{-# DEPRECATED ciImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The name of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAppImageConfigName :: Lens.Lens' CustomImage Types.AppImageConfigName
ciAppImageConfigName = Lens.field @"appImageConfigName"
{-# DEPRECATED ciAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | The version number of the CustomImage.
--
-- /Note:/ Consider using 'imageVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciImageVersionNumber :: Lens.Lens' CustomImage (Core.Maybe Core.Natural)
ciImageVersionNumber = Lens.field @"imageVersionNumber"
{-# DEPRECATED ciImageVersionNumber "Use generic-lens or generic-optics with 'imageVersionNumber' instead." #-}

instance Core.FromJSON CustomImage where
  toJSON CustomImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ImageName" Core..= imageName),
            Core.Just ("AppImageConfigName" Core..= appImageConfigName),
            ("ImageVersionNumber" Core..=) Core.<$> imageVersionNumber
          ]
      )

instance Core.FromJSON CustomImage where
  parseJSON =
    Core.withObject "CustomImage" Core.$
      \x ->
        CustomImage'
          Core.<$> (x Core..: "ImageName")
          Core.<*> (x Core..: "AppImageConfigName")
          Core.<*> (x Core..:? "ImageVersionNumber")

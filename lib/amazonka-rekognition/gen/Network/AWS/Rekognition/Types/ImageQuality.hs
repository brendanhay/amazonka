{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ImageQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ImageQuality
  ( ImageQuality (..),

    -- * Smart constructor
    mkImageQuality,

    -- * Lenses
    iqSharpness,
    iqBrightness,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies face image brightness and sharpness.
--
-- /See:/ 'mkImageQuality' smart constructor.
data ImageQuality = ImageQuality'
  { -- | Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
    sharpness :: Lude.Maybe Lude.Double,
    -- | Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
    brightness :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageQuality' with the minimum fields required to make a request.
--
-- * 'sharpness' - Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
-- * 'brightness' - Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
mkImageQuality ::
  ImageQuality
mkImageQuality =
  ImageQuality'
    { sharpness = Lude.Nothing,
      brightness = Lude.Nothing
    }

-- | Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iqSharpness :: Lens.Lens' ImageQuality (Lude.Maybe Lude.Double)
iqSharpness = Lens.lens (sharpness :: ImageQuality -> Lude.Maybe Lude.Double) (\s a -> s {sharpness = a} :: ImageQuality)
{-# DEPRECATED iqSharpness "Use generic-lens or generic-optics with 'sharpness' instead." #-}

-- | Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
--
-- /Note:/ Consider using 'brightness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iqBrightness :: Lens.Lens' ImageQuality (Lude.Maybe Lude.Double)
iqBrightness = Lens.lens (brightness :: ImageQuality -> Lude.Maybe Lude.Double) (\s a -> s {brightness = a} :: ImageQuality)
{-# DEPRECATED iqBrightness "Use generic-lens or generic-optics with 'brightness' instead." #-}

instance Lude.FromJSON ImageQuality where
  parseJSON =
    Lude.withObject
      "ImageQuality"
      ( \x ->
          ImageQuality'
            Lude.<$> (x Lude..:? "Sharpness") Lude.<*> (x Lude..:? "Brightness")
      )

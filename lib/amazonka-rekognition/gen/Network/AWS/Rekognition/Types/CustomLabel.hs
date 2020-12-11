-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CustomLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CustomLabel
  ( CustomLabel (..),

    -- * Smart constructor
    mkCustomLabel,

    -- * Lenses
    clConfidence,
    clName,
    clGeometry,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Geometry

-- | A custom label detected in an image by a call to 'DetectCustomLabels' .
--
-- /See:/ 'mkCustomLabel' smart constructor.
data CustomLabel = CustomLabel'
  { confidence ::
      Lude.Maybe Lude.Double,
    name :: Lude.Maybe Lude.Text,
    geometry :: Lude.Maybe Geometry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomLabel' with the minimum fields required to make a request.
--
-- * 'confidence' - The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
-- * 'geometry' - The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
-- * 'name' - The name of the custom label.
mkCustomLabel ::
  CustomLabel
mkCustomLabel =
  CustomLabel'
    { confidence = Lude.Nothing,
      name = Lude.Nothing,
      geometry = Lude.Nothing
    }

-- | The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConfidence :: Lens.Lens' CustomLabel (Lude.Maybe Lude.Double)
clConfidence = Lens.lens (confidence :: CustomLabel -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: CustomLabel)
{-# DEPRECATED clConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The name of the custom label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clName :: Lens.Lens' CustomLabel (Lude.Maybe Lude.Text)
clName = Lens.lens (name :: CustomLabel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CustomLabel)
{-# DEPRECATED clName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
--
-- /Note:/ Consider using 'geometry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clGeometry :: Lens.Lens' CustomLabel (Lude.Maybe Geometry)
clGeometry = Lens.lens (geometry :: CustomLabel -> Lude.Maybe Geometry) (\s a -> s {geometry = a} :: CustomLabel)
{-# DEPRECATED clGeometry "Use generic-lens or generic-optics with 'geometry' instead." #-}

instance Lude.FromJSON CustomLabel where
  parseJSON =
    Lude.withObject
      "CustomLabel"
      ( \x ->
          CustomLabel'
            Lude.<$> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Geometry")
      )

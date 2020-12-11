-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iBoundingBox,
    iConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox

-- | An instance of a label returned by Amazon Rekognition Image ('DetectLabels' ) or by Amazon Rekognition Video ('GetLabelDetection' ).
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { boundingBox :: Lude.Maybe BoundingBox,
    confidence :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'boundingBox' - The position of the label instance on the image.
-- * 'confidence' - The confidence that Amazon Rekognition has in the accuracy of the bounding box.
mkInstance ::
  Instance
mkInstance =
  Instance' {boundingBox = Lude.Nothing, confidence = Lude.Nothing}

-- | The position of the label instance on the image.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBoundingBox :: Lens.Lens' Instance (Lude.Maybe BoundingBox)
iBoundingBox = Lens.lens (boundingBox :: Instance -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: Instance)
{-# DEPRECATED iBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | The confidence that Amazon Rekognition has in the accuracy of the bounding box.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConfidence :: Lens.Lens' Instance (Lude.Maybe Lude.Double)
iConfidence = Lens.lens (confidence :: Instance -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Instance)
{-# DEPRECATED iConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "BoundingBox") Lude.<*> (x Lude..:? "Confidence")
      )

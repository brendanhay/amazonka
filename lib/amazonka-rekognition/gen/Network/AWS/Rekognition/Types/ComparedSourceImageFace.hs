-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ComparedSourceImageFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedSourceImageFace
  ( ComparedSourceImageFace (..),

    -- * Smart constructor
    mkComparedSourceImageFace,

    -- * Lenses
    csifBoundingBox,
    csifConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison.
--
-- /See:/ 'mkComparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
  { boundingBox ::
      Lude.Maybe BoundingBox,
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

-- | Creates a value of 'ComparedSourceImageFace' with the minimum fields required to make a request.
--
-- * 'boundingBox' - Bounding box of the face.
-- * 'confidence' - Confidence level that the selected bounding box contains a face.
mkComparedSourceImageFace ::
  ComparedSourceImageFace
mkComparedSourceImageFace =
  ComparedSourceImageFace'
    { boundingBox = Lude.Nothing,
      confidence = Lude.Nothing
    }

-- | Bounding box of the face.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csifBoundingBox :: Lens.Lens' ComparedSourceImageFace (Lude.Maybe BoundingBox)
csifBoundingBox = Lens.lens (boundingBox :: ComparedSourceImageFace -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: ComparedSourceImageFace)
{-# DEPRECATED csifBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Confidence level that the selected bounding box contains a face.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csifConfidence :: Lens.Lens' ComparedSourceImageFace (Lude.Maybe Lude.Double)
csifConfidence = Lens.lens (confidence :: ComparedSourceImageFace -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ComparedSourceImageFace)
{-# DEPRECATED csifConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON ComparedSourceImageFace where
  parseJSON =
    Lude.withObject
      "ComparedSourceImageFace"
      ( \x ->
          ComparedSourceImageFace'
            Lude.<$> (x Lude..:? "BoundingBox") Lude.<*> (x Lude..:? "Confidence")
      )

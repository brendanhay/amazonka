{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceMatch
  ( FaceMatch (..),

    -- * Smart constructor
    mkFaceMatch,

    -- * Lenses
    fmSimilarity,
    fmFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Face

-- | Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.
--
-- /See:/ 'mkFaceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { similarity :: Lude.Maybe Lude.Double,
    face :: Lude.Maybe Face
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaceMatch' with the minimum fields required to make a request.
--
-- * 'face' - Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
-- * 'similarity' - Confidence in the match of this face with the input face.
mkFaceMatch ::
  FaceMatch
mkFaceMatch =
  FaceMatch' {similarity = Lude.Nothing, face = Lude.Nothing}

-- | Confidence in the match of this face with the input face.
--
-- /Note:/ Consider using 'similarity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSimilarity :: Lens.Lens' FaceMatch (Lude.Maybe Lude.Double)
fmSimilarity = Lens.lens (similarity :: FaceMatch -> Lude.Maybe Lude.Double) (\s a -> s {similarity = a} :: FaceMatch)
{-# DEPRECATED fmSimilarity "Use generic-lens or generic-optics with 'similarity' instead." #-}

-- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmFace :: Lens.Lens' FaceMatch (Lude.Maybe Face)
fmFace = Lens.lens (face :: FaceMatch -> Lude.Maybe Face) (\s a -> s {face = a} :: FaceMatch)
{-# DEPRECATED fmFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON FaceMatch where
  parseJSON =
    Lude.withObject
      "FaceMatch"
      ( \x ->
          FaceMatch'
            Lude.<$> (x Lude..:? "Similarity") Lude.<*> (x Lude..:? "Face")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CompareFacesMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CompareFacesMatch
  ( CompareFacesMatch (..),

    -- * Smart constructor
    mkCompareFacesMatch,

    -- * Lenses
    cfmSimilarity,
    cfmFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a face in a target image that matches the source image face analyzed by @CompareFaces@ . The @Face@ property contains the bounding box of the face in the target image. The @Similarity@ property is the confidence that the source image face matches the face in the bounding box.
--
-- /See:/ 'mkCompareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { -- | Level of confidence that the faces match.
    similarity :: Lude.Maybe Lude.Double,
    -- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
    face :: Lude.Maybe ComparedFace
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompareFacesMatch' with the minimum fields required to make a request.
--
-- * 'similarity' - Level of confidence that the faces match.
-- * 'face' - Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
mkCompareFacesMatch ::
  CompareFacesMatch
mkCompareFacesMatch =
  CompareFacesMatch'
    { similarity = Lude.Nothing,
      face = Lude.Nothing
    }

-- | Level of confidence that the faces match.
--
-- /Note:/ Consider using 'similarity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfmSimilarity :: Lens.Lens' CompareFacesMatch (Lude.Maybe Lude.Double)
cfmSimilarity = Lens.lens (similarity :: CompareFacesMatch -> Lude.Maybe Lude.Double) (\s a -> s {similarity = a} :: CompareFacesMatch)
{-# DEPRECATED cfmSimilarity "Use generic-lens or generic-optics with 'similarity' instead." #-}

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfmFace :: Lens.Lens' CompareFacesMatch (Lude.Maybe ComparedFace)
cfmFace = Lens.lens (face :: CompareFacesMatch -> Lude.Maybe ComparedFace) (\s a -> s {face = a} :: CompareFacesMatch)
{-# DEPRECATED cfmFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON CompareFacesMatch where
  parseJSON =
    Lude.withObject
      "CompareFacesMatch"
      ( \x ->
          CompareFacesMatch'
            Lude.<$> (x Lude..:? "Similarity") Lude.<*> (x Lude..:? "Face")
      )

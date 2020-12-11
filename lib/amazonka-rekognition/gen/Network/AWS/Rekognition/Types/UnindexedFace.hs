-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.UnindexedFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.UnindexedFace
  ( UnindexedFace (..),

    -- * Smart constructor
    mkUnindexedFace,

    -- * Lenses
    ufReasons,
    ufFaceDetail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.Reason

-- | A face that 'IndexFaces' detected, but didn't index. Use the @Reasons@ response attribute to determine why a face wasn't indexed.
--
-- /See:/ 'mkUnindexedFace' smart constructor.
data UnindexedFace = UnindexedFace'
  { reasons :: Lude.Maybe [Reason],
    faceDetail :: Lude.Maybe FaceDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnindexedFace' with the minimum fields required to make a request.
--
-- * 'faceDetail' - The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
-- * 'reasons' - An array of reasons that specify why a face wasn't indexed.
--
--
--     * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.
--
--
--     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .
--
--
--     * LOW_BRIGHTNESS - The image is too dark.
--
--
--     * LOW_SHARPNESS - The image is too blurry.
--
--
--     * LOW_CONFIDENCE - The face was detected with a low confidence.
--
--
--     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
mkUnindexedFace ::
  UnindexedFace
mkUnindexedFace =
  UnindexedFace' {reasons = Lude.Nothing, faceDetail = Lude.Nothing}

-- | An array of reasons that specify why a face wasn't indexed.
--
--
--     * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.
--
--
--     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .
--
--
--     * LOW_BRIGHTNESS - The image is too dark.
--
--
--     * LOW_SHARPNESS - The image is too blurry.
--
--
--     * LOW_CONFIDENCE - The face was detected with a low confidence.
--
--
--     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
--
--
--
-- /Note:/ Consider using 'reasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufReasons :: Lens.Lens' UnindexedFace (Lude.Maybe [Reason])
ufReasons = Lens.lens (reasons :: UnindexedFace -> Lude.Maybe [Reason]) (\s a -> s {reasons = a} :: UnindexedFace)
{-# DEPRECATED ufReasons "Use generic-lens or generic-optics with 'reasons' instead." #-}

-- | The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
--
-- /Note:/ Consider using 'faceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFaceDetail :: Lens.Lens' UnindexedFace (Lude.Maybe FaceDetail)
ufFaceDetail = Lens.lens (faceDetail :: UnindexedFace -> Lude.Maybe FaceDetail) (\s a -> s {faceDetail = a} :: UnindexedFace)
{-# DEPRECATED ufFaceDetail "Use generic-lens or generic-optics with 'faceDetail' instead." #-}

instance Lude.FromJSON UnindexedFace where
  parseJSON =
    Lude.withObject
      "UnindexedFace"
      ( \x ->
          UnindexedFace'
            Lude.<$> (x Lude..:? "Reasons" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "FaceDetail")
      )

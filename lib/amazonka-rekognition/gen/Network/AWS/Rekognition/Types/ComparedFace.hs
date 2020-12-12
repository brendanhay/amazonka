{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ComparedFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedFace
  ( ComparedFace (..),

    -- * Smart constructor
    mkComparedFace,

    -- * Lenses
    cfBoundingBox,
    cfPose,
    cfConfidence,
    cfQuality,
    cfLandmarks,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.Pose

-- | Provides face metadata for target image faces that are analyzed by @CompareFaces@ and @RecognizeCelebrities@ .
--
-- /See:/ 'mkComparedFace' smart constructor.
data ComparedFace = ComparedFace'
  { boundingBox ::
      Lude.Maybe BoundingBox,
    pose :: Lude.Maybe Pose,
    confidence :: Lude.Maybe Lude.Double,
    quality :: Lude.Maybe ImageQuality,
    landmarks :: Lude.Maybe [Landmark]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComparedFace' with the minimum fields required to make a request.
--
-- * 'boundingBox' - Bounding box of the face.
-- * 'confidence' - Level of confidence that what the bounding box contains is a face.
-- * 'landmarks' - An array of facial landmarks.
-- * 'pose' - Indicates the pose of the face as determined by its pitch, roll, and yaw.
-- * 'quality' - Identifies face image brightness and sharpness.
mkComparedFace ::
  ComparedFace
mkComparedFace =
  ComparedFace'
    { boundingBox = Lude.Nothing,
      pose = Lude.Nothing,
      confidence = Lude.Nothing,
      quality = Lude.Nothing,
      landmarks = Lude.Nothing
    }

-- | Bounding box of the face.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfBoundingBox :: Lens.Lens' ComparedFace (Lude.Maybe BoundingBox)
cfBoundingBox = Lens.lens (boundingBox :: ComparedFace -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: ComparedFace)
{-# DEPRECATED cfBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
-- /Note:/ Consider using 'pose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPose :: Lens.Lens' ComparedFace (Lude.Maybe Pose)
cfPose = Lens.lens (pose :: ComparedFace -> Lude.Maybe Pose) (\s a -> s {pose = a} :: ComparedFace)
{-# DEPRECATED cfPose "Use generic-lens or generic-optics with 'pose' instead." #-}

-- | Level of confidence that what the bounding box contains is a face.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConfidence :: Lens.Lens' ComparedFace (Lude.Maybe Lude.Double)
cfConfidence = Lens.lens (confidence :: ComparedFace -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ComparedFace)
{-# DEPRECATED cfConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Identifies face image brightness and sharpness.
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfQuality :: Lens.Lens' ComparedFace (Lude.Maybe ImageQuality)
cfQuality = Lens.lens (quality :: ComparedFace -> Lude.Maybe ImageQuality) (\s a -> s {quality = a} :: ComparedFace)
{-# DEPRECATED cfQuality "Use generic-lens or generic-optics with 'quality' instead." #-}

-- | An array of facial landmarks.
--
-- /Note:/ Consider using 'landmarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLandmarks :: Lens.Lens' ComparedFace (Lude.Maybe [Landmark])
cfLandmarks = Lens.lens (landmarks :: ComparedFace -> Lude.Maybe [Landmark]) (\s a -> s {landmarks = a} :: ComparedFace)
{-# DEPRECATED cfLandmarks "Use generic-lens or generic-optics with 'landmarks' instead." #-}

instance Lude.FromJSON ComparedFace where
  parseJSON =
    Lude.withObject
      "ComparedFace"
      ( \x ->
          ComparedFace'
            Lude.<$> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Pose")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Quality")
            Lude.<*> (x Lude..:? "Landmarks" Lude..!= Lude.mempty)
      )

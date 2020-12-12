{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetection
  ( FaceDetection (..),

    -- * Smart constructor
    mkFaceDetection,

    -- * Lenses
    fdTimestamp,
    fdFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.FaceDetail

-- | Information about a face detected in a video analysis request and the time the face was detected in the video.
--
-- /See:/ 'mkFaceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { timestamp ::
      Lude.Maybe Lude.Integer,
    face :: Lude.Maybe FaceDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaceDetection' with the minimum fields required to make a request.
--
-- * 'face' - The face properties for the detected face.
-- * 'timestamp' - Time, in milliseconds from the start of the video, that the face was detected.
mkFaceDetection ::
  FaceDetection
mkFaceDetection =
  FaceDetection' {timestamp = Lude.Nothing, face = Lude.Nothing}

-- | Time, in milliseconds from the start of the video, that the face was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTimestamp :: Lens.Lens' FaceDetection (Lude.Maybe Lude.Integer)
fdTimestamp = Lens.lens (timestamp :: FaceDetection -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: FaceDetection)
{-# DEPRECATED fdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The face properties for the detected face.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFace :: Lens.Lens' FaceDetection (Lude.Maybe FaceDetail)
fdFace = Lens.lens (face :: FaceDetection -> Lude.Maybe FaceDetail) (\s a -> s {face = a} :: FaceDetection)
{-# DEPRECATED fdFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON FaceDetection where
  parseJSON =
    Lude.withObject
      "FaceDetection"
      ( \x ->
          FaceDetection'
            Lude.<$> (x Lude..:? "Timestamp") Lude.<*> (x Lude..:? "Face")
      )

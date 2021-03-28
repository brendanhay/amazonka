{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.FaceDetection
  ( FaceDetection (..)
  -- * Smart constructor
  , mkFaceDetection
  -- * Lenses
  , fdFace
  , fdTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.FaceDetail as Types

-- | Information about a face detected in a video analysis request and the time the face was detected in the video. 
--
-- /See:/ 'mkFaceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { face :: Core.Maybe Types.FaceDetail
    -- ^ The face properties for the detected face.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ Time, in milliseconds from the start of the video, that the face was detected.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaceDetection' value with any optional fields omitted.
mkFaceDetection
    :: FaceDetection
mkFaceDetection
  = FaceDetection'{face = Core.Nothing, timestamp = Core.Nothing}

-- | The face properties for the detected face.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFace :: Lens.Lens' FaceDetection (Core.Maybe Types.FaceDetail)
fdFace = Lens.field @"face"
{-# INLINEABLE fdFace #-}
{-# DEPRECATED face "Use generic-lens or generic-optics with 'face' instead"  #-}

-- | Time, in milliseconds from the start of the video, that the face was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTimestamp :: Lens.Lens' FaceDetection (Core.Maybe Core.Integer)
fdTimestamp = Lens.field @"timestamp"
{-# INLINEABLE fdTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON FaceDetection where
        parseJSON
          = Core.withObject "FaceDetection" Core.$
              \ x ->
                FaceDetection' Core.<$>
                  (x Core..:? "Face") Core.<*> x Core..:? "Timestamp"

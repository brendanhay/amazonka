{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TextDetectionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TextDetectionResult
  ( TextDetectionResult (..)
  -- * Smart constructor
  , mkTextDetectionResult
  -- * Lenses
  , tdrTextDetection
  , tdrTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.TextDetection as Types

-- | Information about text detected in a video. Incudes the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- /See:/ 'mkTextDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { textDetection :: Core.Maybe Types.TextDetection
    -- ^ Details about text detected in a video.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds from the start of the video, that the text was detected.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextDetectionResult' value with any optional fields omitted.
mkTextDetectionResult
    :: TextDetectionResult
mkTextDetectionResult
  = TextDetectionResult'{textDetection = Core.Nothing,
                         timestamp = Core.Nothing}

-- | Details about text detected in a video.
--
-- /Note:/ Consider using 'textDetection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrTextDetection :: Lens.Lens' TextDetectionResult (Core.Maybe Types.TextDetection)
tdrTextDetection = Lens.field @"textDetection"
{-# INLINEABLE tdrTextDetection #-}
{-# DEPRECATED textDetection "Use generic-lens or generic-optics with 'textDetection' instead"  #-}

-- | The time, in milliseconds from the start of the video, that the text was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrTimestamp :: Lens.Lens' TextDetectionResult (Core.Maybe Core.Integer)
tdrTimestamp = Lens.field @"timestamp"
{-# INLINEABLE tdrTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON TextDetectionResult where
        parseJSON
          = Core.withObject "TextDetectionResult" Core.$
              \ x ->
                TextDetectionResult' Core.<$>
                  (x Core..:? "TextDetection") Core.<*> x Core..:? "Timestamp"

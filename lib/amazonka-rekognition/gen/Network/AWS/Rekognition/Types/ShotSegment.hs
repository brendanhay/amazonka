{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ShotSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ShotSegment
  ( ShotSegment (..)
  -- * Smart constructor
  , mkShotSegment
  -- * Lenses
  , ssConfidence
  , ssIndex
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a shot detection segment detected in a video. For more information, see 'SegmentDetection' .
--
-- /See:/ 'mkShotSegment' smart constructor.
data ShotSegment = ShotSegment'
  { confidence :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
  , index :: Core.Maybe Core.Natural
    -- ^ An Identifier for a shot detection segment detected in a video. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShotSegment' value with any optional fields omitted.
mkShotSegment
    :: ShotSegment
mkShotSegment
  = ShotSegment'{confidence = Core.Nothing, index = Core.Nothing}

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssConfidence :: Lens.Lens' ShotSegment (Core.Maybe Core.Double)
ssConfidence = Lens.field @"confidence"
{-# INLINEABLE ssConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | An Identifier for a shot detection segment detected in a video. 
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIndex :: Lens.Lens' ShotSegment (Core.Maybe Core.Natural)
ssIndex = Lens.field @"index"
{-# INLINEABLE ssIndex #-}
{-# DEPRECATED index "Use generic-lens or generic-optics with 'index' instead"  #-}

instance Core.FromJSON ShotSegment where
        parseJSON
          = Core.withObject "ShotSegment" Core.$
              \ x ->
                ShotSegment' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Index"

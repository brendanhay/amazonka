{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TechnicalCueSegment
  ( TechnicalCueSegment (..)
  -- * Smart constructor
  , mkTechnicalCueSegment
  -- * Lenses
  , tcsConfidence
  , tcsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.TechnicalCueType as Types

-- | Information about a technical cue segment. For more information, see 'SegmentDetection' .
--
-- /See:/ 'mkTechnicalCueSegment' smart constructor.
data TechnicalCueSegment = TechnicalCueSegment'
  { confidence :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
  , type' :: Core.Maybe Types.TechnicalCueType
    -- ^ The type of the technical cue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TechnicalCueSegment' value with any optional fields omitted.
mkTechnicalCueSegment
    :: TechnicalCueSegment
mkTechnicalCueSegment
  = TechnicalCueSegment'{confidence = Core.Nothing,
                         type' = Core.Nothing}

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsConfidence :: Lens.Lens' TechnicalCueSegment (Core.Maybe Core.Double)
tcsConfidence = Lens.field @"confidence"
{-# INLINEABLE tcsConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | The type of the technical cue.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsType :: Lens.Lens' TechnicalCueSegment (Core.Maybe Types.TechnicalCueType)
tcsType = Lens.field @"type'"
{-# INLINEABLE tcsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON TechnicalCueSegment where
        parseJSON
          = Core.withObject "TechnicalCueSegment" Core.$
              \ x ->
                TechnicalCueSegment' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Type"

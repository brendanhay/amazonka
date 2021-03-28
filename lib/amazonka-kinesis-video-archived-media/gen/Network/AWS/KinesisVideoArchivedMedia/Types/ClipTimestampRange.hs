{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
  ( ClipTimestampRange (..)
  -- * Smart constructor
  , mkClipTimestampRange
  -- * Lenses
  , ctrStartTimestamp
  , ctrEndTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The range of timestamps for which to return fragments.
--
-- The values in the ClipTimestampRange are @inclusive@ . Fragments that begin before the start time but continue past it, or fragments that begin before the end time but continue past it, are included in the session. 
--
-- /See:/ 'mkClipTimestampRange' smart constructor.
data ClipTimestampRange = ClipTimestampRange'
  { startTimestamp :: Core.NominalDiffTime
    -- ^ The starting timestamp in the range of timestamps for which to return fragments. 
--
-- This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head. 
  , endTimestamp :: Core.NominalDiffTime
    -- ^ The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. 
-- This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClipTimestampRange' value with any optional fields omitted.
mkClipTimestampRange
    :: Core.NominalDiffTime -- ^ 'startTimestamp'
    -> Core.NominalDiffTime -- ^ 'endTimestamp'
    -> ClipTimestampRange
mkClipTimestampRange startTimestamp endTimestamp
  = ClipTimestampRange'{startTimestamp, endTimestamp}

-- | The starting timestamp in the range of timestamps for which to return fragments. 
--
-- This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head. 
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrStartTimestamp :: Lens.Lens' ClipTimestampRange Core.NominalDiffTime
ctrStartTimestamp = Lens.field @"startTimestamp"
{-# INLINEABLE ctrStartTimestamp #-}
{-# DEPRECATED startTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead"  #-}

-- | The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. 
-- This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session. 
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrEndTimestamp :: Lens.Lens' ClipTimestampRange Core.NominalDiffTime
ctrEndTimestamp = Lens.field @"endTimestamp"
{-# INLINEABLE ctrEndTimestamp #-}
{-# DEPRECATED endTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead"  #-}

instance Core.FromJSON ClipTimestampRange where
        toJSON ClipTimestampRange{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartTimestamp" Core..= startTimestamp),
                  Core.Just ("EndTimestamp" Core..= endTimestamp)])

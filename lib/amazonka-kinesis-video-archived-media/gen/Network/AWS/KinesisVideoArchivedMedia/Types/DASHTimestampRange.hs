{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
  ( DASHTimestampRange (..)
  -- * Smart constructor
  , mkDASHTimestampRange
  -- * Lenses
  , dashtrEndTimestamp
  , dashtrStartTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
-- /See:/ 'mkDASHTimestampRange' smart constructor.
data DASHTimestampRange = DASHTimestampRange'
  { endTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
  , startTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The start of the timestamp range for the requested media.
--
-- If the @DASHTimestampRange@ value is specified, the @StartTimestamp@ value is required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DASHTimestampRange' value with any optional fields omitted.
mkDASHTimestampRange
    :: DASHTimestampRange
mkDASHTimestampRange
  = DASHTimestampRange'{endTimestamp = Core.Nothing,
                        startTimestamp = Core.Nothing}

-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dashtrEndTimestamp :: Lens.Lens' DASHTimestampRange (Core.Maybe Core.NominalDiffTime)
dashtrEndTimestamp = Lens.field @"endTimestamp"
{-# INLINEABLE dashtrEndTimestamp #-}
{-# DEPRECATED endTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead"  #-}

-- | The start of the timestamp range for the requested media.
--
-- If the @DASHTimestampRange@ value is specified, the @StartTimestamp@ value is required.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dashtrStartTimestamp :: Lens.Lens' DASHTimestampRange (Core.Maybe Core.NominalDiffTime)
dashtrStartTimestamp = Lens.field @"startTimestamp"
{-# INLINEABLE dashtrStartTimestamp #-}
{-# DEPRECATED startTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead"  #-}

instance Core.FromJSON DASHTimestampRange where
        toJSON DASHTimestampRange{..}
          = Core.object
              (Core.catMaybes
                 [("EndTimestamp" Core..=) Core.<$> endTimestamp,
                  ("StartTimestamp" Core..=) Core.<$> startTimestamp])

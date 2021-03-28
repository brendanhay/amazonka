{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
  ( RejectedLogEventsInfo (..)
  -- * Smart constructor
  , mkRejectedLogEventsInfo
  -- * Lenses
  , rleiExpiredLogEventEndIndex
  , rleiTooNewLogEventStartIndex
  , rleiTooOldLogEventEndIndex
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the rejected events.
--
-- /See:/ 'mkRejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
  { expiredLogEventEndIndex :: Core.Maybe Core.Int
    -- ^ The expired log events.
  , tooNewLogEventStartIndex :: Core.Maybe Core.Int
    -- ^ The log events that are too new.
  , tooOldLogEventEndIndex :: Core.Maybe Core.Int
    -- ^ The log events that are too old.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectedLogEventsInfo' value with any optional fields omitted.
mkRejectedLogEventsInfo
    :: RejectedLogEventsInfo
mkRejectedLogEventsInfo
  = RejectedLogEventsInfo'{expiredLogEventEndIndex = Core.Nothing,
                           tooNewLogEventStartIndex = Core.Nothing,
                           tooOldLogEventEndIndex = Core.Nothing}

-- | The expired log events.
--
-- /Note:/ Consider using 'expiredLogEventEndIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiExpiredLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Core.Maybe Core.Int)
rleiExpiredLogEventEndIndex = Lens.field @"expiredLogEventEndIndex"
{-# INLINEABLE rleiExpiredLogEventEndIndex #-}
{-# DEPRECATED expiredLogEventEndIndex "Use generic-lens or generic-optics with 'expiredLogEventEndIndex' instead"  #-}

-- | The log events that are too new.
--
-- /Note:/ Consider using 'tooNewLogEventStartIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiTooNewLogEventStartIndex :: Lens.Lens' RejectedLogEventsInfo (Core.Maybe Core.Int)
rleiTooNewLogEventStartIndex = Lens.field @"tooNewLogEventStartIndex"
{-# INLINEABLE rleiTooNewLogEventStartIndex #-}
{-# DEPRECATED tooNewLogEventStartIndex "Use generic-lens or generic-optics with 'tooNewLogEventStartIndex' instead"  #-}

-- | The log events that are too old.
--
-- /Note:/ Consider using 'tooOldLogEventEndIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiTooOldLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Core.Maybe Core.Int)
rleiTooOldLogEventEndIndex = Lens.field @"tooOldLogEventEndIndex"
{-# INLINEABLE rleiTooOldLogEventEndIndex #-}
{-# DEPRECATED tooOldLogEventEndIndex "Use generic-lens or generic-optics with 'tooOldLogEventEndIndex' instead"  #-}

instance Core.FromJSON RejectedLogEventsInfo where
        parseJSON
          = Core.withObject "RejectedLogEventsInfo" Core.$
              \ x ->
                RejectedLogEventsInfo' Core.<$>
                  (x Core..:? "expiredLogEventEndIndex") Core.<*>
                    x Core..:? "tooNewLogEventStartIndex"
                    Core.<*> x Core..:? "tooOldLogEventEndIndex"

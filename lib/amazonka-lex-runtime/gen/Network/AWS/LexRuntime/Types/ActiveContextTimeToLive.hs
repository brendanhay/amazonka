{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
  ( ActiveContextTimeToLive (..)
  -- * Smart constructor
  , mkActiveContextTimeToLive
  -- * Lenses
  , acttlTimeToLiveInSeconds
  , acttlTurnsToLive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The length of time or number of turns that a context remains active.
--
-- /See:/ 'mkActiveContextTimeToLive' smart constructor.
data ActiveContextTimeToLive = ActiveContextTimeToLive'
  { timeToLiveInSeconds :: Core.Maybe Core.Natural
    -- ^ The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
  , turnsToLive :: Core.Maybe Core.Natural
    -- ^ The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActiveContextTimeToLive' value with any optional fields omitted.
mkActiveContextTimeToLive
    :: ActiveContextTimeToLive
mkActiveContextTimeToLive
  = ActiveContextTimeToLive'{timeToLiveInSeconds = Core.Nothing,
                             turnsToLive = Core.Nothing}

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acttlTimeToLiveInSeconds :: Lens.Lens' ActiveContextTimeToLive (Core.Maybe Core.Natural)
acttlTimeToLiveInSeconds = Lens.field @"timeToLiveInSeconds"
{-# INLINEABLE acttlTimeToLiveInSeconds #-}
{-# DEPRECATED timeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead"  #-}

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
--
-- /Note:/ Consider using 'turnsToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acttlTurnsToLive :: Lens.Lens' ActiveContextTimeToLive (Core.Maybe Core.Natural)
acttlTurnsToLive = Lens.field @"turnsToLive"
{-# INLINEABLE acttlTurnsToLive #-}
{-# DEPRECATED turnsToLive "Use generic-lens or generic-optics with 'turnsToLive' instead"  #-}

instance Core.FromJSON ActiveContextTimeToLive where
        toJSON ActiveContextTimeToLive{..}
          = Core.object
              (Core.catMaybes
                 [("timeToLiveInSeconds" Core..=) Core.<$> timeToLiveInSeconds,
                  ("turnsToLive" Core..=) Core.<$> turnsToLive])

instance Core.FromJSON ActiveContextTimeToLive where
        parseJSON
          = Core.withObject "ActiveContextTimeToLive" Core.$
              \ x ->
                ActiveContextTimeToLive' Core.<$>
                  (x Core..:? "timeToLiveInSeconds") Core.<*>
                    x Core..:? "turnsToLive"

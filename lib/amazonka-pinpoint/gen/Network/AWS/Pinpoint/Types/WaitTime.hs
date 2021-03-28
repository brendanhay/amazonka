{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WaitTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.WaitTime
  ( WaitTime (..)
  -- * Smart constructor
  , mkWaitTime
  -- * Lenses
  , wtWaitFor
  , wtWaitUntil
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a duration or a date and time that indicates when Amazon Pinpoint determines whether an activity's conditions have been met or an activity moves participants to the next activity in a journey.
--
-- /See:/ 'mkWaitTime' smart constructor.
data WaitTime = WaitTime'
  { waitFor :: Core.Maybe Core.Text
    -- ^ The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
  , waitUntil :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WaitTime' value with any optional fields omitted.
mkWaitTime
    :: WaitTime
mkWaitTime
  = WaitTime'{waitFor = Core.Nothing, waitUntil = Core.Nothing}

-- | The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitFor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtWaitFor :: Lens.Lens' WaitTime (Core.Maybe Core.Text)
wtWaitFor = Lens.field @"waitFor"
{-# INLINEABLE wtWaitFor #-}
{-# DEPRECATED waitFor "Use generic-lens or generic-optics with 'waitFor' instead"  #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtWaitUntil :: Lens.Lens' WaitTime (Core.Maybe Core.Text)
wtWaitUntil = Lens.field @"waitUntil"
{-# INLINEABLE wtWaitUntil #-}
{-# DEPRECATED waitUntil "Use generic-lens or generic-optics with 'waitUntil' instead"  #-}

instance Core.FromJSON WaitTime where
        toJSON WaitTime{..}
          = Core.object
              (Core.catMaybes
                 [("WaitFor" Core..=) Core.<$> waitFor,
                  ("WaitUntil" Core..=) Core.<$> waitUntil])

instance Core.FromJSON WaitTime where
        parseJSON
          = Core.withObject "WaitTime" Core.$
              \ x ->
                WaitTime' Core.<$>
                  (x Core..:? "WaitFor") Core.<*> x Core..:? "WaitUntil"

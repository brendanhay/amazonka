{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.BookingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.BookingOptions
  ( BookingOptions (..)
  -- * Smart constructor
  , mkBookingOptions
  -- * Lenses
  , boAutoAcceptRequests
  , boAutoDeclineConflictingRequests
  , boAutoDeclineRecurringRequests
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | At least one delegate must be associated to the resource to disable automatic replies from the resource.
--
-- /See:/ 'mkBookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { autoAcceptRequests :: Core.Maybe Core.Bool
    -- ^ The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
  , autoDeclineConflictingRequests :: Core.Maybe Core.Bool
    -- ^ The resource's ability to automatically decline any conflicting requests.
  , autoDeclineRecurringRequests :: Core.Maybe Core.Bool
    -- ^ The resource's ability to automatically decline any recurring requests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BookingOptions' value with any optional fields omitted.
mkBookingOptions
    :: BookingOptions
mkBookingOptions
  = BookingOptions'{autoAcceptRequests = Core.Nothing,
                    autoDeclineConflictingRequests = Core.Nothing,
                    autoDeclineRecurringRequests = Core.Nothing}

-- | The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
--
-- /Note:/ Consider using 'autoAcceptRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoAcceptRequests :: Lens.Lens' BookingOptions (Core.Maybe Core.Bool)
boAutoAcceptRequests = Lens.field @"autoAcceptRequests"
{-# INLINEABLE boAutoAcceptRequests #-}
{-# DEPRECATED autoAcceptRequests "Use generic-lens or generic-optics with 'autoAcceptRequests' instead"  #-}

-- | The resource's ability to automatically decline any conflicting requests.
--
-- /Note:/ Consider using 'autoDeclineConflictingRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoDeclineConflictingRequests :: Lens.Lens' BookingOptions (Core.Maybe Core.Bool)
boAutoDeclineConflictingRequests = Lens.field @"autoDeclineConflictingRequests"
{-# INLINEABLE boAutoDeclineConflictingRequests #-}
{-# DEPRECATED autoDeclineConflictingRequests "Use generic-lens or generic-optics with 'autoDeclineConflictingRequests' instead"  #-}

-- | The resource's ability to automatically decline any recurring requests.
--
-- /Note:/ Consider using 'autoDeclineRecurringRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoDeclineRecurringRequests :: Lens.Lens' BookingOptions (Core.Maybe Core.Bool)
boAutoDeclineRecurringRequests = Lens.field @"autoDeclineRecurringRequests"
{-# INLINEABLE boAutoDeclineRecurringRequests #-}
{-# DEPRECATED autoDeclineRecurringRequests "Use generic-lens or generic-optics with 'autoDeclineRecurringRequests' instead"  #-}

instance Core.FromJSON BookingOptions where
        toJSON BookingOptions{..}
          = Core.object
              (Core.catMaybes
                 [("AutoAcceptRequests" Core..=) Core.<$> autoAcceptRequests,
                  ("AutoDeclineConflictingRequests" Core..=) Core.<$>
                    autoDeclineConflictingRequests,
                  ("AutoDeclineRecurringRequests" Core..=) Core.<$>
                    autoDeclineRecurringRequests])

instance Core.FromJSON BookingOptions where
        parseJSON
          = Core.withObject "BookingOptions" Core.$
              \ x ->
                BookingOptions' Core.<$>
                  (x Core..:? "AutoAcceptRequests") Core.<*>
                    x Core..:? "AutoDeclineConflictingRequests"
                    Core.<*> x Core..:? "AutoDeclineRecurringRequests"

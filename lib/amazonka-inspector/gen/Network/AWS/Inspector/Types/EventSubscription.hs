{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.EventSubscription
  ( EventSubscription (..)
  -- * Smart constructor
  , mkEventSubscription
  -- * Lenses
  , esEvent
  , esSubscribedAt
  ) where

import qualified Network.AWS.Inspector.Types.InspectorEvent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'Subscription' data type.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { event :: Types.InspectorEvent
    -- ^ The event for which Amazon Simple Notification Service (SNS) notifications are sent.
  , subscribedAt :: Core.NominalDiffTime
    -- ^ The time at which 'SubscribeToEvent' is called.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventSubscription' value with any optional fields omitted.
mkEventSubscription
    :: Types.InspectorEvent -- ^ 'event'
    -> Core.NominalDiffTime -- ^ 'subscribedAt'
    -> EventSubscription
mkEventSubscription event subscribedAt
  = EventSubscription'{event, subscribedAt}

-- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEvent :: Lens.Lens' EventSubscription Types.InspectorEvent
esEvent = Lens.field @"event"
{-# INLINEABLE esEvent #-}
{-# DEPRECATED event "Use generic-lens or generic-optics with 'event' instead"  #-}

-- | The time at which 'SubscribeToEvent' is called.
--
-- /Note:/ Consider using 'subscribedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscribedAt :: Lens.Lens' EventSubscription Core.NominalDiffTime
esSubscribedAt = Lens.field @"subscribedAt"
{-# INLINEABLE esSubscribedAt #-}
{-# DEPRECATED subscribedAt "Use generic-lens or generic-optics with 'subscribedAt' instead"  #-}

instance Core.FromJSON EventSubscription where
        parseJSON
          = Core.withObject "EventSubscription" Core.$
              \ x ->
                EventSubscription' Core.<$>
                  (x Core..: "event") Core.<*> x Core..: "subscribedAt"

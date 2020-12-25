{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.EventSubscription
  ( EventSubscription (..),

    -- * Smart constructor
    mkEventSubscription,

    -- * Lenses
    esEvent,
    esSubscribedAt,
  )
where

import qualified Network.AWS.Inspector.Types.InspectorEvent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'Subscription' data type.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
    event :: Types.InspectorEvent,
    -- | The time at which 'SubscribeToEvent' is called.
    subscribedAt :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventSubscription' value with any optional fields omitted.
mkEventSubscription ::
  -- | 'event'
  Types.InspectorEvent ->
  -- | 'subscribedAt'
  Core.NominalDiffTime ->
  EventSubscription
mkEventSubscription event subscribedAt =
  EventSubscription' {event, subscribedAt}

-- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEvent :: Lens.Lens' EventSubscription Types.InspectorEvent
esEvent = Lens.field @"event"
{-# DEPRECATED esEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The time at which 'SubscribeToEvent' is called.
--
-- /Note:/ Consider using 'subscribedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscribedAt :: Lens.Lens' EventSubscription Core.NominalDiffTime
esSubscribedAt = Lens.field @"subscribedAt"
{-# DEPRECATED esSubscribedAt "Use generic-lens or generic-optics with 'subscribedAt' instead." #-}

instance Core.FromJSON EventSubscription where
  parseJSON =
    Core.withObject "EventSubscription" Core.$
      \x ->
        EventSubscription'
          Core.<$> (x Core..: "event") Core.<*> (x Core..: "subscribedAt")

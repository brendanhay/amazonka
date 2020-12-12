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

import Network.AWS.Inspector.Types.InspectorEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used in the 'Subscription' data type.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { event ::
      InspectorEvent,
    subscribedAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- * 'event' - The event for which Amazon Simple Notification Service (SNS) notifications are sent.
-- * 'subscribedAt' - The time at which 'SubscribeToEvent' is called.
mkEventSubscription ::
  -- | 'event'
  InspectorEvent ->
  -- | 'subscribedAt'
  Lude.Timestamp ->
  EventSubscription
mkEventSubscription pEvent_ pSubscribedAt_ =
  EventSubscription'
    { event = pEvent_,
      subscribedAt = pSubscribedAt_
    }

-- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEvent :: Lens.Lens' EventSubscription InspectorEvent
esEvent = Lens.lens (event :: EventSubscription -> InspectorEvent) (\s a -> s {event = a} :: EventSubscription)
{-# DEPRECATED esEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The time at which 'SubscribeToEvent' is called.
--
-- /Note:/ Consider using 'subscribedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscribedAt :: Lens.Lens' EventSubscription Lude.Timestamp
esSubscribedAt = Lens.lens (subscribedAt :: EventSubscription -> Lude.Timestamp) (\s a -> s {subscribedAt = a} :: EventSubscription)
{-# DEPRECATED esSubscribedAt "Use generic-lens or generic-optics with 'subscribedAt' instead." #-}

instance Lude.FromJSON EventSubscription where
  parseJSON =
    Lude.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Lude.<$> (x Lude..: "event") Lude.<*> (x Lude..: "subscribedAt")
      )

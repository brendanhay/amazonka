{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.EventSubscription where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.InspectorEvent
import qualified Network.AWS.Lens as Lens

-- | This data type is used in the Subscription data type.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The event for which Amazon Simple Notification Service (SNS)
    -- notifications are sent.
    event :: InspectorEvent,
    -- | The time at which SubscribeToEvent is called.
    subscribedAt :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'eventSubscription_event' - The event for which Amazon Simple Notification Service (SNS)
-- notifications are sent.
--
-- 'subscribedAt', 'eventSubscription_subscribedAt' - The time at which SubscribeToEvent is called.
newEventSubscription ::
  -- | 'event'
  InspectorEvent ->
  -- | 'subscribedAt'
  Core.UTCTime ->
  EventSubscription
newEventSubscription pEvent_ pSubscribedAt_ =
  EventSubscription'
    { event = pEvent_,
      subscribedAt = Core._Time Lens.# pSubscribedAt_
    }

-- | The event for which Amazon Simple Notification Service (SNS)
-- notifications are sent.
eventSubscription_event :: Lens.Lens' EventSubscription InspectorEvent
eventSubscription_event = Lens.lens (\EventSubscription' {event} -> event) (\s@EventSubscription' {} a -> s {event = a} :: EventSubscription)

-- | The time at which SubscribeToEvent is called.
eventSubscription_subscribedAt :: Lens.Lens' EventSubscription Core.UTCTime
eventSubscription_subscribedAt = Lens.lens (\EventSubscription' {subscribedAt} -> subscribedAt) (\s@EventSubscription' {} a -> s {subscribedAt = a} :: EventSubscription) Core.. Core._Time

instance Core.FromJSON EventSubscription where
  parseJSON =
    Core.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Core.<$> (x Core..: "event")
            Core.<*> (x Core..: "subscribedAt")
      )

instance Core.Hashable EventSubscription

instance Core.NFData EventSubscription

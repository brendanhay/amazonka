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
-- Module      : Amazonka.Inspector.Types.EventSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.EventSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.InspectorEvent
import qualified Amazonka.Prelude as Prelude

-- | This data type is used in the Subscription data type.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The event for which Amazon Simple Notification Service (SNS)
    -- notifications are sent.
    event :: InspectorEvent,
    -- | The time at which SubscribeToEvent is called.
    subscribedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.UTCTime ->
  EventSubscription
newEventSubscription pEvent_ pSubscribedAt_ =
  EventSubscription'
    { event = pEvent_,
      subscribedAt = Data._Time Lens.# pSubscribedAt_
    }

-- | The event for which Amazon Simple Notification Service (SNS)
-- notifications are sent.
eventSubscription_event :: Lens.Lens' EventSubscription InspectorEvent
eventSubscription_event = Lens.lens (\EventSubscription' {event} -> event) (\s@EventSubscription' {} a -> s {event = a} :: EventSubscription)

-- | The time at which SubscribeToEvent is called.
eventSubscription_subscribedAt :: Lens.Lens' EventSubscription Prelude.UTCTime
eventSubscription_subscribedAt = Lens.lens (\EventSubscription' {subscribedAt} -> subscribedAt) (\s@EventSubscription' {} a -> s {subscribedAt = a} :: EventSubscription) Prelude.. Data._Time

instance Data.FromJSON EventSubscription where
  parseJSON =
    Data.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Prelude.<$> (x Data..: "event")
            Prelude.<*> (x Data..: "subscribedAt")
      )

instance Prelude.Hashable EventSubscription where
  hashWithSalt _salt EventSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` subscribedAt

instance Prelude.NFData EventSubscription where
  rnf EventSubscription' {..} =
    Prelude.rnf event `Prelude.seq`
      Prelude.rnf subscribedAt

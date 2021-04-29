{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.EventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventSource where

import Network.AWS.CloudWatchEvents.Types.EventSourceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A partner event source is created by an SaaS partner. If a customer
-- creates a partner event bus that matches this event source, that AWS
-- account can receive events from the partner\'s applications or services.
--
-- /See:/ 'newEventSource' smart constructor.
data EventSource = EventSource'
  { -- | The date and time the event source was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the event source will expire, if the AWS account
    -- doesn\'t create a matching event bus for it.
    expirationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of the event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Prelude.Maybe EventSourceState,
    -- | The name of the event source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the partner that created the event source.
    createdBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'eventSource_creationTime' - The date and time the event source was created.
--
-- 'expirationTime', 'eventSource_expirationTime' - The date and time that the event source will expire, if the AWS account
-- doesn\'t create a matching event bus for it.
--
-- 'arn', 'eventSource_arn' - The ARN of the event source.
--
-- 'state', 'eventSource_state' - The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
--
-- 'name', 'eventSource_name' - The name of the event source.
--
-- 'createdBy', 'eventSource_createdBy' - The name of the partner that created the event source.
newEventSource ::
  EventSource
newEventSource =
  EventSource'
    { creationTime = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      createdBy = Prelude.Nothing
    }

-- | The date and time the event source was created.
eventSource_creationTime :: Lens.Lens' EventSource (Prelude.Maybe Prelude.UTCTime)
eventSource_creationTime = Lens.lens (\EventSource' {creationTime} -> creationTime) (\s@EventSource' {} a -> s {creationTime = a} :: EventSource) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the event source will expire, if the AWS account
-- doesn\'t create a matching event bus for it.
eventSource_expirationTime :: Lens.Lens' EventSource (Prelude.Maybe Prelude.UTCTime)
eventSource_expirationTime = Lens.lens (\EventSource' {expirationTime} -> expirationTime) (\s@EventSource' {} a -> s {expirationTime = a} :: EventSource) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of the event source.
eventSource_arn :: Lens.Lens' EventSource (Prelude.Maybe Prelude.Text)
eventSource_arn = Lens.lens (\EventSource' {arn} -> arn) (\s@EventSource' {} a -> s {arn = a} :: EventSource)

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
eventSource_state :: Lens.Lens' EventSource (Prelude.Maybe EventSourceState)
eventSource_state = Lens.lens (\EventSource' {state} -> state) (\s@EventSource' {} a -> s {state = a} :: EventSource)

-- | The name of the event source.
eventSource_name :: Lens.Lens' EventSource (Prelude.Maybe Prelude.Text)
eventSource_name = Lens.lens (\EventSource' {name} -> name) (\s@EventSource' {} a -> s {name = a} :: EventSource)

-- | The name of the partner that created the event source.
eventSource_createdBy :: Lens.Lens' EventSource (Prelude.Maybe Prelude.Text)
eventSource_createdBy = Lens.lens (\EventSource' {createdBy} -> createdBy) (\s@EventSource' {} a -> s {createdBy = a} :: EventSource)

instance Prelude.FromJSON EventSource where
  parseJSON =
    Prelude.withObject
      "EventSource"
      ( \x ->
          EventSource'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ExpirationTime")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "CreatedBy")
      )

instance Prelude.Hashable EventSource

instance Prelude.NFData EventSource

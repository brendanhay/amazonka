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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A partner event source is created by an SaaS partner. If a customer
-- creates a partner event bus that matches this event source, that AWS
-- account can receive events from the partner\'s applications or services.
--
-- /See:/ 'newEventSource' smart constructor.
data EventSource = EventSource'
  { -- | The date and time the event source was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The date and time that the event source will expire, if the AWS account
    -- doesn\'t create a matching event bus for it.
    expirationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the event source.
    arn :: Core.Maybe Core.Text,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Core.Maybe EventSourceState,
    -- | The name of the event source.
    name :: Core.Maybe Core.Text,
    -- | The name of the partner that created the event source.
    createdBy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { creationTime = Core.Nothing,
      expirationTime = Core.Nothing,
      arn = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      createdBy = Core.Nothing
    }

-- | The date and time the event source was created.
eventSource_creationTime :: Lens.Lens' EventSource (Core.Maybe Core.UTCTime)
eventSource_creationTime = Lens.lens (\EventSource' {creationTime} -> creationTime) (\s@EventSource' {} a -> s {creationTime = a} :: EventSource) Core.. Lens.mapping Core._Time

-- | The date and time that the event source will expire, if the AWS account
-- doesn\'t create a matching event bus for it.
eventSource_expirationTime :: Lens.Lens' EventSource (Core.Maybe Core.UTCTime)
eventSource_expirationTime = Lens.lens (\EventSource' {expirationTime} -> expirationTime) (\s@EventSource' {} a -> s {expirationTime = a} :: EventSource) Core.. Lens.mapping Core._Time

-- | The ARN of the event source.
eventSource_arn :: Lens.Lens' EventSource (Core.Maybe Core.Text)
eventSource_arn = Lens.lens (\EventSource' {arn} -> arn) (\s@EventSource' {} a -> s {arn = a} :: EventSource)

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
eventSource_state :: Lens.Lens' EventSource (Core.Maybe EventSourceState)
eventSource_state = Lens.lens (\EventSource' {state} -> state) (\s@EventSource' {} a -> s {state = a} :: EventSource)

-- | The name of the event source.
eventSource_name :: Lens.Lens' EventSource (Core.Maybe Core.Text)
eventSource_name = Lens.lens (\EventSource' {name} -> name) (\s@EventSource' {} a -> s {name = a} :: EventSource)

-- | The name of the partner that created the event source.
eventSource_createdBy :: Lens.Lens' EventSource (Core.Maybe Core.Text)
eventSource_createdBy = Lens.lens (\EventSource' {createdBy} -> createdBy) (\s@EventSource' {} a -> s {createdBy = a} :: EventSource)

instance Core.FromJSON EventSource where
  parseJSON =
    Core.withObject
      "EventSource"
      ( \x ->
          EventSource'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ExpirationTime")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "CreatedBy")
      )

instance Core.Hashable EventSource

instance Core.NFData EventSource

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
-- Module      : Amazonka.DataExchange.Types.EventActionEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.EventActionEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.Action
import Amazonka.DataExchange.Types.Event
import qualified Amazonka.Prelude as Prelude

-- | An event action is an object that defines the relationship between a
-- specific event and an automated action that will be taken on behalf of
-- the customer.
--
-- /See:/ 'newEventActionEntry' smart constructor.
data EventActionEntry = EventActionEntry'
  { -- | What occurs after a certain event.
    action :: Action,
    -- | The Amazon Resource Name (ARN) for the event action.
    arn :: Prelude.Text,
    -- | The date and time that the event action was created, in ISO 8601 format.
    createdAt :: Data.ISO8601,
    -- | What occurs to start an action.
    event :: Event,
    -- | The unique identifier for the event action.
    id :: Prelude.Text,
    -- | The date and time that the event action was last updated, in ISO 8601
    -- format.
    updatedAt :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventActionEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'eventActionEntry_action' - What occurs after a certain event.
--
-- 'arn', 'eventActionEntry_arn' - The Amazon Resource Name (ARN) for the event action.
--
-- 'createdAt', 'eventActionEntry_createdAt' - The date and time that the event action was created, in ISO 8601 format.
--
-- 'event', 'eventActionEntry_event' - What occurs to start an action.
--
-- 'id', 'eventActionEntry_id' - The unique identifier for the event action.
--
-- 'updatedAt', 'eventActionEntry_updatedAt' - The date and time that the event action was last updated, in ISO 8601
-- format.
newEventActionEntry ::
  -- | 'action'
  Action ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'event'
  Event ->
  -- | 'id'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  EventActionEntry
newEventActionEntry
  pAction_
  pArn_
  pCreatedAt_
  pEvent_
  pId_
  pUpdatedAt_ =
    EventActionEntry'
      { action = pAction_,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        event = pEvent_,
        id = pId_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | What occurs after a certain event.
eventActionEntry_action :: Lens.Lens' EventActionEntry Action
eventActionEntry_action = Lens.lens (\EventActionEntry' {action} -> action) (\s@EventActionEntry' {} a -> s {action = a} :: EventActionEntry)

-- | The Amazon Resource Name (ARN) for the event action.
eventActionEntry_arn :: Lens.Lens' EventActionEntry Prelude.Text
eventActionEntry_arn = Lens.lens (\EventActionEntry' {arn} -> arn) (\s@EventActionEntry' {} a -> s {arn = a} :: EventActionEntry)

-- | The date and time that the event action was created, in ISO 8601 format.
eventActionEntry_createdAt :: Lens.Lens' EventActionEntry Prelude.UTCTime
eventActionEntry_createdAt = Lens.lens (\EventActionEntry' {createdAt} -> createdAt) (\s@EventActionEntry' {} a -> s {createdAt = a} :: EventActionEntry) Prelude.. Data._Time

-- | What occurs to start an action.
eventActionEntry_event :: Lens.Lens' EventActionEntry Event
eventActionEntry_event = Lens.lens (\EventActionEntry' {event} -> event) (\s@EventActionEntry' {} a -> s {event = a} :: EventActionEntry)

-- | The unique identifier for the event action.
eventActionEntry_id :: Lens.Lens' EventActionEntry Prelude.Text
eventActionEntry_id = Lens.lens (\EventActionEntry' {id} -> id) (\s@EventActionEntry' {} a -> s {id = a} :: EventActionEntry)

-- | The date and time that the event action was last updated, in ISO 8601
-- format.
eventActionEntry_updatedAt :: Lens.Lens' EventActionEntry Prelude.UTCTime
eventActionEntry_updatedAt = Lens.lens (\EventActionEntry' {updatedAt} -> updatedAt) (\s@EventActionEntry' {} a -> s {updatedAt = a} :: EventActionEntry) Prelude.. Data._Time

instance Data.FromJSON EventActionEntry where
  parseJSON =
    Data.withObject
      "EventActionEntry"
      ( \x ->
          EventActionEntry'
            Prelude.<$> (x Data..: "Action")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "Event")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable EventActionEntry where
  hashWithSalt _salt EventActionEntry' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData EventActionEntry where
  rnf EventActionEntry' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updatedAt

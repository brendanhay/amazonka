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
-- Module      : Amazonka.Evidently.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.EventType
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the information about one evaluation event or
-- custom event sent to Evidently. This is a JSON payload. If this event
-- specifies a pre-defined event type, the payload must follow the defined
-- event schema.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The event data.
    data' :: Prelude.Text,
    -- | The timestamp of the event.
    timestamp :: Data.POSIX,
    -- | @aws.evidently.evaluation@ specifies an evaluation event, which
    -- determines which feature variation that a user sees.
    -- @aws.evidently.custom@ specifies a custom event, which generates metrics
    -- from user actions such as clicks and checkouts.
    type' :: EventType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'event_data' - The event data.
--
-- 'timestamp', 'event_timestamp' - The timestamp of the event.
--
-- 'type'', 'event_type' - @aws.evidently.evaluation@ specifies an evaluation event, which
-- determines which feature variation that a user sees.
-- @aws.evidently.custom@ specifies a custom event, which generates metrics
-- from user actions such as clicks and checkouts.
newEvent ::
  -- | 'data''
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'type''
  EventType ->
  Event
newEvent pData_ pTimestamp_ pType_ =
  Event'
    { data' = pData_,
      timestamp = Data._Time Lens.# pTimestamp_,
      type' = pType_
    }

-- | The event data.
event_data :: Lens.Lens' Event Prelude.Text
event_data = Lens.lens (\Event' {data'} -> data') (\s@Event' {} a -> s {data' = a} :: Event)

-- | The timestamp of the event.
event_timestamp :: Lens.Lens' Event Prelude.UTCTime
event_timestamp = Lens.lens (\Event' {timestamp} -> timestamp) (\s@Event' {} a -> s {timestamp = a} :: Event) Prelude.. Data._Time

-- | @aws.evidently.evaluation@ specifies an evaluation event, which
-- determines which feature variation that a user sees.
-- @aws.evidently.custom@ specifies a custom event, which generates metrics
-- from user actions such as clicks and checkouts.
event_type :: Lens.Lens' Event EventType
event_type = Lens.lens (\Event' {type'} -> type') (\s@Event' {} a -> s {type' = a} :: Event)

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Event where
  toJSON Event' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("data" Data..= data'),
            Prelude.Just ("timestamp" Data..= timestamp),
            Prelude.Just ("type" Data..= type')
          ]
      )

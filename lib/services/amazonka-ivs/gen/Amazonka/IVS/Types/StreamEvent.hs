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
-- Module      : Amazonka.IVS.Types.StreamEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a stream’s events. For a list of events, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/eventbridge.html Using Amazon EventBridge with Amazon IVS>.
--
-- /See:/ 'newStreamEvent' smart constructor.
data StreamEvent = StreamEvent'
  { -- | Time when the event occurred. This is an ISO 8601 timestamp; /note that
    -- this is returned as a string/.
    eventTime :: Prelude.Maybe Data.ISO8601,
    -- | Name that identifies the stream event within a @type@.
    name :: Prelude.Maybe Prelude.Text,
    -- | Logical group for certain events.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTime', 'streamEvent_eventTime' - Time when the event occurred. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
--
-- 'name', 'streamEvent_name' - Name that identifies the stream event within a @type@.
--
-- 'type'', 'streamEvent_type' - Logical group for certain events.
newStreamEvent ::
  StreamEvent
newStreamEvent =
  StreamEvent'
    { eventTime = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Time when the event occurred. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
streamEvent_eventTime :: Lens.Lens' StreamEvent (Prelude.Maybe Prelude.UTCTime)
streamEvent_eventTime = Lens.lens (\StreamEvent' {eventTime} -> eventTime) (\s@StreamEvent' {} a -> s {eventTime = a} :: StreamEvent) Prelude.. Lens.mapping Data._Time

-- | Name that identifies the stream event within a @type@.
streamEvent_name :: Lens.Lens' StreamEvent (Prelude.Maybe Prelude.Text)
streamEvent_name = Lens.lens (\StreamEvent' {name} -> name) (\s@StreamEvent' {} a -> s {name = a} :: StreamEvent)

-- | Logical group for certain events.
streamEvent_type :: Lens.Lens' StreamEvent (Prelude.Maybe Prelude.Text)
streamEvent_type = Lens.lens (\StreamEvent' {type'} -> type') (\s@StreamEvent' {} a -> s {type' = a} :: StreamEvent)

instance Data.FromJSON StreamEvent where
  parseJSON =
    Data.withObject
      "StreamEvent"
      ( \x ->
          StreamEvent'
            Prelude.<$> (x Data..:? "eventTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable StreamEvent where
  hashWithSalt _salt StreamEvent' {..} =
    _salt
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData StreamEvent where
  rnf StreamEvent' {..} =
    Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

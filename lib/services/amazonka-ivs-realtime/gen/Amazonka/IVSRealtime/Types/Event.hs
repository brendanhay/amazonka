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
-- Module      : Amazonka.IVSRealtime.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types.EventErrorCode
import Amazonka.IVSRealtime.Types.EventName
import qualified Amazonka.Prelude as Prelude

-- | An occurrence during a stage session.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | If the event is an error event, the error code is provided to give
    -- insight into the specific error that occurred. If the event is not an
    -- error event, this field is null. @INSUFFICIENT_CAPABILITIES@ indicates
    -- that the participant tried to take an action that the participant’s
    -- token is not allowed to do. For more information about participant
    -- capabilities, see the @capabilities@ field in CreateParticipantToken.
    errorCode :: Prelude.Maybe EventErrorCode,
    -- | ISO 8601 timestamp (returned as a string) for when the event occurred.
    eventTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the event.
    name :: Prelude.Maybe EventName,
    -- | Unique identifier for the participant who triggered the event. This is
    -- assigned by IVS.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for the remote participant. For a subscribe event,
    -- this is the publisher. For a publish or join event, this is null. This
    -- is assigned by IVS.
    remoteParticipantId :: Prelude.Maybe Prelude.Text
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
-- 'errorCode', 'event_errorCode' - If the event is an error event, the error code is provided to give
-- insight into the specific error that occurred. If the event is not an
-- error event, this field is null. @INSUFFICIENT_CAPABILITIES@ indicates
-- that the participant tried to take an action that the participant’s
-- token is not allowed to do. For more information about participant
-- capabilities, see the @capabilities@ field in CreateParticipantToken.
--
-- 'eventTime', 'event_eventTime' - ISO 8601 timestamp (returned as a string) for when the event occurred.
--
-- 'name', 'event_name' - The name of the event.
--
-- 'participantId', 'event_participantId' - Unique identifier for the participant who triggered the event. This is
-- assigned by IVS.
--
-- 'remoteParticipantId', 'event_remoteParticipantId' - Unique identifier for the remote participant. For a subscribe event,
-- this is the publisher. For a publish or join event, this is null. This
-- is assigned by IVS.
newEvent ::
  Event
newEvent =
  Event'
    { errorCode = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      name = Prelude.Nothing,
      participantId = Prelude.Nothing,
      remoteParticipantId = Prelude.Nothing
    }

-- | If the event is an error event, the error code is provided to give
-- insight into the specific error that occurred. If the event is not an
-- error event, this field is null. @INSUFFICIENT_CAPABILITIES@ indicates
-- that the participant tried to take an action that the participant’s
-- token is not allowed to do. For more information about participant
-- capabilities, see the @capabilities@ field in CreateParticipantToken.
event_errorCode :: Lens.Lens' Event (Prelude.Maybe EventErrorCode)
event_errorCode = Lens.lens (\Event' {errorCode} -> errorCode) (\s@Event' {} a -> s {errorCode = a} :: Event)

-- | ISO 8601 timestamp (returned as a string) for when the event occurred.
event_eventTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_eventTime = Lens.lens (\Event' {eventTime} -> eventTime) (\s@Event' {} a -> s {eventTime = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | The name of the event.
event_name :: Lens.Lens' Event (Prelude.Maybe EventName)
event_name = Lens.lens (\Event' {name} -> name) (\s@Event' {} a -> s {name = a} :: Event)

-- | Unique identifier for the participant who triggered the event. This is
-- assigned by IVS.
event_participantId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_participantId = Lens.lens (\Event' {participantId} -> participantId) (\s@Event' {} a -> s {participantId = a} :: Event)

-- | Unique identifier for the remote participant. For a subscribe event,
-- this is the publisher. For a publish or join event, this is null. This
-- is assigned by IVS.
event_remoteParticipantId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_remoteParticipantId = Lens.lens (\Event' {remoteParticipantId} -> remoteParticipantId) (\s@Event' {} a -> s {remoteParticipantId = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "eventTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "participantId")
            Prelude.<*> (x Data..:? "remoteParticipantId")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` remoteParticipantId

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf remoteParticipantId

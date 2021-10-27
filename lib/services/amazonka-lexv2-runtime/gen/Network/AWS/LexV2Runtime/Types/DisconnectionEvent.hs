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
-- Module      : Network.AWS.LexV2Runtime.Types.DisconnectionEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.DisconnectionEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A notification from the client that it is disconnecting from Amazon Lex
-- V2. Sending a @DisconnectionEvent@ event is optional, but can help
-- identify a conversation in logs.
--
-- /See:/ 'newDisconnectionEvent' smart constructor.
data DisconnectionEvent = DisconnectionEvent'
  { -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectionEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientTimestampMillis', 'disconnectionEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'disconnectionEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
newDisconnectionEvent ::
  DisconnectionEvent
newDisconnectionEvent =
  DisconnectionEvent'
    { clientTimestampMillis =
        Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
disconnectionEvent_clientTimestampMillis :: Lens.Lens' DisconnectionEvent (Prelude.Maybe Prelude.Integer)
disconnectionEvent_clientTimestampMillis = Lens.lens (\DisconnectionEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@DisconnectionEvent' {} a -> s {clientTimestampMillis = a} :: DisconnectionEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
disconnectionEvent_eventId :: Lens.Lens' DisconnectionEvent (Prelude.Maybe Prelude.Text)
disconnectionEvent_eventId = Lens.lens (\DisconnectionEvent' {eventId} -> eventId) (\s@DisconnectionEvent' {} a -> s {eventId = a} :: DisconnectionEvent)

instance Prelude.Hashable DisconnectionEvent

instance Prelude.NFData DisconnectionEvent

instance Core.ToJSON DisconnectionEvent where
  toJSON DisconnectionEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId
          ]
      )

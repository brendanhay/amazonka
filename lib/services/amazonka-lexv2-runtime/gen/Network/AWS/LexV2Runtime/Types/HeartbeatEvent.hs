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
-- Module      : Network.AWS.LexV2Runtime.Types.HeartbeatEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.HeartbeatEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Event that Amazon Lex V2 sends to indicate that the stream is still open
-- between the client application and Amazon Lex V2
--
-- /See:/ 'newHeartbeatEvent' smart constructor.
data HeartbeatEvent = HeartbeatEvent'
  { -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeartbeatEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'heartbeatEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newHeartbeatEvent ::
  HeartbeatEvent
newHeartbeatEvent =
  HeartbeatEvent' {eventId = Prelude.Nothing}

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
heartbeatEvent_eventId :: Lens.Lens' HeartbeatEvent (Prelude.Maybe Prelude.Text)
heartbeatEvent_eventId = Lens.lens (\HeartbeatEvent' {eventId} -> eventId) (\s@HeartbeatEvent' {} a -> s {eventId = a} :: HeartbeatEvent)

instance Core.FromJSON HeartbeatEvent where
  parseJSON =
    Core.withObject
      "HeartbeatEvent"
      ( \x ->
          HeartbeatEvent' Prelude.<$> (x Core..:? "eventId")
      )

instance Prelude.Hashable HeartbeatEvent

instance Prelude.NFData HeartbeatEvent

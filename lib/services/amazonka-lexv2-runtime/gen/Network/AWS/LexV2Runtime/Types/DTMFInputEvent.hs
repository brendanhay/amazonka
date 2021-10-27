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
-- Module      : Network.AWS.LexV2Runtime.Types.DTMFInputEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.DTMFInputEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A DTMF character sent from the client application. DTMF characters are
-- typically sent from a phone keypad to represent numbers. For example,
-- you can have Amazon Lex V2 process a credit card number input from a
-- phone.
--
-- /See:/ 'newDTMFInputEvent' smart constructor.
data DTMFInputEvent = DTMFInputEvent'
  { -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The DTMF character that the user pressed. The allowed characters are A -
    -- D, 0 - 9, # and *.
    inputCharacter :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DTMFInputEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientTimestampMillis', 'dTMFInputEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'dTMFInputEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
--
-- 'inputCharacter', 'dTMFInputEvent_inputCharacter' - The DTMF character that the user pressed. The allowed characters are A -
-- D, 0 - 9, # and *.
newDTMFInputEvent ::
  -- | 'inputCharacter'
  Prelude.Text ->
  DTMFInputEvent
newDTMFInputEvent pInputCharacter_ =
  DTMFInputEvent'
    { clientTimestampMillis =
        Prelude.Nothing,
      eventId = Prelude.Nothing,
      inputCharacter =
        Core._Sensitive Lens.# pInputCharacter_
    }

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
dTMFInputEvent_clientTimestampMillis :: Lens.Lens' DTMFInputEvent (Prelude.Maybe Prelude.Integer)
dTMFInputEvent_clientTimestampMillis = Lens.lens (\DTMFInputEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@DTMFInputEvent' {} a -> s {clientTimestampMillis = a} :: DTMFInputEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
dTMFInputEvent_eventId :: Lens.Lens' DTMFInputEvent (Prelude.Maybe Prelude.Text)
dTMFInputEvent_eventId = Lens.lens (\DTMFInputEvent' {eventId} -> eventId) (\s@DTMFInputEvent' {} a -> s {eventId = a} :: DTMFInputEvent)

-- | The DTMF character that the user pressed. The allowed characters are A -
-- D, 0 - 9, # and *.
dTMFInputEvent_inputCharacter :: Lens.Lens' DTMFInputEvent Prelude.Text
dTMFInputEvent_inputCharacter = Lens.lens (\DTMFInputEvent' {inputCharacter} -> inputCharacter) (\s@DTMFInputEvent' {} a -> s {inputCharacter = a} :: DTMFInputEvent) Prelude.. Core._Sensitive

instance Prelude.Hashable DTMFInputEvent

instance Prelude.NFData DTMFInputEvent

instance Core.ToJSON DTMFInputEvent where
  toJSON DTMFInputEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId,
            Prelude.Just
              ("inputCharacter" Core..= inputCharacter)
          ]
      )

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
-- Module      : Network.AWS.LexV2Runtime.Types.TextInputEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.TextInputEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The event sent from your client application to Amazon Lex V2 with text
-- input from the user.
--
-- /See:/ 'newTextInputEvent' smart constructor.
data TextInputEvent = TextInputEvent'
  { -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The text from the user. Amazon Lex V2 processes this as a complete
    -- statement.
    text :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextInputEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientTimestampMillis', 'textInputEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'textInputEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
--
-- 'text', 'textInputEvent_text' - The text from the user. Amazon Lex V2 processes this as a complete
-- statement.
newTextInputEvent ::
  -- | 'text'
  Prelude.Text ->
  TextInputEvent
newTextInputEvent pText_ =
  TextInputEvent'
    { clientTimestampMillis =
        Prelude.Nothing,
      eventId = Prelude.Nothing,
      text = Core._Sensitive Lens.# pText_
    }

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
textInputEvent_clientTimestampMillis :: Lens.Lens' TextInputEvent (Prelude.Maybe Prelude.Integer)
textInputEvent_clientTimestampMillis = Lens.lens (\TextInputEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@TextInputEvent' {} a -> s {clientTimestampMillis = a} :: TextInputEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
textInputEvent_eventId :: Lens.Lens' TextInputEvent (Prelude.Maybe Prelude.Text)
textInputEvent_eventId = Lens.lens (\TextInputEvent' {eventId} -> eventId) (\s@TextInputEvent' {} a -> s {eventId = a} :: TextInputEvent)

-- | The text from the user. Amazon Lex V2 processes this as a complete
-- statement.
textInputEvent_text :: Lens.Lens' TextInputEvent Prelude.Text
textInputEvent_text = Lens.lens (\TextInputEvent' {text} -> text) (\s@TextInputEvent' {} a -> s {text = a} :: TextInputEvent) Prelude.. Core._Sensitive

instance Prelude.Hashable TextInputEvent

instance Prelude.NFData TextInputEvent

instance Core.ToJSON TextInputEvent where
  toJSON TextInputEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId,
            Prelude.Just ("text" Core..= text)
          ]
      )

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
-- Module      : Network.AWS.LexV2Runtime.Types.IntentResultEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.IntentResultEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.InputMode
import Network.AWS.LexV2Runtime.Types.Interpretation
import Network.AWS.LexV2Runtime.Types.SessionState
import qualified Network.AWS.Prelude as Prelude

-- | Contains the current state of the conversation between the client
-- application and Amazon Lex V2.
--
-- /See:/ 'newIntentResultEvent' smart constructor.
data IntentResultEvent = IntentResultEvent'
  { sessionState :: Prelude.Maybe SessionState,
    -- | Indicates whether the input to the operation was text or speech.
    inputMode :: Prelude.Maybe InputMode,
    -- | The identifier of the session in use.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The attributes sent in the request.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of intents that Amazon Lex V2 determined might satisfy the
    -- user\'s utterance.
    --
    -- Each interpretation includes the intent, a score that indicates how
    -- confident Amazon Lex V2 is that the interpretation is the correct one,
    -- and an optional sentiment response that indicates the sentiment
    -- expressed in the utterance.
    interpretations :: Prelude.Maybe [Interpretation],
    -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentResultEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'intentResultEvent_sessionState' - Undocumented member.
--
-- 'inputMode', 'intentResultEvent_inputMode' - Indicates whether the input to the operation was text or speech.
--
-- 'sessionId', 'intentResultEvent_sessionId' - The identifier of the session in use.
--
-- 'requestAttributes', 'intentResultEvent_requestAttributes' - The attributes sent in the request.
--
-- 'interpretations', 'intentResultEvent_interpretations' - A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
--
-- 'eventId', 'intentResultEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newIntentResultEvent ::
  IntentResultEvent
newIntentResultEvent =
  IntentResultEvent'
    { sessionState = Prelude.Nothing,
      inputMode = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      requestAttributes = Prelude.Nothing,
      interpretations = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | Undocumented member.
intentResultEvent_sessionState :: Lens.Lens' IntentResultEvent (Prelude.Maybe SessionState)
intentResultEvent_sessionState = Lens.lens (\IntentResultEvent' {sessionState} -> sessionState) (\s@IntentResultEvent' {} a -> s {sessionState = a} :: IntentResultEvent)

-- | Indicates whether the input to the operation was text or speech.
intentResultEvent_inputMode :: Lens.Lens' IntentResultEvent (Prelude.Maybe InputMode)
intentResultEvent_inputMode = Lens.lens (\IntentResultEvent' {inputMode} -> inputMode) (\s@IntentResultEvent' {} a -> s {inputMode = a} :: IntentResultEvent)

-- | The identifier of the session in use.
intentResultEvent_sessionId :: Lens.Lens' IntentResultEvent (Prelude.Maybe Prelude.Text)
intentResultEvent_sessionId = Lens.lens (\IntentResultEvent' {sessionId} -> sessionId) (\s@IntentResultEvent' {} a -> s {sessionId = a} :: IntentResultEvent)

-- | The attributes sent in the request.
intentResultEvent_requestAttributes :: Lens.Lens' IntentResultEvent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
intentResultEvent_requestAttributes = Lens.lens (\IntentResultEvent' {requestAttributes} -> requestAttributes) (\s@IntentResultEvent' {} a -> s {requestAttributes = a} :: IntentResultEvent) Prelude.. Lens.mapping Lens.coerced

-- | A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
intentResultEvent_interpretations :: Lens.Lens' IntentResultEvent (Prelude.Maybe [Interpretation])
intentResultEvent_interpretations = Lens.lens (\IntentResultEvent' {interpretations} -> interpretations) (\s@IntentResultEvent' {} a -> s {interpretations = a} :: IntentResultEvent) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
intentResultEvent_eventId :: Lens.Lens' IntentResultEvent (Prelude.Maybe Prelude.Text)
intentResultEvent_eventId = Lens.lens (\IntentResultEvent' {eventId} -> eventId) (\s@IntentResultEvent' {} a -> s {eventId = a} :: IntentResultEvent)

instance Core.FromJSON IntentResultEvent where
  parseJSON =
    Core.withObject
      "IntentResultEvent"
      ( \x ->
          IntentResultEvent'
            Prelude.<$> (x Core..:? "sessionState")
            Prelude.<*> (x Core..:? "inputMode")
            Prelude.<*> (x Core..:? "sessionId")
            Prelude.<*> ( x Core..:? "requestAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "interpretations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable IntentResultEvent

instance Prelude.NFData IntentResultEvent

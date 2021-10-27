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
-- Module      : Network.AWS.LexV2Runtime.Types.SessionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.SessionState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ActiveContext
import Network.AWS.LexV2Runtime.Types.DialogAction
import Network.AWS.LexV2Runtime.Types.Intent
import qualified Network.AWS.Prelude as Prelude

-- | The state of the user\'s session with Amazon Lex V2.
--
-- /See:/ 'newSessionState' smart constructor.
data SessionState = SessionState'
  { originatingRequestId :: Prelude.Maybe Prelude.Text,
    -- | The active intent that Amazon Lex V2 is processing.
    intent :: Prelude.Maybe Intent,
    -- | One or more contexts that indicate to Amazon Lex V2 the context of a
    -- request. When a context is active, Amazon Lex V2 considers intents with
    -- the matching context as a trigger as the next intent in a session.
    activeContexts :: Prelude.Maybe [ActiveContext],
    -- | The next step that Amazon Lex V2 should take in the conversation with a
    -- user.
    dialogAction :: Prelude.Maybe DialogAction,
    -- | Map of key\/value pairs representing session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex V2 and a client application.
    sessionAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originatingRequestId', 'sessionState_originatingRequestId' -
--
-- 'intent', 'sessionState_intent' - The active intent that Amazon Lex V2 is processing.
--
-- 'activeContexts', 'sessionState_activeContexts' - One or more contexts that indicate to Amazon Lex V2 the context of a
-- request. When a context is active, Amazon Lex V2 considers intents with
-- the matching context as a trigger as the next intent in a session.
--
-- 'dialogAction', 'sessionState_dialogAction' - The next step that Amazon Lex V2 should take in the conversation with a
-- user.
--
-- 'sessionAttributes', 'sessionState_sessionAttributes' - Map of key\/value pairs representing session-specific context
-- information. It contains application information passed between Amazon
-- Lex V2 and a client application.
newSessionState ::
  SessionState
newSessionState =
  SessionState'
    { originatingRequestId =
        Prelude.Nothing,
      intent = Prelude.Nothing,
      activeContexts = Prelude.Nothing,
      dialogAction = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing
    }

-- |
sessionState_originatingRequestId :: Lens.Lens' SessionState (Prelude.Maybe Prelude.Text)
sessionState_originatingRequestId = Lens.lens (\SessionState' {originatingRequestId} -> originatingRequestId) (\s@SessionState' {} a -> s {originatingRequestId = a} :: SessionState)

-- | The active intent that Amazon Lex V2 is processing.
sessionState_intent :: Lens.Lens' SessionState (Prelude.Maybe Intent)
sessionState_intent = Lens.lens (\SessionState' {intent} -> intent) (\s@SessionState' {} a -> s {intent = a} :: SessionState)

-- | One or more contexts that indicate to Amazon Lex V2 the context of a
-- request. When a context is active, Amazon Lex V2 considers intents with
-- the matching context as a trigger as the next intent in a session.
sessionState_activeContexts :: Lens.Lens' SessionState (Prelude.Maybe [ActiveContext])
sessionState_activeContexts = Lens.lens (\SessionState' {activeContexts} -> activeContexts) (\s@SessionState' {} a -> s {activeContexts = a} :: SessionState) Prelude.. Lens.mapping Lens.coerced

-- | The next step that Amazon Lex V2 should take in the conversation with a
-- user.
sessionState_dialogAction :: Lens.Lens' SessionState (Prelude.Maybe DialogAction)
sessionState_dialogAction = Lens.lens (\SessionState' {dialogAction} -> dialogAction) (\s@SessionState' {} a -> s {dialogAction = a} :: SessionState)

-- | Map of key\/value pairs representing session-specific context
-- information. It contains application information passed between Amazon
-- Lex V2 and a client application.
sessionState_sessionAttributes :: Lens.Lens' SessionState (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sessionState_sessionAttributes = Lens.lens (\SessionState' {sessionAttributes} -> sessionAttributes) (\s@SessionState' {} a -> s {sessionAttributes = a} :: SessionState) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SessionState where
  parseJSON =
    Core.withObject
      "SessionState"
      ( \x ->
          SessionState'
            Prelude.<$> (x Core..:? "originatingRequestId")
            Prelude.<*> (x Core..:? "intent")
            Prelude.<*> (x Core..:? "activeContexts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "dialogAction")
            Prelude.<*> ( x Core..:? "sessionAttributes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SessionState

instance Prelude.NFData SessionState

instance Core.ToJSON SessionState where
  toJSON SessionState' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("originatingRequestId" Core..=)
              Prelude.<$> originatingRequestId,
            ("intent" Core..=) Prelude.<$> intent,
            ("activeContexts" Core..=)
              Prelude.<$> activeContexts,
            ("dialogAction" Core..=) Prelude.<$> dialogAction,
            ("sessionAttributes" Core..=)
              Prelude.<$> sessionAttributes
          ]
      )

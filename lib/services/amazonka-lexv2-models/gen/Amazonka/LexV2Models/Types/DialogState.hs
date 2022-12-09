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
-- Module      : Amazonka.LexV2Models.Types.DialogState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DialogState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.DialogAction
import Amazonka.LexV2Models.Types.IntentOverride
import qualified Amazonka.Prelude as Prelude

-- | The current state of the conversation with the user.
--
-- /See:/ 'newDialogState' smart constructor.
data DialogState = DialogState'
  { dialogAction :: Prelude.Maybe DialogAction,
    intent :: Prelude.Maybe IntentOverride,
    -- | Map of key\/value pairs representing session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex and a client application.
    sessionAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialogState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialogAction', 'dialogState_dialogAction' - Undocumented member.
--
-- 'intent', 'dialogState_intent' - Undocumented member.
--
-- 'sessionAttributes', 'dialogState_sessionAttributes' - Map of key\/value pairs representing session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
newDialogState ::
  DialogState
newDialogState =
  DialogState'
    { dialogAction = Prelude.Nothing,
      intent = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing
    }

-- | Undocumented member.
dialogState_dialogAction :: Lens.Lens' DialogState (Prelude.Maybe DialogAction)
dialogState_dialogAction = Lens.lens (\DialogState' {dialogAction} -> dialogAction) (\s@DialogState' {} a -> s {dialogAction = a} :: DialogState)

-- | Undocumented member.
dialogState_intent :: Lens.Lens' DialogState (Prelude.Maybe IntentOverride)
dialogState_intent = Lens.lens (\DialogState' {intent} -> intent) (\s@DialogState' {} a -> s {intent = a} :: DialogState)

-- | Map of key\/value pairs representing session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
dialogState_sessionAttributes :: Lens.Lens' DialogState (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dialogState_sessionAttributes = Lens.lens (\DialogState' {sessionAttributes} -> sessionAttributes) (\s@DialogState' {} a -> s {sessionAttributes = a} :: DialogState) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DialogState where
  parseJSON =
    Data.withObject
      "DialogState"
      ( \x ->
          DialogState'
            Prelude.<$> (x Data..:? "dialogAction")
            Prelude.<*> (x Data..:? "intent")
            Prelude.<*> ( x Data..:? "sessionAttributes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DialogState where
  hashWithSalt _salt DialogState' {..} =
    _salt `Prelude.hashWithSalt` dialogAction
      `Prelude.hashWithSalt` intent
      `Prelude.hashWithSalt` sessionAttributes

instance Prelude.NFData DialogState where
  rnf DialogState' {..} =
    Prelude.rnf dialogAction
      `Prelude.seq` Prelude.rnf intent
      `Prelude.seq` Prelude.rnf sessionAttributes

instance Data.ToJSON DialogState where
  toJSON DialogState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dialogAction" Data..=) Prelude.<$> dialogAction,
            ("intent" Data..=) Prelude.<$> intent,
            ("sessionAttributes" Data..=)
              Prelude.<$> sessionAttributes
          ]
      )

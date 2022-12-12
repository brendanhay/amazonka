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
-- Module      : Amazonka.LexV2Models.Types.IntentClosingSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentClosingSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides a statement the Amazon Lex conveys to the user when the intent
-- is successfully fulfilled.
--
-- /See:/ 'newIntentClosingSetting' smart constructor.
data IntentClosingSetting = IntentClosingSetting'
  { -- | Specifies whether an intent\'s closing response is used. When this field
    -- is false, the closing response isn\'t sent to the user. If the @active@
    -- field isn\'t specified, the default is true.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The response that Amazon Lex sends to the user when the intent is
    -- complete.
    closingResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches associated with the intent\'s closing
    -- response. These branches are executed when the @nextStep@ attribute is
    -- set to @EvalutateConditional@.
    conditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step that the bot executes after playing the
    -- intent\'s closing response.
    nextStep :: Prelude.Maybe DialogState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentClosingSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'intentClosingSetting_active' - Specifies whether an intent\'s closing response is used. When this field
-- is false, the closing response isn\'t sent to the user. If the @active@
-- field isn\'t specified, the default is true.
--
-- 'closingResponse', 'intentClosingSetting_closingResponse' - The response that Amazon Lex sends to the user when the intent is
-- complete.
--
-- 'conditional', 'intentClosingSetting_conditional' - A list of conditional branches associated with the intent\'s closing
-- response. These branches are executed when the @nextStep@ attribute is
-- set to @EvalutateConditional@.
--
-- 'nextStep', 'intentClosingSetting_nextStep' - Specifies the next step that the bot executes after playing the
-- intent\'s closing response.
newIntentClosingSetting ::
  IntentClosingSetting
newIntentClosingSetting =
  IntentClosingSetting'
    { active = Prelude.Nothing,
      closingResponse = Prelude.Nothing,
      conditional = Prelude.Nothing,
      nextStep = Prelude.Nothing
    }

-- | Specifies whether an intent\'s closing response is used. When this field
-- is false, the closing response isn\'t sent to the user. If the @active@
-- field isn\'t specified, the default is true.
intentClosingSetting_active :: Lens.Lens' IntentClosingSetting (Prelude.Maybe Prelude.Bool)
intentClosingSetting_active = Lens.lens (\IntentClosingSetting' {active} -> active) (\s@IntentClosingSetting' {} a -> s {active = a} :: IntentClosingSetting)

-- | The response that Amazon Lex sends to the user when the intent is
-- complete.
intentClosingSetting_closingResponse :: Lens.Lens' IntentClosingSetting (Prelude.Maybe ResponseSpecification)
intentClosingSetting_closingResponse = Lens.lens (\IntentClosingSetting' {closingResponse} -> closingResponse) (\s@IntentClosingSetting' {} a -> s {closingResponse = a} :: IntentClosingSetting)

-- | A list of conditional branches associated with the intent\'s closing
-- response. These branches are executed when the @nextStep@ attribute is
-- set to @EvalutateConditional@.
intentClosingSetting_conditional :: Lens.Lens' IntentClosingSetting (Prelude.Maybe ConditionalSpecification)
intentClosingSetting_conditional = Lens.lens (\IntentClosingSetting' {conditional} -> conditional) (\s@IntentClosingSetting' {} a -> s {conditional = a} :: IntentClosingSetting)

-- | Specifies the next step that the bot executes after playing the
-- intent\'s closing response.
intentClosingSetting_nextStep :: Lens.Lens' IntentClosingSetting (Prelude.Maybe DialogState)
intentClosingSetting_nextStep = Lens.lens (\IntentClosingSetting' {nextStep} -> nextStep) (\s@IntentClosingSetting' {} a -> s {nextStep = a} :: IntentClosingSetting)

instance Data.FromJSON IntentClosingSetting where
  parseJSON =
    Data.withObject
      "IntentClosingSetting"
      ( \x ->
          IntentClosingSetting'
            Prelude.<$> (x Data..:? "active")
            Prelude.<*> (x Data..:? "closingResponse")
            Prelude.<*> (x Data..:? "conditional")
            Prelude.<*> (x Data..:? "nextStep")
      )

instance Prelude.Hashable IntentClosingSetting where
  hashWithSalt _salt IntentClosingSetting' {..} =
    _salt `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` closingResponse
      `Prelude.hashWithSalt` conditional
      `Prelude.hashWithSalt` nextStep

instance Prelude.NFData IntentClosingSetting where
  rnf IntentClosingSetting' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf closingResponse
      `Prelude.seq` Prelude.rnf conditional
      `Prelude.seq` Prelude.rnf nextStep

instance Data.ToJSON IntentClosingSetting where
  toJSON IntentClosingSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("active" Data..=) Prelude.<$> active,
            ("closingResponse" Data..=)
              Prelude.<$> closingResponse,
            ("conditional" Data..=) Prelude.<$> conditional,
            ("nextStep" Data..=) Prelude.<$> nextStep
          ]
      )

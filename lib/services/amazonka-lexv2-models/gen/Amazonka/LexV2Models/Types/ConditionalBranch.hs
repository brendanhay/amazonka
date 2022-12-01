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
-- Module      : Amazonka.LexV2Models.Types.ConditionalBranch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConditionalBranch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.Condition
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | A set of actions that Amazon Lex should run if the condition is matched.
--
-- /See:/ 'newConditionalBranch' smart constructor.
data ConditionalBranch = ConditionalBranch'
  { response :: Prelude.Maybe ResponseSpecification,
    -- | The name of the branch.
    name :: Prelude.Text,
    -- | Contains the expression to evaluate. If the condition is true, the
    -- branch\'s actions are taken.
    condition :: Condition,
    -- | The next step in the conversation.
    nextStep :: DialogState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'response', 'conditionalBranch_response' - Undocumented member.
--
-- 'name', 'conditionalBranch_name' - The name of the branch.
--
-- 'condition', 'conditionalBranch_condition' - Contains the expression to evaluate. If the condition is true, the
-- branch\'s actions are taken.
--
-- 'nextStep', 'conditionalBranch_nextStep' - The next step in the conversation.
newConditionalBranch ::
  -- | 'name'
  Prelude.Text ->
  -- | 'condition'
  Condition ->
  -- | 'nextStep'
  DialogState ->
  ConditionalBranch
newConditionalBranch pName_ pCondition_ pNextStep_ =
  ConditionalBranch'
    { response = Prelude.Nothing,
      name = pName_,
      condition = pCondition_,
      nextStep = pNextStep_
    }

-- | Undocumented member.
conditionalBranch_response :: Lens.Lens' ConditionalBranch (Prelude.Maybe ResponseSpecification)
conditionalBranch_response = Lens.lens (\ConditionalBranch' {response} -> response) (\s@ConditionalBranch' {} a -> s {response = a} :: ConditionalBranch)

-- | The name of the branch.
conditionalBranch_name :: Lens.Lens' ConditionalBranch Prelude.Text
conditionalBranch_name = Lens.lens (\ConditionalBranch' {name} -> name) (\s@ConditionalBranch' {} a -> s {name = a} :: ConditionalBranch)

-- | Contains the expression to evaluate. If the condition is true, the
-- branch\'s actions are taken.
conditionalBranch_condition :: Lens.Lens' ConditionalBranch Condition
conditionalBranch_condition = Lens.lens (\ConditionalBranch' {condition} -> condition) (\s@ConditionalBranch' {} a -> s {condition = a} :: ConditionalBranch)

-- | The next step in the conversation.
conditionalBranch_nextStep :: Lens.Lens' ConditionalBranch DialogState
conditionalBranch_nextStep = Lens.lens (\ConditionalBranch' {nextStep} -> nextStep) (\s@ConditionalBranch' {} a -> s {nextStep = a} :: ConditionalBranch)

instance Core.FromJSON ConditionalBranch where
  parseJSON =
    Core.withObject
      "ConditionalBranch"
      ( \x ->
          ConditionalBranch'
            Prelude.<$> (x Core..:? "response")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "condition")
            Prelude.<*> (x Core..: "nextStep")
      )

instance Prelude.Hashable ConditionalBranch where
  hashWithSalt _salt ConditionalBranch' {..} =
    _salt `Prelude.hashWithSalt` response
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` nextStep

instance Prelude.NFData ConditionalBranch where
  rnf ConditionalBranch' {..} =
    Prelude.rnf response
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf nextStep

instance Core.ToJSON ConditionalBranch where
  toJSON ConditionalBranch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("response" Core..=) Prelude.<$> response,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("condition" Core..= condition),
            Prelude.Just ("nextStep" Core..= nextStep)
          ]
      )

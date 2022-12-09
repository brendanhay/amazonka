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
-- Module      : Amazonka.LexV2Models.Types.DefaultConditionalBranch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DefaultConditionalBranch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | A set of actions that Amazon Lex should run if none of the other
-- conditions are met.
--
-- /See:/ 'newDefaultConditionalBranch' smart constructor.
data DefaultConditionalBranch = DefaultConditionalBranch'
  { -- | The next step in the conversation.
    nextStep :: Prelude.Maybe DialogState,
    response :: Prelude.Maybe ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultConditionalBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextStep', 'defaultConditionalBranch_nextStep' - The next step in the conversation.
--
-- 'response', 'defaultConditionalBranch_response' - Undocumented member.
newDefaultConditionalBranch ::
  DefaultConditionalBranch
newDefaultConditionalBranch =
  DefaultConditionalBranch'
    { nextStep =
        Prelude.Nothing,
      response = Prelude.Nothing
    }

-- | The next step in the conversation.
defaultConditionalBranch_nextStep :: Lens.Lens' DefaultConditionalBranch (Prelude.Maybe DialogState)
defaultConditionalBranch_nextStep = Lens.lens (\DefaultConditionalBranch' {nextStep} -> nextStep) (\s@DefaultConditionalBranch' {} a -> s {nextStep = a} :: DefaultConditionalBranch)

-- | Undocumented member.
defaultConditionalBranch_response :: Lens.Lens' DefaultConditionalBranch (Prelude.Maybe ResponseSpecification)
defaultConditionalBranch_response = Lens.lens (\DefaultConditionalBranch' {response} -> response) (\s@DefaultConditionalBranch' {} a -> s {response = a} :: DefaultConditionalBranch)

instance Data.FromJSON DefaultConditionalBranch where
  parseJSON =
    Data.withObject
      "DefaultConditionalBranch"
      ( \x ->
          DefaultConditionalBranch'
            Prelude.<$> (x Data..:? "nextStep")
            Prelude.<*> (x Data..:? "response")
      )

instance Prelude.Hashable DefaultConditionalBranch where
  hashWithSalt _salt DefaultConditionalBranch' {..} =
    _salt `Prelude.hashWithSalt` nextStep
      `Prelude.hashWithSalt` response

instance Prelude.NFData DefaultConditionalBranch where
  rnf DefaultConditionalBranch' {..} =
    Prelude.rnf nextStep
      `Prelude.seq` Prelude.rnf response

instance Data.ToJSON DefaultConditionalBranch where
  toJSON DefaultConditionalBranch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextStep" Data..=) Prelude.<$> nextStep,
            ("response" Data..=) Prelude.<$> response
          ]
      )

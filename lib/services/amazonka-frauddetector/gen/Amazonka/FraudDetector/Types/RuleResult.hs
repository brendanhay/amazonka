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
-- Module      : Amazonka.FraudDetector.Types.RuleResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.RuleResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The rule results.
--
-- /See:/ 'newRuleResult' smart constructor.
data RuleResult = RuleResult'
  { -- | The outcomes of the matched rule, based on the rule execution mode.
    outcomes :: Prelude.Maybe [Prelude.Text],
    -- | The rule ID that was matched, based on the rule execution mode.
    ruleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outcomes', 'ruleResult_outcomes' - The outcomes of the matched rule, based on the rule execution mode.
--
-- 'ruleId', 'ruleResult_ruleId' - The rule ID that was matched, based on the rule execution mode.
newRuleResult ::
  RuleResult
newRuleResult =
  RuleResult'
    { outcomes = Prelude.Nothing,
      ruleId = Prelude.Nothing
    }

-- | The outcomes of the matched rule, based on the rule execution mode.
ruleResult_outcomes :: Lens.Lens' RuleResult (Prelude.Maybe [Prelude.Text])
ruleResult_outcomes = Lens.lens (\RuleResult' {outcomes} -> outcomes) (\s@RuleResult' {} a -> s {outcomes = a} :: RuleResult) Prelude.. Lens.mapping Lens.coerced

-- | The rule ID that was matched, based on the rule execution mode.
ruleResult_ruleId :: Lens.Lens' RuleResult (Prelude.Maybe Prelude.Text)
ruleResult_ruleId = Lens.lens (\RuleResult' {ruleId} -> ruleId) (\s@RuleResult' {} a -> s {ruleId = a} :: RuleResult)

instance Data.FromJSON RuleResult where
  parseJSON =
    Data.withObject
      "RuleResult"
      ( \x ->
          RuleResult'
            Prelude.<$> (x Data..:? "outcomes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ruleId")
      )

instance Prelude.Hashable RuleResult where
  hashWithSalt _salt RuleResult' {..} =
    _salt
      `Prelude.hashWithSalt` outcomes
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData RuleResult where
  rnf RuleResult' {..} =
    Prelude.rnf outcomes `Prelude.seq`
      Prelude.rnf ruleId

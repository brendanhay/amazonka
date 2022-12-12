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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSource
import Amazonka.SecurityHub.Types.RuleGroupVariables

-- | Details about the rule group.
--
-- /See:/ 'newRuleGroupDetails' smart constructor.
data RuleGroupDetails = RuleGroupDetails'
  { -- | Additional settings to use in the specified rules.
    ruleVariables :: Prelude.Maybe RuleGroupVariables,
    -- | The rules and actions for the rule group.
    --
    -- For stateful rule groups, can contain @RulesString@, @RulesSourceList@,
    -- or @StatefulRules@.
    --
    -- For stateless rule groups, contains @StatelessRulesAndCustomActions@.
    rulesSource :: Prelude.Maybe RuleGroupSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleVariables', 'ruleGroupDetails_ruleVariables' - Additional settings to use in the specified rules.
--
-- 'rulesSource', 'ruleGroupDetails_rulesSource' - The rules and actions for the rule group.
--
-- For stateful rule groups, can contain @RulesString@, @RulesSourceList@,
-- or @StatefulRules@.
--
-- For stateless rule groups, contains @StatelessRulesAndCustomActions@.
newRuleGroupDetails ::
  RuleGroupDetails
newRuleGroupDetails =
  RuleGroupDetails'
    { ruleVariables = Prelude.Nothing,
      rulesSource = Prelude.Nothing
    }

-- | Additional settings to use in the specified rules.
ruleGroupDetails_ruleVariables :: Lens.Lens' RuleGroupDetails (Prelude.Maybe RuleGroupVariables)
ruleGroupDetails_ruleVariables = Lens.lens (\RuleGroupDetails' {ruleVariables} -> ruleVariables) (\s@RuleGroupDetails' {} a -> s {ruleVariables = a} :: RuleGroupDetails)

-- | The rules and actions for the rule group.
--
-- For stateful rule groups, can contain @RulesString@, @RulesSourceList@,
-- or @StatefulRules@.
--
-- For stateless rule groups, contains @StatelessRulesAndCustomActions@.
ruleGroupDetails_rulesSource :: Lens.Lens' RuleGroupDetails (Prelude.Maybe RuleGroupSource)
ruleGroupDetails_rulesSource = Lens.lens (\RuleGroupDetails' {rulesSource} -> rulesSource) (\s@RuleGroupDetails' {} a -> s {rulesSource = a} :: RuleGroupDetails)

instance Data.FromJSON RuleGroupDetails where
  parseJSON =
    Data.withObject
      "RuleGroupDetails"
      ( \x ->
          RuleGroupDetails'
            Prelude.<$> (x Data..:? "RuleVariables")
            Prelude.<*> (x Data..:? "RulesSource")
      )

instance Prelude.Hashable RuleGroupDetails where
  hashWithSalt _salt RuleGroupDetails' {..} =
    _salt `Prelude.hashWithSalt` ruleVariables
      `Prelude.hashWithSalt` rulesSource

instance Prelude.NFData RuleGroupDetails where
  rnf RuleGroupDetails' {..} =
    Prelude.rnf ruleVariables
      `Prelude.seq` Prelude.rnf rulesSource

instance Data.ToJSON RuleGroupDetails where
  toJSON RuleGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleVariables" Data..=) Prelude.<$> ruleVariables,
            ("RulesSource" Data..=) Prelude.<$> rulesSource
          ]
      )

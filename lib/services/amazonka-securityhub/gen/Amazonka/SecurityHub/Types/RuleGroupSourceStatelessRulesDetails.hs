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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleDefinition

-- | A stateless rule in the rule group.
--
-- /See:/ 'newRuleGroupSourceStatelessRulesDetails' smart constructor.
data RuleGroupSourceStatelessRulesDetails = RuleGroupSourceStatelessRulesDetails'
  { -- | Indicates the order in which to run this rule relative to all of the
    -- rules in the stateless rule group.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Provides the definition of the stateless rule.
    ruleDefinition :: Prelude.Maybe RuleGroupSourceStatelessRuleDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'ruleGroupSourceStatelessRulesDetails_priority' - Indicates the order in which to run this rule relative to all of the
-- rules in the stateless rule group.
--
-- 'ruleDefinition', 'ruleGroupSourceStatelessRulesDetails_ruleDefinition' - Provides the definition of the stateless rule.
newRuleGroupSourceStatelessRulesDetails ::
  RuleGroupSourceStatelessRulesDetails
newRuleGroupSourceStatelessRulesDetails =
  RuleGroupSourceStatelessRulesDetails'
    { priority =
        Prelude.Nothing,
      ruleDefinition = Prelude.Nothing
    }

-- | Indicates the order in which to run this rule relative to all of the
-- rules in the stateless rule group.
ruleGroupSourceStatelessRulesDetails_priority :: Lens.Lens' RuleGroupSourceStatelessRulesDetails (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRulesDetails_priority = Lens.lens (\RuleGroupSourceStatelessRulesDetails' {priority} -> priority) (\s@RuleGroupSourceStatelessRulesDetails' {} a -> s {priority = a} :: RuleGroupSourceStatelessRulesDetails)

-- | Provides the definition of the stateless rule.
ruleGroupSourceStatelessRulesDetails_ruleDefinition :: Lens.Lens' RuleGroupSourceStatelessRulesDetails (Prelude.Maybe RuleGroupSourceStatelessRuleDefinition)
ruleGroupSourceStatelessRulesDetails_ruleDefinition = Lens.lens (\RuleGroupSourceStatelessRulesDetails' {ruleDefinition} -> ruleDefinition) (\s@RuleGroupSourceStatelessRulesDetails' {} a -> s {ruleDefinition = a} :: RuleGroupSourceStatelessRulesDetails)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRulesDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRulesDetails"
      ( \x ->
          RuleGroupSourceStatelessRulesDetails'
            Prelude.<$> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "RuleDefinition")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRulesDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRulesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` ruleDefinition

instance
  Prelude.NFData
    RuleGroupSourceStatelessRulesDetails
  where
  rnf RuleGroupSourceStatelessRulesDetails' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ruleDefinition

instance
  Data.ToJSON
    RuleGroupSourceStatelessRulesDetails
  where
  toJSON RuleGroupSourceStatelessRulesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Priority" Data..=) Prelude.<$> priority,
            ("RuleDefinition" Data..=)
              Prelude.<$> ruleDefinition
          ]
      )

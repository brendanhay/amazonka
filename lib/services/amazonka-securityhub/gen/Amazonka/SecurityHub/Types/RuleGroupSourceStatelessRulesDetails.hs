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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Provides the definition of the stateless rule.
    ruleDefinition :: Prelude.Maybe RuleGroupSourceStatelessRuleDefinition,
    -- | Indicates the order in which to run this rule relative to all of the
    -- rules in the stateless rule group.
    priority :: Prelude.Maybe Prelude.Int
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
-- 'ruleDefinition', 'ruleGroupSourceStatelessRulesDetails_ruleDefinition' - Provides the definition of the stateless rule.
--
-- 'priority', 'ruleGroupSourceStatelessRulesDetails_priority' - Indicates the order in which to run this rule relative to all of the
-- rules in the stateless rule group.
newRuleGroupSourceStatelessRulesDetails ::
  RuleGroupSourceStatelessRulesDetails
newRuleGroupSourceStatelessRulesDetails =
  RuleGroupSourceStatelessRulesDetails'
    { ruleDefinition =
        Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | Provides the definition of the stateless rule.
ruleGroupSourceStatelessRulesDetails_ruleDefinition :: Lens.Lens' RuleGroupSourceStatelessRulesDetails (Prelude.Maybe RuleGroupSourceStatelessRuleDefinition)
ruleGroupSourceStatelessRulesDetails_ruleDefinition = Lens.lens (\RuleGroupSourceStatelessRulesDetails' {ruleDefinition} -> ruleDefinition) (\s@RuleGroupSourceStatelessRulesDetails' {} a -> s {ruleDefinition = a} :: RuleGroupSourceStatelessRulesDetails)

-- | Indicates the order in which to run this rule relative to all of the
-- rules in the stateless rule group.
ruleGroupSourceStatelessRulesDetails_priority :: Lens.Lens' RuleGroupSourceStatelessRulesDetails (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRulesDetails_priority = Lens.lens (\RuleGroupSourceStatelessRulesDetails' {priority} -> priority) (\s@RuleGroupSourceStatelessRulesDetails' {} a -> s {priority = a} :: RuleGroupSourceStatelessRulesDetails)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRulesDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRulesDetails"
      ( \x ->
          RuleGroupSourceStatelessRulesDetails'
            Prelude.<$> (x Data..:? "RuleDefinition")
            Prelude.<*> (x Data..:? "Priority")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRulesDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRulesDetails' {..} =
      _salt `Prelude.hashWithSalt` ruleDefinition
        `Prelude.hashWithSalt` priority

instance
  Prelude.NFData
    RuleGroupSourceStatelessRulesDetails
  where
  rnf RuleGroupSourceStatelessRulesDetails' {..} =
    Prelude.rnf ruleDefinition
      `Prelude.seq` Prelude.rnf priority

instance
  Data.ToJSON
    RuleGroupSourceStatelessRulesDetails
  where
  toJSON RuleGroupSourceStatelessRulesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleDefinition" Data..=)
              Prelude.<$> ruleDefinition,
            ("Priority" Data..=) Prelude.<$> priority
          ]
      )

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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesDetails

-- | Stateless rules and custom actions for a stateless rule group.
--
-- /See:/ 'newRuleGroupSourceStatelessRulesAndCustomActionsDetails' smart constructor.
data RuleGroupSourceStatelessRulesAndCustomActionsDetails = RuleGroupSourceStatelessRulesAndCustomActionsDetails'
  { -- | Custom actions for the rule group.
    customActions :: Prelude.Maybe [RuleGroupSourceCustomActionsDetails],
    -- | Stateless rules for the rule group.
    statelessRules :: Prelude.Maybe [RuleGroupSourceStatelessRulesDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRulesAndCustomActionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customActions', 'ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions' - Custom actions for the rule group.
--
-- 'statelessRules', 'ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules' - Stateless rules for the rule group.
newRuleGroupSourceStatelessRulesAndCustomActionsDetails ::
  RuleGroupSourceStatelessRulesAndCustomActionsDetails
newRuleGroupSourceStatelessRulesAndCustomActionsDetails =
  RuleGroupSourceStatelessRulesAndCustomActionsDetails'
    { customActions =
        Prelude.Nothing,
      statelessRules =
        Prelude.Nothing
    }

-- | Custom actions for the rule group.
ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions :: Lens.Lens' RuleGroupSourceStatelessRulesAndCustomActionsDetails (Prelude.Maybe [RuleGroupSourceCustomActionsDetails])
ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions = Lens.lens (\RuleGroupSourceStatelessRulesAndCustomActionsDetails' {customActions} -> customActions) (\s@RuleGroupSourceStatelessRulesAndCustomActionsDetails' {} a -> s {customActions = a} :: RuleGroupSourceStatelessRulesAndCustomActionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Stateless rules for the rule group.
ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules :: Lens.Lens' RuleGroupSourceStatelessRulesAndCustomActionsDetails (Prelude.Maybe [RuleGroupSourceStatelessRulesDetails])
ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules = Lens.lens (\RuleGroupSourceStatelessRulesAndCustomActionsDetails' {statelessRules} -> statelessRules) (\s@RuleGroupSourceStatelessRulesAndCustomActionsDetails' {} a -> s {statelessRules = a} :: RuleGroupSourceStatelessRulesAndCustomActionsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RuleGroupSourceStatelessRulesAndCustomActionsDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRulesAndCustomActionsDetails"
      ( \x ->
          RuleGroupSourceStatelessRulesAndCustomActionsDetails'
            Prelude.<$> (x Data..:? "CustomActions" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "StatelessRules"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRulesAndCustomActionsDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRulesAndCustomActionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` customActions
        `Prelude.hashWithSalt` statelessRules

instance
  Prelude.NFData
    RuleGroupSourceStatelessRulesAndCustomActionsDetails
  where
  rnf
    RuleGroupSourceStatelessRulesAndCustomActionsDetails' {..} =
      Prelude.rnf customActions
        `Prelude.seq` Prelude.rnf statelessRules

instance
  Data.ToJSON
    RuleGroupSourceStatelessRulesAndCustomActionsDetails
  where
  toJSON
    RuleGroupSourceStatelessRulesAndCustomActionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CustomActions" Data..=) Prelude.<$> customActions,
              ("StatelessRules" Data..=)
                Prelude.<$> statelessRules
            ]
        )

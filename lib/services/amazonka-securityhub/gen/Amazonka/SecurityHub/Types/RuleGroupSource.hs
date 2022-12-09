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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceListDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails

-- | The rules and actions for the rule group.
--
-- /See:/ 'newRuleGroupSource' smart constructor.
data RuleGroupSource = RuleGroupSource'
  { -- | Stateful inspection criteria for a domain list rule group. A domain list
    -- rule group determines access by specific protocols to specific domains.
    rulesSourceList :: Prelude.Maybe RuleGroupSourceListDetails,
    -- | Stateful inspection criteria, provided in Suricata compatible intrusion
    -- prevention system (IPS) rules.
    rulesString :: Prelude.Maybe Prelude.Text,
    -- | Suricata rule specifications.
    statefulRules :: Prelude.Maybe [RuleGroupSourceStatefulRulesDetails],
    -- | The stateless rules and custom actions used by a stateless rule group.
    statelessRulesAndCustomActions :: Prelude.Maybe RuleGroupSourceStatelessRulesAndCustomActionsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rulesSourceList', 'ruleGroupSource_rulesSourceList' - Stateful inspection criteria for a domain list rule group. A domain list
-- rule group determines access by specific protocols to specific domains.
--
-- 'rulesString', 'ruleGroupSource_rulesString' - Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules.
--
-- 'statefulRules', 'ruleGroupSource_statefulRules' - Suricata rule specifications.
--
-- 'statelessRulesAndCustomActions', 'ruleGroupSource_statelessRulesAndCustomActions' - The stateless rules and custom actions used by a stateless rule group.
newRuleGroupSource ::
  RuleGroupSource
newRuleGroupSource =
  RuleGroupSource'
    { rulesSourceList = Prelude.Nothing,
      rulesString = Prelude.Nothing,
      statefulRules = Prelude.Nothing,
      statelessRulesAndCustomActions = Prelude.Nothing
    }

-- | Stateful inspection criteria for a domain list rule group. A domain list
-- rule group determines access by specific protocols to specific domains.
ruleGroupSource_rulesSourceList :: Lens.Lens' RuleGroupSource (Prelude.Maybe RuleGroupSourceListDetails)
ruleGroupSource_rulesSourceList = Lens.lens (\RuleGroupSource' {rulesSourceList} -> rulesSourceList) (\s@RuleGroupSource' {} a -> s {rulesSourceList = a} :: RuleGroupSource)

-- | Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules.
ruleGroupSource_rulesString :: Lens.Lens' RuleGroupSource (Prelude.Maybe Prelude.Text)
ruleGroupSource_rulesString = Lens.lens (\RuleGroupSource' {rulesString} -> rulesString) (\s@RuleGroupSource' {} a -> s {rulesString = a} :: RuleGroupSource)

-- | Suricata rule specifications.
ruleGroupSource_statefulRules :: Lens.Lens' RuleGroupSource (Prelude.Maybe [RuleGroupSourceStatefulRulesDetails])
ruleGroupSource_statefulRules = Lens.lens (\RuleGroupSource' {statefulRules} -> statefulRules) (\s@RuleGroupSource' {} a -> s {statefulRules = a} :: RuleGroupSource) Prelude.. Lens.mapping Lens.coerced

-- | The stateless rules and custom actions used by a stateless rule group.
ruleGroupSource_statelessRulesAndCustomActions :: Lens.Lens' RuleGroupSource (Prelude.Maybe RuleGroupSourceStatelessRulesAndCustomActionsDetails)
ruleGroupSource_statelessRulesAndCustomActions = Lens.lens (\RuleGroupSource' {statelessRulesAndCustomActions} -> statelessRulesAndCustomActions) (\s@RuleGroupSource' {} a -> s {statelessRulesAndCustomActions = a} :: RuleGroupSource)

instance Data.FromJSON RuleGroupSource where
  parseJSON =
    Data.withObject
      "RuleGroupSource"
      ( \x ->
          RuleGroupSource'
            Prelude.<$> (x Data..:? "RulesSourceList")
            Prelude.<*> (x Data..:? "RulesString")
            Prelude.<*> (x Data..:? "StatefulRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StatelessRulesAndCustomActions")
      )

instance Prelude.Hashable RuleGroupSource where
  hashWithSalt _salt RuleGroupSource' {..} =
    _salt `Prelude.hashWithSalt` rulesSourceList
      `Prelude.hashWithSalt` rulesString
      `Prelude.hashWithSalt` statefulRules
      `Prelude.hashWithSalt` statelessRulesAndCustomActions

instance Prelude.NFData RuleGroupSource where
  rnf RuleGroupSource' {..} =
    Prelude.rnf rulesSourceList
      `Prelude.seq` Prelude.rnf rulesString
      `Prelude.seq` Prelude.rnf statefulRules
      `Prelude.seq` Prelude.rnf statelessRulesAndCustomActions

instance Data.ToJSON RuleGroupSource where
  toJSON RuleGroupSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RulesSourceList" Data..=)
              Prelude.<$> rulesSourceList,
            ("RulesString" Data..=) Prelude.<$> rulesString,
            ("StatefulRules" Data..=) Prelude.<$> statefulRules,
            ("StatelessRulesAndCustomActions" Data..=)
              Prelude.<$> statelessRulesAndCustomActions
          ]
      )

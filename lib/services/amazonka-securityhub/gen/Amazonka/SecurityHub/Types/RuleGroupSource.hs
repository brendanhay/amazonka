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
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceListDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails

-- | The rules and actions for the rule group.
--
-- /See:/ 'newRuleGroupSource' smart constructor.
data RuleGroupSource = RuleGroupSource'
  { -- | Stateful inspection criteria, provided in Suricata compatible intrusion
    -- prevention system (IPS) rules.
    rulesString :: Prelude.Maybe Prelude.Text,
    -- | Suricata rule specifications.
    statefulRules :: Prelude.Maybe [RuleGroupSourceStatefulRulesDetails],
    -- | Stateful inspection criteria for a domain list rule group. A domain list
    -- rule group determines access by specific protocols to specific domains.
    rulesSourceList :: Prelude.Maybe RuleGroupSourceListDetails,
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
-- 'rulesString', 'ruleGroupSource_rulesString' - Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules.
--
-- 'statefulRules', 'ruleGroupSource_statefulRules' - Suricata rule specifications.
--
-- 'rulesSourceList', 'ruleGroupSource_rulesSourceList' - Stateful inspection criteria for a domain list rule group. A domain list
-- rule group determines access by specific protocols to specific domains.
--
-- 'statelessRulesAndCustomActions', 'ruleGroupSource_statelessRulesAndCustomActions' - The stateless rules and custom actions used by a stateless rule group.
newRuleGroupSource ::
  RuleGroupSource
newRuleGroupSource =
  RuleGroupSource'
    { rulesString = Prelude.Nothing,
      statefulRules = Prelude.Nothing,
      rulesSourceList = Prelude.Nothing,
      statelessRulesAndCustomActions = Prelude.Nothing
    }

-- | Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules.
ruleGroupSource_rulesString :: Lens.Lens' RuleGroupSource (Prelude.Maybe Prelude.Text)
ruleGroupSource_rulesString = Lens.lens (\RuleGroupSource' {rulesString} -> rulesString) (\s@RuleGroupSource' {} a -> s {rulesString = a} :: RuleGroupSource)

-- | Suricata rule specifications.
ruleGroupSource_statefulRules :: Lens.Lens' RuleGroupSource (Prelude.Maybe [RuleGroupSourceStatefulRulesDetails])
ruleGroupSource_statefulRules = Lens.lens (\RuleGroupSource' {statefulRules} -> statefulRules) (\s@RuleGroupSource' {} a -> s {statefulRules = a} :: RuleGroupSource) Prelude.. Lens.mapping Lens.coerced

-- | Stateful inspection criteria for a domain list rule group. A domain list
-- rule group determines access by specific protocols to specific domains.
ruleGroupSource_rulesSourceList :: Lens.Lens' RuleGroupSource (Prelude.Maybe RuleGroupSourceListDetails)
ruleGroupSource_rulesSourceList = Lens.lens (\RuleGroupSource' {rulesSourceList} -> rulesSourceList) (\s@RuleGroupSource' {} a -> s {rulesSourceList = a} :: RuleGroupSource)

-- | The stateless rules and custom actions used by a stateless rule group.
ruleGroupSource_statelessRulesAndCustomActions :: Lens.Lens' RuleGroupSource (Prelude.Maybe RuleGroupSourceStatelessRulesAndCustomActionsDetails)
ruleGroupSource_statelessRulesAndCustomActions = Lens.lens (\RuleGroupSource' {statelessRulesAndCustomActions} -> statelessRulesAndCustomActions) (\s@RuleGroupSource' {} a -> s {statelessRulesAndCustomActions = a} :: RuleGroupSource)

instance Core.FromJSON RuleGroupSource where
  parseJSON =
    Core.withObject
      "RuleGroupSource"
      ( \x ->
          RuleGroupSource'
            Prelude.<$> (x Core..:? "RulesString")
            Prelude.<*> (x Core..:? "StatefulRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RulesSourceList")
            Prelude.<*> (x Core..:? "StatelessRulesAndCustomActions")
      )

instance Prelude.Hashable RuleGroupSource where
  hashWithSalt _salt RuleGroupSource' {..} =
    _salt `Prelude.hashWithSalt` rulesString
      `Prelude.hashWithSalt` statefulRules
      `Prelude.hashWithSalt` rulesSourceList
      `Prelude.hashWithSalt` statelessRulesAndCustomActions

instance Prelude.NFData RuleGroupSource where
  rnf RuleGroupSource' {..} =
    Prelude.rnf rulesString
      `Prelude.seq` Prelude.rnf statefulRules
      `Prelude.seq` Prelude.rnf rulesSourceList
      `Prelude.seq` Prelude.rnf statelessRulesAndCustomActions

instance Core.ToJSON RuleGroupSource where
  toJSON RuleGroupSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RulesString" Core..=) Prelude.<$> rulesString,
            ("StatefulRules" Core..=) Prelude.<$> statefulRules,
            ("RulesSourceList" Core..=)
              Prelude.<$> rulesSourceList,
            ("StatelessRulesAndCustomActions" Core..=)
              Prelude.<$> statelessRulesAndCustomActions
          ]
      )

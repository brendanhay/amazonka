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
-- Module      : Amazonka.NetworkFirewall.Types.RuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.RuleVariables
import Amazonka.NetworkFirewall.Types.RulesSource
import Amazonka.NetworkFirewall.Types.StatefulRuleOptions
import qualified Amazonka.Prelude as Prelude

-- | The object that defines the rules in a rule group. This, along with
-- RuleGroupResponse, define the rule group. You can retrieve all objects
-- for a rule group by calling DescribeRuleGroup.
--
-- AWS Network Firewall uses a rule group to inspect and control network
-- traffic. You define stateless rule groups to inspect individual packets
-- and you define stateful rule groups to inspect packets in the context of
-- their traffic flow.
--
-- To use a rule group, you include it by reference in an Network Firewall
-- firewall policy, then you use the policy in a firewall. You can
-- reference a rule group from more than one firewall policy, and you can
-- use a firewall policy in more than one firewall.
--
-- /See:/ 'newRuleGroup' smart constructor.
data RuleGroup = RuleGroup'
  { -- | Additional options governing how Network Firewall handles stateful
    -- rules. The policies where you use your stateful rule group must have
    -- stateful rule options settings that are compatible with these settings.
    statefulRuleOptions :: Prelude.Maybe StatefulRuleOptions,
    -- | Settings that are available for use in the rules in the rule group. You
    -- can only use these for stateful rule groups.
    ruleVariables :: Prelude.Maybe RuleVariables,
    -- | The stateful rules or stateless rules for the rule group.
    rulesSource :: RulesSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statefulRuleOptions', 'ruleGroup_statefulRuleOptions' - Additional options governing how Network Firewall handles stateful
-- rules. The policies where you use your stateful rule group must have
-- stateful rule options settings that are compatible with these settings.
--
-- 'ruleVariables', 'ruleGroup_ruleVariables' - Settings that are available for use in the rules in the rule group. You
-- can only use these for stateful rule groups.
--
-- 'rulesSource', 'ruleGroup_rulesSource' - The stateful rules or stateless rules for the rule group.
newRuleGroup ::
  -- | 'rulesSource'
  RulesSource ->
  RuleGroup
newRuleGroup pRulesSource_ =
  RuleGroup'
    { statefulRuleOptions = Prelude.Nothing,
      ruleVariables = Prelude.Nothing,
      rulesSource = pRulesSource_
    }

-- | Additional options governing how Network Firewall handles stateful
-- rules. The policies where you use your stateful rule group must have
-- stateful rule options settings that are compatible with these settings.
ruleGroup_statefulRuleOptions :: Lens.Lens' RuleGroup (Prelude.Maybe StatefulRuleOptions)
ruleGroup_statefulRuleOptions = Lens.lens (\RuleGroup' {statefulRuleOptions} -> statefulRuleOptions) (\s@RuleGroup' {} a -> s {statefulRuleOptions = a} :: RuleGroup)

-- | Settings that are available for use in the rules in the rule group. You
-- can only use these for stateful rule groups.
ruleGroup_ruleVariables :: Lens.Lens' RuleGroup (Prelude.Maybe RuleVariables)
ruleGroup_ruleVariables = Lens.lens (\RuleGroup' {ruleVariables} -> ruleVariables) (\s@RuleGroup' {} a -> s {ruleVariables = a} :: RuleGroup)

-- | The stateful rules or stateless rules for the rule group.
ruleGroup_rulesSource :: Lens.Lens' RuleGroup RulesSource
ruleGroup_rulesSource = Lens.lens (\RuleGroup' {rulesSource} -> rulesSource) (\s@RuleGroup' {} a -> s {rulesSource = a} :: RuleGroup)

instance Core.FromJSON RuleGroup where
  parseJSON =
    Core.withObject
      "RuleGroup"
      ( \x ->
          RuleGroup'
            Prelude.<$> (x Core..:? "StatefulRuleOptions")
            Prelude.<*> (x Core..:? "RuleVariables")
            Prelude.<*> (x Core..: "RulesSource")
      )

instance Prelude.Hashable RuleGroup where
  hashWithSalt salt' RuleGroup' {..} =
    salt' `Prelude.hashWithSalt` rulesSource
      `Prelude.hashWithSalt` ruleVariables
      `Prelude.hashWithSalt` statefulRuleOptions

instance Prelude.NFData RuleGroup where
  rnf RuleGroup' {..} =
    Prelude.rnf statefulRuleOptions
      `Prelude.seq` Prelude.rnf rulesSource
      `Prelude.seq` Prelude.rnf ruleVariables

instance Core.ToJSON RuleGroup where
  toJSON RuleGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StatefulRuleOptions" Core..=)
              Prelude.<$> statefulRuleOptions,
            ("RuleVariables" Core..=) Prelude.<$> ruleVariables,
            Prelude.Just ("RulesSource" Core..= rulesSource)
          ]
      )

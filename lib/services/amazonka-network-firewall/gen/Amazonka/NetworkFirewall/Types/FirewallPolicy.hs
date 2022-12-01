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
-- Module      : Amazonka.NetworkFirewall.Types.FirewallPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.FirewallPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkFirewall.Types.CustomAction
import Amazonka.NetworkFirewall.Types.StatefulEngineOptions
import Amazonka.NetworkFirewall.Types.StatefulRuleGroupReference
import Amazonka.NetworkFirewall.Types.StatelessRuleGroupReference
import qualified Amazonka.Prelude as Prelude

-- | The firewall policy defines the behavior of a firewall using a
-- collection of stateless and stateful rule groups and other settings. You
-- can use one firewall policy for multiple firewalls.
--
-- This, along with FirewallPolicyResponse, define the policy. You can
-- retrieve all objects for a firewall policy by calling
-- DescribeFirewallPolicy.
--
-- /See:/ 'newFirewallPolicy' smart constructor.
data FirewallPolicy = FirewallPolicy'
  { -- | Additional options governing how Network Firewall handles stateful
    -- rules. The stateful rule groups that you use in your policy must have
    -- stateful rule options settings that are compatible with these settings.
    statefulEngineOptions :: Prelude.Maybe StatefulEngineOptions,
    -- | The custom action definitions that are available for use in the firewall
    -- policy\'s @StatelessDefaultActions@ setting. You name each custom action
    -- that you define, and then you can use it by name in your default actions
    -- specifications.
    statelessCustomActions :: Prelude.Maybe [CustomAction],
    -- | References to the stateful rule groups that are used in the policy.
    -- These define the inspection criteria in stateful rules.
    statefulRuleGroupReferences :: Prelude.Maybe [StatefulRuleGroupReference],
    -- | References to the stateless rule groups that are used in the policy.
    -- These define the matching criteria in stateless rules.
    statelessRuleGroupReferences :: Prelude.Maybe [StatelessRuleGroupReference],
    -- | The default actions to take on a packet that doesn\'t match any stateful
    -- rules. The stateful default action is optional, and is only valid when
    -- using the strict rule order.
    --
    -- Valid values of the stateful default action:
    --
    -- -   aws:drop_strict
    --
    -- -   aws:drop_established
    --
    -- -   aws:alert_strict
    --
    -- -   aws:alert_established
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html#suricata-strict-rule-evaluation-order.html Strict evaluation order>
    -- in the /Network Firewall Developer Guide/.
    statefulDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to take on a packet if it doesn\'t match any of the
    -- stateless rules in the policy. If you want non-matching packets to be
    -- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
    --
    -- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
    -- @aws:forward_to_sfe@. In addition, you can specify custom actions that
    -- are compatible with your standard section choice.
    --
    -- For example, you could specify @[\"aws:pass\"]@ or you could specify
    -- @[\"aws:pass\", “customActionName”]@. For information about
    -- compatibility, see the custom action descriptions under CustomAction.
    statelessDefaultActions :: [Prelude.Text],
    -- | The actions to take on a fragmented UDP packet if it doesn\'t match any
    -- of the stateless rules in the policy. Network Firewall only manages UDP
    -- packet fragments and silently drops packet fragments for other
    -- protocols. If you want non-matching fragmented UDP packets to be
    -- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
    --
    -- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
    -- @aws:forward_to_sfe@. In addition, you can specify custom actions that
    -- are compatible with your standard section choice.
    --
    -- For example, you could specify @[\"aws:pass\"]@ or you could specify
    -- @[\"aws:pass\", “customActionName”]@. For information about
    -- compatibility, see the custom action descriptions under CustomAction.
    statelessFragmentDefaultActions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statefulEngineOptions', 'firewallPolicy_statefulEngineOptions' - Additional options governing how Network Firewall handles stateful
-- rules. The stateful rule groups that you use in your policy must have
-- stateful rule options settings that are compatible with these settings.
--
-- 'statelessCustomActions', 'firewallPolicy_statelessCustomActions' - The custom action definitions that are available for use in the firewall
-- policy\'s @StatelessDefaultActions@ setting. You name each custom action
-- that you define, and then you can use it by name in your default actions
-- specifications.
--
-- 'statefulRuleGroupReferences', 'firewallPolicy_statefulRuleGroupReferences' - References to the stateful rule groups that are used in the policy.
-- These define the inspection criteria in stateful rules.
--
-- 'statelessRuleGroupReferences', 'firewallPolicy_statelessRuleGroupReferences' - References to the stateless rule groups that are used in the policy.
-- These define the matching criteria in stateless rules.
--
-- 'statefulDefaultActions', 'firewallPolicy_statefulDefaultActions' - The default actions to take on a packet that doesn\'t match any stateful
-- rules. The stateful default action is optional, and is only valid when
-- using the strict rule order.
--
-- Valid values of the stateful default action:
--
-- -   aws:drop_strict
--
-- -   aws:drop_established
--
-- -   aws:alert_strict
--
-- -   aws:alert_established
--
-- For more information, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html#suricata-strict-rule-evaluation-order.html Strict evaluation order>
-- in the /Network Firewall Developer Guide/.
--
-- 'statelessDefaultActions', 'firewallPolicy_statelessDefaultActions' - The actions to take on a packet if it doesn\'t match any of the
-- stateless rules in the policy. If you want non-matching packets to be
-- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
--
-- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
-- @aws:forward_to_sfe@. In addition, you can specify custom actions that
-- are compatible with your standard section choice.
--
-- For example, you could specify @[\"aws:pass\"]@ or you could specify
-- @[\"aws:pass\", “customActionName”]@. For information about
-- compatibility, see the custom action descriptions under CustomAction.
--
-- 'statelessFragmentDefaultActions', 'firewallPolicy_statelessFragmentDefaultActions' - The actions to take on a fragmented UDP packet if it doesn\'t match any
-- of the stateless rules in the policy. Network Firewall only manages UDP
-- packet fragments and silently drops packet fragments for other
-- protocols. If you want non-matching fragmented UDP packets to be
-- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
--
-- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
-- @aws:forward_to_sfe@. In addition, you can specify custom actions that
-- are compatible with your standard section choice.
--
-- For example, you could specify @[\"aws:pass\"]@ or you could specify
-- @[\"aws:pass\", “customActionName”]@. For information about
-- compatibility, see the custom action descriptions under CustomAction.
newFirewallPolicy ::
  FirewallPolicy
newFirewallPolicy =
  FirewallPolicy'
    { statefulEngineOptions =
        Prelude.Nothing,
      statelessCustomActions = Prelude.Nothing,
      statefulRuleGroupReferences = Prelude.Nothing,
      statelessRuleGroupReferences = Prelude.Nothing,
      statefulDefaultActions = Prelude.Nothing,
      statelessDefaultActions = Prelude.mempty,
      statelessFragmentDefaultActions = Prelude.mempty
    }

-- | Additional options governing how Network Firewall handles stateful
-- rules. The stateful rule groups that you use in your policy must have
-- stateful rule options settings that are compatible with these settings.
firewallPolicy_statefulEngineOptions :: Lens.Lens' FirewallPolicy (Prelude.Maybe StatefulEngineOptions)
firewallPolicy_statefulEngineOptions = Lens.lens (\FirewallPolicy' {statefulEngineOptions} -> statefulEngineOptions) (\s@FirewallPolicy' {} a -> s {statefulEngineOptions = a} :: FirewallPolicy)

-- | The custom action definitions that are available for use in the firewall
-- policy\'s @StatelessDefaultActions@ setting. You name each custom action
-- that you define, and then you can use it by name in your default actions
-- specifications.
firewallPolicy_statelessCustomActions :: Lens.Lens' FirewallPolicy (Prelude.Maybe [CustomAction])
firewallPolicy_statelessCustomActions = Lens.lens (\FirewallPolicy' {statelessCustomActions} -> statelessCustomActions) (\s@FirewallPolicy' {} a -> s {statelessCustomActions = a} :: FirewallPolicy) Prelude.. Lens.mapping Lens.coerced

-- | References to the stateful rule groups that are used in the policy.
-- These define the inspection criteria in stateful rules.
firewallPolicy_statefulRuleGroupReferences :: Lens.Lens' FirewallPolicy (Prelude.Maybe [StatefulRuleGroupReference])
firewallPolicy_statefulRuleGroupReferences = Lens.lens (\FirewallPolicy' {statefulRuleGroupReferences} -> statefulRuleGroupReferences) (\s@FirewallPolicy' {} a -> s {statefulRuleGroupReferences = a} :: FirewallPolicy) Prelude.. Lens.mapping Lens.coerced

-- | References to the stateless rule groups that are used in the policy.
-- These define the matching criteria in stateless rules.
firewallPolicy_statelessRuleGroupReferences :: Lens.Lens' FirewallPolicy (Prelude.Maybe [StatelessRuleGroupReference])
firewallPolicy_statelessRuleGroupReferences = Lens.lens (\FirewallPolicy' {statelessRuleGroupReferences} -> statelessRuleGroupReferences) (\s@FirewallPolicy' {} a -> s {statelessRuleGroupReferences = a} :: FirewallPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The default actions to take on a packet that doesn\'t match any stateful
-- rules. The stateful default action is optional, and is only valid when
-- using the strict rule order.
--
-- Valid values of the stateful default action:
--
-- -   aws:drop_strict
--
-- -   aws:drop_established
--
-- -   aws:alert_strict
--
-- -   aws:alert_established
--
-- For more information, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html#suricata-strict-rule-evaluation-order.html Strict evaluation order>
-- in the /Network Firewall Developer Guide/.
firewallPolicy_statefulDefaultActions :: Lens.Lens' FirewallPolicy (Prelude.Maybe [Prelude.Text])
firewallPolicy_statefulDefaultActions = Lens.lens (\FirewallPolicy' {statefulDefaultActions} -> statefulDefaultActions) (\s@FirewallPolicy' {} a -> s {statefulDefaultActions = a} :: FirewallPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on a packet if it doesn\'t match any of the
-- stateless rules in the policy. If you want non-matching packets to be
-- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
--
-- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
-- @aws:forward_to_sfe@. In addition, you can specify custom actions that
-- are compatible with your standard section choice.
--
-- For example, you could specify @[\"aws:pass\"]@ or you could specify
-- @[\"aws:pass\", “customActionName”]@. For information about
-- compatibility, see the custom action descriptions under CustomAction.
firewallPolicy_statelessDefaultActions :: Lens.Lens' FirewallPolicy [Prelude.Text]
firewallPolicy_statelessDefaultActions = Lens.lens (\FirewallPolicy' {statelessDefaultActions} -> statelessDefaultActions) (\s@FirewallPolicy' {} a -> s {statelessDefaultActions = a} :: FirewallPolicy) Prelude.. Lens.coerced

-- | The actions to take on a fragmented UDP packet if it doesn\'t match any
-- of the stateless rules in the policy. Network Firewall only manages UDP
-- packet fragments and silently drops packet fragments for other
-- protocols. If you want non-matching fragmented UDP packets to be
-- forwarded for stateful inspection, specify @aws:forward_to_sfe@.
--
-- You must specify one of the standard actions: @aws:pass@, @aws:drop@, or
-- @aws:forward_to_sfe@. In addition, you can specify custom actions that
-- are compatible with your standard section choice.
--
-- For example, you could specify @[\"aws:pass\"]@ or you could specify
-- @[\"aws:pass\", “customActionName”]@. For information about
-- compatibility, see the custom action descriptions under CustomAction.
firewallPolicy_statelessFragmentDefaultActions :: Lens.Lens' FirewallPolicy [Prelude.Text]
firewallPolicy_statelessFragmentDefaultActions = Lens.lens (\FirewallPolicy' {statelessFragmentDefaultActions} -> statelessFragmentDefaultActions) (\s@FirewallPolicy' {} a -> s {statelessFragmentDefaultActions = a} :: FirewallPolicy) Prelude.. Lens.coerced

instance Core.FromJSON FirewallPolicy where
  parseJSON =
    Core.withObject
      "FirewallPolicy"
      ( \x ->
          FirewallPolicy'
            Prelude.<$> (x Core..:? "StatefulEngineOptions")
            Prelude.<*> ( x Core..:? "StatelessCustomActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatefulRuleGroupReferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessRuleGroupReferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatefulDefaultActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessDefaultActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessFragmentDefaultActions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FirewallPolicy where
  hashWithSalt _salt FirewallPolicy' {..} =
    _salt `Prelude.hashWithSalt` statefulEngineOptions
      `Prelude.hashWithSalt` statelessCustomActions
      `Prelude.hashWithSalt` statefulRuleGroupReferences
      `Prelude.hashWithSalt` statelessRuleGroupReferences
      `Prelude.hashWithSalt` statefulDefaultActions
      `Prelude.hashWithSalt` statelessDefaultActions
      `Prelude.hashWithSalt` statelessFragmentDefaultActions

instance Prelude.NFData FirewallPolicy where
  rnf FirewallPolicy' {..} =
    Prelude.rnf statefulEngineOptions
      `Prelude.seq` Prelude.rnf statelessCustomActions
      `Prelude.seq` Prelude.rnf statefulRuleGroupReferences
      `Prelude.seq` Prelude.rnf statelessRuleGroupReferences
      `Prelude.seq` Prelude.rnf statefulDefaultActions
      `Prelude.seq` Prelude.rnf statelessDefaultActions
      `Prelude.seq` Prelude.rnf statelessFragmentDefaultActions

instance Core.ToJSON FirewallPolicy where
  toJSON FirewallPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StatefulEngineOptions" Core..=)
              Prelude.<$> statefulEngineOptions,
            ("StatelessCustomActions" Core..=)
              Prelude.<$> statelessCustomActions,
            ("StatefulRuleGroupReferences" Core..=)
              Prelude.<$> statefulRuleGroupReferences,
            ("StatelessRuleGroupReferences" Core..=)
              Prelude.<$> statelessRuleGroupReferences,
            ("StatefulDefaultActions" Core..=)
              Prelude.<$> statefulDefaultActions,
            Prelude.Just
              ( "StatelessDefaultActions"
                  Core..= statelessDefaultActions
              ),
            Prelude.Just
              ( "StatelessFragmentDefaultActions"
                  Core..= statelessFragmentDefaultActions
              )
          ]
      )

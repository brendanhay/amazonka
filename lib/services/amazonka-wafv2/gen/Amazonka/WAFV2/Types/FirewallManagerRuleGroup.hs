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
-- Module      : Amazonka.WAFV2.Types.FirewallManagerRuleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.FirewallManagerRuleGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FirewallManagerStatement
import Amazonka.WAFV2.Types.OverrideAction
import Amazonka.WAFV2.Types.VisibilityConfig

-- | A rule group that\'s defined for an Firewall Manager WAF policy.
--
-- /See:/ 'newFirewallManagerRuleGroup' smart constructor.
data FirewallManagerRuleGroup = FirewallManagerRuleGroup'
  { -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
    name :: Prelude.Text,
    -- | If you define more than one rule group in the first or last Firewall
    -- Manager rule groups, WAF evaluates each request against the rule groups
    -- in order, starting from the lowest priority setting. The priorities
    -- don\'t need to be consecutive, but they must all be different.
    priority :: Prelude.Natural,
    -- | The processing guidance for an Firewall Manager rule. This is like a
    -- regular rule Statement, but it can only contain a rule group reference.
    firewallManagerStatement :: FirewallManagerStatement,
    -- | The action to use in the place of the action that results from the rule
    -- group evaluation. Set the override action to none to leave the result of
    -- the rule group alone. Set it to count to override the result to count
    -- only.
    --
    -- You can only use this for rule statements that reference a rule group,
    -- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
    --
    -- This option is usually set to none. It does not affect how the rules in
    -- the rule group are evaluated. If you want the rules in the rule group to
    -- only count matches, do not use this and instead use the rule action
    -- override option, with @Count@ action, in your rule group reference
    -- statement settings.
    overrideAction :: OverrideAction,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallManagerRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'firewallManagerRuleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'priority', 'firewallManagerRuleGroup_priority' - If you define more than one rule group in the first or last Firewall
-- Manager rule groups, WAF evaluates each request against the rule groups
-- in order, starting from the lowest priority setting. The priorities
-- don\'t need to be consecutive, but they must all be different.
--
-- 'firewallManagerStatement', 'firewallManagerRuleGroup_firewallManagerStatement' - The processing guidance for an Firewall Manager rule. This is like a
-- regular rule Statement, but it can only contain a rule group reference.
--
-- 'overrideAction', 'firewallManagerRuleGroup_overrideAction' - The action to use in the place of the action that results from the rule
-- group evaluation. Set the override action to none to leave the result of
-- the rule group alone. Set it to count to override the result to count
-- only.
--
-- You can only use this for rule statements that reference a rule group,
-- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
--
-- 'visibilityConfig', 'firewallManagerRuleGroup_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newFirewallManagerRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'firewallManagerStatement'
  FirewallManagerStatement ->
  -- | 'overrideAction'
  OverrideAction ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  FirewallManagerRuleGroup
newFirewallManagerRuleGroup
  pName_
  pPriority_
  pFirewallManagerStatement_
  pOverrideAction_
  pVisibilityConfig_ =
    FirewallManagerRuleGroup'
      { name = pName_,
        priority = pPriority_,
        firewallManagerStatement =
          pFirewallManagerStatement_,
        overrideAction = pOverrideAction_,
        visibilityConfig = pVisibilityConfig_
      }

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
firewallManagerRuleGroup_name :: Lens.Lens' FirewallManagerRuleGroup Prelude.Text
firewallManagerRuleGroup_name = Lens.lens (\FirewallManagerRuleGroup' {name} -> name) (\s@FirewallManagerRuleGroup' {} a -> s {name = a} :: FirewallManagerRuleGroup)

-- | If you define more than one rule group in the first or last Firewall
-- Manager rule groups, WAF evaluates each request against the rule groups
-- in order, starting from the lowest priority setting. The priorities
-- don\'t need to be consecutive, but they must all be different.
firewallManagerRuleGroup_priority :: Lens.Lens' FirewallManagerRuleGroup Prelude.Natural
firewallManagerRuleGroup_priority = Lens.lens (\FirewallManagerRuleGroup' {priority} -> priority) (\s@FirewallManagerRuleGroup' {} a -> s {priority = a} :: FirewallManagerRuleGroup)

-- | The processing guidance for an Firewall Manager rule. This is like a
-- regular rule Statement, but it can only contain a rule group reference.
firewallManagerRuleGroup_firewallManagerStatement :: Lens.Lens' FirewallManagerRuleGroup FirewallManagerStatement
firewallManagerRuleGroup_firewallManagerStatement = Lens.lens (\FirewallManagerRuleGroup' {firewallManagerStatement} -> firewallManagerStatement) (\s@FirewallManagerRuleGroup' {} a -> s {firewallManagerStatement = a} :: FirewallManagerRuleGroup)

-- | The action to use in the place of the action that results from the rule
-- group evaluation. Set the override action to none to leave the result of
-- the rule group alone. Set it to count to override the result to count
-- only.
--
-- You can only use this for rule statements that reference a rule group,
-- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
firewallManagerRuleGroup_overrideAction :: Lens.Lens' FirewallManagerRuleGroup OverrideAction
firewallManagerRuleGroup_overrideAction = Lens.lens (\FirewallManagerRuleGroup' {overrideAction} -> overrideAction) (\s@FirewallManagerRuleGroup' {} a -> s {overrideAction = a} :: FirewallManagerRuleGroup)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
firewallManagerRuleGroup_visibilityConfig :: Lens.Lens' FirewallManagerRuleGroup VisibilityConfig
firewallManagerRuleGroup_visibilityConfig = Lens.lens (\FirewallManagerRuleGroup' {visibilityConfig} -> visibilityConfig) (\s@FirewallManagerRuleGroup' {} a -> s {visibilityConfig = a} :: FirewallManagerRuleGroup)

instance Data.FromJSON FirewallManagerRuleGroup where
  parseJSON =
    Data.withObject
      "FirewallManagerRuleGroup"
      ( \x ->
          FirewallManagerRuleGroup'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Priority")
            Prelude.<*> (x Data..: "FirewallManagerStatement")
            Prelude.<*> (x Data..: "OverrideAction")
            Prelude.<*> (x Data..: "VisibilityConfig")
      )

instance Prelude.Hashable FirewallManagerRuleGroup where
  hashWithSalt _salt FirewallManagerRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` firewallManagerStatement
      `Prelude.hashWithSalt` overrideAction
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData FirewallManagerRuleGroup where
  rnf FirewallManagerRuleGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf firewallManagerStatement
      `Prelude.seq` Prelude.rnf overrideAction
      `Prelude.seq` Prelude.rnf visibilityConfig

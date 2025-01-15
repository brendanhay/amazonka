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
-- Module      : Amazonka.FMS.Types.StatefulRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.StatefulRuleGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.NetworkFirewallStatefulRuleGroupOverride
import qualified Amazonka.Prelude as Prelude

-- | Network Firewall stateful rule group, used in a
-- NetworkFirewallPolicyDescription.
--
-- /See:/ 'newStatefulRuleGroup' smart constructor.
data StatefulRuleGroup = StatefulRuleGroup'
  { -- | The action that allows the policy owner to override the behavior of the
    -- rule group within a policy.
    override :: Prelude.Maybe NetworkFirewallStatefulRuleGroupOverride,
    -- | An integer setting that indicates the order in which to run the stateful
    -- rule groups in a single Network Firewall firewall policy. This setting
    -- only applies to firewall policies that specify the @STRICT_ORDER@ rule
    -- order in the stateful engine options settings.
    --
    -- Network Firewall evalutes each stateful rule group against a packet
    -- starting with the group that has the lowest priority setting. You must
    -- ensure that the priority settings are unique within each policy. For
    -- information about
    --
    -- You can change the priority settings of your rule groups at any time. To
    -- make it easier to insert rule groups later, number them so there\'s a
    -- wide range in between, for example use 100, 200, and so on.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The resource ID of the rule group.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule group.
    ruleGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'statefulRuleGroup_override' - The action that allows the policy owner to override the behavior of the
-- rule group within a policy.
--
-- 'priority', 'statefulRuleGroup_priority' - An integer setting that indicates the order in which to run the stateful
-- rule groups in a single Network Firewall firewall policy. This setting
-- only applies to firewall policies that specify the @STRICT_ORDER@ rule
-- order in the stateful engine options settings.
--
-- Network Firewall evalutes each stateful rule group against a packet
-- starting with the group that has the lowest priority setting. You must
-- ensure that the priority settings are unique within each policy. For
-- information about
--
-- You can change the priority settings of your rule groups at any time. To
-- make it easier to insert rule groups later, number them so there\'s a
-- wide range in between, for example use 100, 200, and so on.
--
-- 'resourceId', 'statefulRuleGroup_resourceId' - The resource ID of the rule group.
--
-- 'ruleGroupName', 'statefulRuleGroup_ruleGroupName' - The name of the rule group.
newStatefulRuleGroup ::
  StatefulRuleGroup
newStatefulRuleGroup =
  StatefulRuleGroup'
    { override = Prelude.Nothing,
      priority = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      ruleGroupName = Prelude.Nothing
    }

-- | The action that allows the policy owner to override the behavior of the
-- rule group within a policy.
statefulRuleGroup_override :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe NetworkFirewallStatefulRuleGroupOverride)
statefulRuleGroup_override = Lens.lens (\StatefulRuleGroup' {override} -> override) (\s@StatefulRuleGroup' {} a -> s {override = a} :: StatefulRuleGroup)

-- | An integer setting that indicates the order in which to run the stateful
-- rule groups in a single Network Firewall firewall policy. This setting
-- only applies to firewall policies that specify the @STRICT_ORDER@ rule
-- order in the stateful engine options settings.
--
-- Network Firewall evalutes each stateful rule group against a packet
-- starting with the group that has the lowest priority setting. You must
-- ensure that the priority settings are unique within each policy. For
-- information about
--
-- You can change the priority settings of your rule groups at any time. To
-- make it easier to insert rule groups later, number them so there\'s a
-- wide range in between, for example use 100, 200, and so on.
statefulRuleGroup_priority :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe Prelude.Int)
statefulRuleGroup_priority = Lens.lens (\StatefulRuleGroup' {priority} -> priority) (\s@StatefulRuleGroup' {} a -> s {priority = a} :: StatefulRuleGroup)

-- | The resource ID of the rule group.
statefulRuleGroup_resourceId :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe Prelude.Text)
statefulRuleGroup_resourceId = Lens.lens (\StatefulRuleGroup' {resourceId} -> resourceId) (\s@StatefulRuleGroup' {} a -> s {resourceId = a} :: StatefulRuleGroup)

-- | The name of the rule group.
statefulRuleGroup_ruleGroupName :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe Prelude.Text)
statefulRuleGroup_ruleGroupName = Lens.lens (\StatefulRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@StatefulRuleGroup' {} a -> s {ruleGroupName = a} :: StatefulRuleGroup)

instance Data.FromJSON StatefulRuleGroup where
  parseJSON =
    Data.withObject
      "StatefulRuleGroup"
      ( \x ->
          StatefulRuleGroup'
            Prelude.<$> (x Data..:? "Override")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "RuleGroupName")
      )

instance Prelude.Hashable StatefulRuleGroup where
  hashWithSalt _salt StatefulRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` override
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` ruleGroupName

instance Prelude.NFData StatefulRuleGroup where
  rnf StatefulRuleGroup' {..} =
    Prelude.rnf override `Prelude.seq`
      Prelude.rnf priority `Prelude.seq`
        Prelude.rnf resourceId `Prelude.seq`
          Prelude.rnf ruleGroupName

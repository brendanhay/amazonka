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
-- Module      : Network.AWS.FMS.Types.StatelessRuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.StatelessRuleGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | AWS Network Firewall stateless rule group, used in a
-- NetworkFirewallPolicyDescription.
--
-- /See:/ 'newStatelessRuleGroup' smart constructor.
data StatelessRuleGroup = StatelessRuleGroup'
  { -- | The resource ID of the rule group.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The priority of the rule group. AWS Network Firewall evaluates the
    -- stateless rule groups in a firewall policy starting from the lowest
    -- priority setting.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The name of the rule group.
    ruleGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'statelessRuleGroup_resourceId' - The resource ID of the rule group.
--
-- 'priority', 'statelessRuleGroup_priority' - The priority of the rule group. AWS Network Firewall evaluates the
-- stateless rule groups in a firewall policy starting from the lowest
-- priority setting.
--
-- 'ruleGroupName', 'statelessRuleGroup_ruleGroupName' - The name of the rule group.
newStatelessRuleGroup ::
  StatelessRuleGroup
newStatelessRuleGroup =
  StatelessRuleGroup'
    { resourceId = Prelude.Nothing,
      priority = Prelude.Nothing,
      ruleGroupName = Prelude.Nothing
    }

-- | The resource ID of the rule group.
statelessRuleGroup_resourceId :: Lens.Lens' StatelessRuleGroup (Prelude.Maybe Prelude.Text)
statelessRuleGroup_resourceId = Lens.lens (\StatelessRuleGroup' {resourceId} -> resourceId) (\s@StatelessRuleGroup' {} a -> s {resourceId = a} :: StatelessRuleGroup)

-- | The priority of the rule group. AWS Network Firewall evaluates the
-- stateless rule groups in a firewall policy starting from the lowest
-- priority setting.
statelessRuleGroup_priority :: Lens.Lens' StatelessRuleGroup (Prelude.Maybe Prelude.Natural)
statelessRuleGroup_priority = Lens.lens (\StatelessRuleGroup' {priority} -> priority) (\s@StatelessRuleGroup' {} a -> s {priority = a} :: StatelessRuleGroup)

-- | The name of the rule group.
statelessRuleGroup_ruleGroupName :: Lens.Lens' StatelessRuleGroup (Prelude.Maybe Prelude.Text)
statelessRuleGroup_ruleGroupName = Lens.lens (\StatelessRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@StatelessRuleGroup' {} a -> s {ruleGroupName = a} :: StatelessRuleGroup)

instance Core.FromJSON StatelessRuleGroup where
  parseJSON =
    Core.withObject
      "StatelessRuleGroup"
      ( \x ->
          StatelessRuleGroup'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "RuleGroupName")
      )

instance Prelude.Hashable StatelessRuleGroup

instance Prelude.NFData StatelessRuleGroup

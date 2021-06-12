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
-- Module      : Network.AWS.FMS.Types.StatefulRuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.StatefulRuleGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | AWS Network Firewall stateful rule group, used in a
-- NetworkFirewallPolicyDescription.
--
-- /See:/ 'newStatefulRuleGroup' smart constructor.
data StatefulRuleGroup = StatefulRuleGroup'
  { -- | The resource ID of the rule group.
    resourceId :: Core.Maybe Core.Text,
    -- | The name of the rule group.
    ruleGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StatefulRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'statefulRuleGroup_resourceId' - The resource ID of the rule group.
--
-- 'ruleGroupName', 'statefulRuleGroup_ruleGroupName' - The name of the rule group.
newStatefulRuleGroup ::
  StatefulRuleGroup
newStatefulRuleGroup =
  StatefulRuleGroup'
    { resourceId = Core.Nothing,
      ruleGroupName = Core.Nothing
    }

-- | The resource ID of the rule group.
statefulRuleGroup_resourceId :: Lens.Lens' StatefulRuleGroup (Core.Maybe Core.Text)
statefulRuleGroup_resourceId = Lens.lens (\StatefulRuleGroup' {resourceId} -> resourceId) (\s@StatefulRuleGroup' {} a -> s {resourceId = a} :: StatefulRuleGroup)

-- | The name of the rule group.
statefulRuleGroup_ruleGroupName :: Lens.Lens' StatefulRuleGroup (Core.Maybe Core.Text)
statefulRuleGroup_ruleGroupName = Lens.lens (\StatefulRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@StatefulRuleGroup' {} a -> s {ruleGroupName = a} :: StatefulRuleGroup)

instance Core.FromJSON StatefulRuleGroup where
  parseJSON =
    Core.withObject
      "StatefulRuleGroup"
      ( \x ->
          StatefulRuleGroup'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "RuleGroupName")
      )

instance Core.Hashable StatefulRuleGroup

instance Core.NFData StatefulRuleGroup

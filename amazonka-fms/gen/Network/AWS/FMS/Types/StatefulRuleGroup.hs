{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | AWS Network Firewall stateful rule group, used in a
-- NetworkFirewallPolicyDescription.
--
-- /See:/ 'newStatefulRuleGroup' smart constructor.
data StatefulRuleGroup = StatefulRuleGroup'
  { -- | The resource ID of the rule group.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule group.
    ruleGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      ruleGroupName = Prelude.Nothing
    }

-- | The resource ID of the rule group.
statefulRuleGroup_resourceId :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe Prelude.Text)
statefulRuleGroup_resourceId = Lens.lens (\StatefulRuleGroup' {resourceId} -> resourceId) (\s@StatefulRuleGroup' {} a -> s {resourceId = a} :: StatefulRuleGroup)

-- | The name of the rule group.
statefulRuleGroup_ruleGroupName :: Lens.Lens' StatefulRuleGroup (Prelude.Maybe Prelude.Text)
statefulRuleGroup_ruleGroupName = Lens.lens (\StatefulRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@StatefulRuleGroup' {} a -> s {ruleGroupName = a} :: StatefulRuleGroup)

instance Prelude.FromJSON StatefulRuleGroup where
  parseJSON =
    Prelude.withObject
      "StatefulRuleGroup"
      ( \x ->
          StatefulRuleGroup'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "RuleGroupName")
      )

instance Prelude.Hashable StatefulRuleGroup

instance Prelude.NFData StatefulRuleGroup

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.StatefulRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.StatefulRuleGroup
  ( StatefulRuleGroup (..),

    -- * Smart constructor
    mkStatefulRuleGroup,

    -- * Lenses
    srgResourceId,
    srgRuleGroupName,
  )
where

import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.RuleGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | AWS Network Firewall stateful rule group, used in a 'NetworkFirewallPolicyDescription' .
--
-- /See:/ 'mkStatefulRuleGroup' smart constructor.
data StatefulRuleGroup = StatefulRuleGroup'
  { -- | The resource ID of the rule group.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The name of the rule group.
    ruleGroupName :: Core.Maybe Types.RuleGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StatefulRuleGroup' value with any optional fields omitted.
mkStatefulRuleGroup ::
  StatefulRuleGroup
mkStatefulRuleGroup =
  StatefulRuleGroup'
    { resourceId = Core.Nothing,
      ruleGroupName = Core.Nothing
    }

-- | The resource ID of the rule group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgResourceId :: Lens.Lens' StatefulRuleGroup (Core.Maybe Types.ResourceId)
srgResourceId = Lens.field @"resourceId"
{-# DEPRECATED srgResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The name of the rule group.
--
-- /Note:/ Consider using 'ruleGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgRuleGroupName :: Lens.Lens' StatefulRuleGroup (Core.Maybe Types.RuleGroupName)
srgRuleGroupName = Lens.field @"ruleGroupName"
{-# DEPRECATED srgRuleGroupName "Use generic-lens or generic-optics with 'ruleGroupName' instead." #-}

instance Core.FromJSON StatefulRuleGroup where
  parseJSON =
    Core.withObject "StatefulRuleGroup" Core.$
      \x ->
        StatefulRuleGroup'
          Core.<$> (x Core..:? "ResourceId") Core.<*> (x Core..:? "RuleGroupName")

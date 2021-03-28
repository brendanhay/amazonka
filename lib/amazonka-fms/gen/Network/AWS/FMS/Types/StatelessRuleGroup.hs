{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.StatelessRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.StatelessRuleGroup
  ( StatelessRuleGroup (..)
  -- * Smart constructor
  , mkStatelessRuleGroup
  -- * Lenses
  , sPriority
  , sResourceId
  , sRuleGroupName
  ) where

import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.RuleGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | AWS Network Firewall stateless rule group, used in a 'NetworkFirewallPolicyDescription' . 
--
-- /See:/ 'mkStatelessRuleGroup' smart constructor.
data StatelessRuleGroup = StatelessRuleGroup'
  { priority :: Core.Maybe Core.Natural
    -- ^ The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting. 
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The resource ID of the rule group.
  , ruleGroupName :: Core.Maybe Types.RuleGroupName
    -- ^ The name of the rule group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StatelessRuleGroup' value with any optional fields omitted.
mkStatelessRuleGroup
    :: StatelessRuleGroup
mkStatelessRuleGroup
  = StatelessRuleGroup'{priority = Core.Nothing,
                        resourceId = Core.Nothing, ruleGroupName = Core.Nothing}

-- | The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting. 
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPriority :: Lens.Lens' StatelessRuleGroup (Core.Maybe Core.Natural)
sPriority = Lens.field @"priority"
{-# INLINEABLE sPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The resource ID of the rule group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceId :: Lens.Lens' StatelessRuleGroup (Core.Maybe Types.ResourceId)
sResourceId = Lens.field @"resourceId"
{-# INLINEABLE sResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The name of the rule group.
--
-- /Note:/ Consider using 'ruleGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRuleGroupName :: Lens.Lens' StatelessRuleGroup (Core.Maybe Types.RuleGroupName)
sRuleGroupName = Lens.field @"ruleGroupName"
{-# INLINEABLE sRuleGroupName #-}
{-# DEPRECATED ruleGroupName "Use generic-lens or generic-optics with 'ruleGroupName' instead"  #-}

instance Core.FromJSON StatelessRuleGroup where
        parseJSON
          = Core.withObject "StatelessRuleGroup" Core.$
              \ x ->
                StatelessRuleGroup' Core.<$>
                  (x Core..:? "Priority") Core.<*> x Core..:? "ResourceId" Core.<*>
                    x Core..:? "RuleGroupName"

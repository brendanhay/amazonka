{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.StatelessRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.StatelessRuleGroup
  ( StatelessRuleGroup (..),

    -- * Smart constructor
    mkStatelessRuleGroup,

    -- * Lenses
    sResourceId,
    sPriority,
    sRuleGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | AWS Network Firewall stateless rule group, used in a 'NetworkFirewallPolicyDescription' .
--
-- /See:/ 'mkStatelessRuleGroup' smart constructor.
data StatelessRuleGroup = StatelessRuleGroup'
  { resourceId ::
      Lude.Maybe Lude.Text,
    priority :: Lude.Maybe Lude.Natural,
    ruleGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatelessRuleGroup' with the minimum fields required to make a request.
--
-- * 'priority' - The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting.
-- * 'resourceId' - The resource ID of the rule group.
-- * 'ruleGroupName' - The name of the rule group.
mkStatelessRuleGroup ::
  StatelessRuleGroup
mkStatelessRuleGroup =
  StatelessRuleGroup'
    { resourceId = Lude.Nothing,
      priority = Lude.Nothing,
      ruleGroupName = Lude.Nothing
    }

-- | The resource ID of the rule group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceId :: Lens.Lens' StatelessRuleGroup (Lude.Maybe Lude.Text)
sResourceId = Lens.lens (resourceId :: StatelessRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: StatelessRuleGroup)
{-# DEPRECATED sResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPriority :: Lens.Lens' StatelessRuleGroup (Lude.Maybe Lude.Natural)
sPriority = Lens.lens (priority :: StatelessRuleGroup -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: StatelessRuleGroup)
{-# DEPRECATED sPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The name of the rule group.
--
-- /Note:/ Consider using 'ruleGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRuleGroupName :: Lens.Lens' StatelessRuleGroup (Lude.Maybe Lude.Text)
sRuleGroupName = Lens.lens (ruleGroupName :: StatelessRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {ruleGroupName = a} :: StatelessRuleGroup)
{-# DEPRECATED sRuleGroupName "Use generic-lens or generic-optics with 'ruleGroupName' instead." #-}

instance Lude.FromJSON StatelessRuleGroup where
  parseJSON =
    Lude.withObject
      "StatelessRuleGroup"
      ( \x ->
          StatelessRuleGroup'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "Priority")
            Lude.<*> (x Lude..:? "RuleGroupName")
      )

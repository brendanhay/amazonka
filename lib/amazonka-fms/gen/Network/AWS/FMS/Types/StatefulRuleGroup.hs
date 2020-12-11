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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | AWS Network Firewall stateful rule group, used in a 'NetworkFirewallPolicyDescription' .
--
-- /See:/ 'mkStatefulRuleGroup' smart constructor.
data StatefulRuleGroup = StatefulRuleGroup'
  { resourceId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StatefulRuleGroup' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource ID of the rule group.
-- * 'ruleGroupName' - The name of the rule group.
mkStatefulRuleGroup ::
  StatefulRuleGroup
mkStatefulRuleGroup =
  StatefulRuleGroup'
    { resourceId = Lude.Nothing,
      ruleGroupName = Lude.Nothing
    }

-- | The resource ID of the rule group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgResourceId :: Lens.Lens' StatefulRuleGroup (Lude.Maybe Lude.Text)
srgResourceId = Lens.lens (resourceId :: StatefulRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: StatefulRuleGroup)
{-# DEPRECATED srgResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The name of the rule group.
--
-- /Note:/ Consider using 'ruleGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgRuleGroupName :: Lens.Lens' StatefulRuleGroup (Lude.Maybe Lude.Text)
srgRuleGroupName = Lens.lens (ruleGroupName :: StatefulRuleGroup -> Lude.Maybe Lude.Text) (\s a -> s {ruleGroupName = a} :: StatefulRuleGroup)
{-# DEPRECATED srgRuleGroupName "Use generic-lens or generic-optics with 'ruleGroupName' instead." #-}

instance Lude.FromJSON StatefulRuleGroup where
  parseJSON =
    Lude.withObject
      "StatefulRuleGroup"
      ( \x ->
          StatefulRuleGroup'
            Lude.<$> (x Lude..:? "ResourceId") Lude.<*> (x Lude..:? "RuleGroupName")
      )

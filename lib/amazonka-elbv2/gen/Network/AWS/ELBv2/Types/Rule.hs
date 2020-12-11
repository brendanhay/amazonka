-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rPriority,
    rActions,
    rConditions,
    rRuleARN,
    rIsDefault,
  )
where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.RuleCondition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a rule.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { priority :: Lude.Maybe Lude.Text,
    actions :: Lude.Maybe [Action],
    conditions :: Lude.Maybe [RuleCondition],
    ruleARN :: Lude.Maybe Lude.Text,
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- * 'actions' - The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
-- * 'conditions' - The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
-- * 'isDefault' - Indicates whether this is the default rule.
-- * 'priority' - The priority.
-- * 'ruleARN' - The Amazon Resource Name (ARN) of the rule.
mkRule ::
  Rule
mkRule =
  Rule'
    { priority = Lude.Nothing,
      actions = Lude.Nothing,
      conditions = Lude.Nothing,
      ruleARN = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | The priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPriority :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rPriority = Lens.lens (priority :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {priority = a} :: Rule)
{-# DEPRECATED rPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rActions :: Lens.Lens' Rule (Lude.Maybe [Action])
rActions = Lens.lens (actions :: Rule -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: Rule)
{-# DEPRECATED rActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rConditions :: Lens.Lens' Rule (Lude.Maybe [RuleCondition])
rConditions = Lens.lens (conditions :: Rule -> Lude.Maybe [RuleCondition]) (\s a -> s {conditions = a} :: Rule)
{-# DEPRECATED rConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRuleARN :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rRuleARN = Lens.lens (ruleARN :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: Rule)
{-# DEPRECATED rRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | Indicates whether this is the default rule.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIsDefault :: Lens.Lens' Rule (Lude.Maybe Lude.Bool)
rIsDefault = Lens.lens (isDefault :: Rule -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: Rule)
{-# DEPRECATED rIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML Rule where
  parseXML x =
    Rule'
      Lude.<$> (x Lude..@? "Priority")
      Lude.<*> ( x Lude..@? "Actions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Conditions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "RuleArn")
      Lude.<*> (x Lude..@? "IsDefault")

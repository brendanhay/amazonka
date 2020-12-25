{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rActions,
    rConditions,
    rIsDefault,
    rPriority,
    rRuleArn,
  )
where

import qualified Network.AWS.ELBv2.Types.Action as Types
import qualified Network.AWS.ELBv2.Types.RuleArn as Types
import qualified Network.AWS.ELBv2.Types.RuleCondition as Types
import qualified Network.AWS.ELBv2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a rule.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { -- | The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
    actions :: Core.Maybe [Types.Action],
    -- | The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
    conditions :: Core.Maybe [Types.RuleCondition],
    -- | Indicates whether this is the default rule.
    isDefault :: Core.Maybe Core.Bool,
    -- | The priority.
    priority :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Core.Maybe Types.RuleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rule' value with any optional fields omitted.
mkRule ::
  Rule
mkRule =
  Rule'
    { actions = Core.Nothing,
      conditions = Core.Nothing,
      isDefault = Core.Nothing,
      priority = Core.Nothing,
      ruleArn = Core.Nothing
    }

-- | The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rActions :: Lens.Lens' Rule (Core.Maybe [Types.Action])
rActions = Lens.field @"actions"
{-# DEPRECATED rActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rConditions :: Lens.Lens' Rule (Core.Maybe [Types.RuleCondition])
rConditions = Lens.field @"conditions"
{-# DEPRECATED rConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | Indicates whether this is the default rule.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIsDefault :: Lens.Lens' Rule (Core.Maybe Core.Bool)
rIsDefault = Lens.field @"isDefault"
{-# DEPRECATED rIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

-- | The priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPriority :: Lens.Lens' Rule (Core.Maybe Types.String)
rPriority = Lens.field @"priority"
{-# DEPRECATED rPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRuleArn :: Lens.Lens' Rule (Core.Maybe Types.RuleArn)
rRuleArn = Lens.field @"ruleArn"
{-# DEPRECATED rRuleArn "Use generic-lens or generic-optics with 'ruleArn' instead." #-}

instance Core.FromXML Rule where
  parseXML x =
    Rule'
      Core.<$> (x Core..@? "Actions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Conditions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "Priority")
      Core.<*> (x Core..@? "RuleArn")

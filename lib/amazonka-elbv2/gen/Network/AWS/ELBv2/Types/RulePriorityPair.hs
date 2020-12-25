{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RulePriorityPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RulePriorityPair
  ( RulePriorityPair (..),

    -- * Smart constructor
    mkRulePriorityPair,

    -- * Lenses
    rppPriority,
    rppRuleArn,
  )
where

import qualified Network.AWS.ELBv2.Types.RuleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the priorities for the rules for a listener.
--
-- /See:/ 'mkRulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { -- | The rule priority.
    priority :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Core.Maybe Types.RuleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RulePriorityPair' value with any optional fields omitted.
mkRulePriorityPair ::
  RulePriorityPair
mkRulePriorityPair =
  RulePriorityPair'
    { priority = Core.Nothing,
      ruleArn = Core.Nothing
    }

-- | The rule priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rppPriority :: Lens.Lens' RulePriorityPair (Core.Maybe Core.Natural)
rppPriority = Lens.field @"priority"
{-# DEPRECATED rppPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rppRuleArn :: Lens.Lens' RulePriorityPair (Core.Maybe Types.RuleArn)
rppRuleArn = Lens.field @"ruleArn"
{-# DEPRECATED rppRuleArn "Use generic-lens or generic-optics with 'ruleArn' instead." #-}

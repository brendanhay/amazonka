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
    rppRuleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the priorities for the rules for a listener.
--
-- /See:/ 'mkRulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { -- | The rule priority.
    priority :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RulePriorityPair' with the minimum fields required to make a request.
--
-- * 'priority' - The rule priority.
-- * 'ruleARN' - The Amazon Resource Name (ARN) of the rule.
mkRulePriorityPair ::
  RulePriorityPair
mkRulePriorityPair =
  RulePriorityPair'
    { priority = Lude.Nothing,
      ruleARN = Lude.Nothing
    }

-- | The rule priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rppPriority :: Lens.Lens' RulePriorityPair (Lude.Maybe Lude.Natural)
rppPriority = Lens.lens (priority :: RulePriorityPair -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: RulePriorityPair)
{-# DEPRECATED rppPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rppRuleARN :: Lens.Lens' RulePriorityPair (Lude.Maybe Lude.Text)
rppRuleARN = Lens.lens (ruleARN :: RulePriorityPair -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: RulePriorityPair)
{-# DEPRECATED rppRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

instance Lude.ToQuery RulePriorityPair where
  toQuery RulePriorityPair' {..} =
    Lude.mconcat
      ["Priority" Lude.=: priority, "RuleArn" Lude.=: ruleARN]

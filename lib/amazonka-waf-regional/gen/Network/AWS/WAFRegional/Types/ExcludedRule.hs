{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ExcludedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ExcludedRule
  ( ExcludedRule (..),

    -- * Smart constructor
    mkExcludedRule,

    -- * Lenses
    erRuleId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.RuleId as Types

-- | The rule to exclude from a rule group. This is applicable only when the @ActivatedRule@ refers to a @RuleGroup@ . The rule must belong to the @RuleGroup@ that is specified by the @ActivatedRule@ .
--
-- /See:/ 'mkExcludedRule' smart constructor.
newtype ExcludedRule = ExcludedRule'
  { -- | The unique identifier for the rule to exclude from the rule group.
    ruleId :: Types.RuleId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExcludedRule' value with any optional fields omitted.
mkExcludedRule ::
  -- | 'ruleId'
  Types.RuleId ->
  ExcludedRule
mkExcludedRule ruleId = ExcludedRule' {ruleId}

-- | The unique identifier for the rule to exclude from the rule group.
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRuleId :: Lens.Lens' ExcludedRule Types.RuleId
erRuleId = Lens.field @"ruleId"
{-# DEPRECATED erRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Core.FromJSON ExcludedRule where
  toJSON ExcludedRule {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RuleId" Core..= ruleId)])

instance Core.FromJSON ExcludedRule where
  parseJSON =
    Core.withObject "ExcludedRule" Core.$
      \x -> ExcludedRule' Core.<$> (x Core..: "RuleId")

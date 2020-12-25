{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleSummary
  ( RuleSummary (..),

    -- * Smart constructor
    mkRuleSummary,

    -- * Lenses
    rsRuleId,
    rsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | Contains the identifier and the friendly name or description of the @Rule@ .
--
-- /See:/ 'mkRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
    --
    -- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
    ruleId :: Types.ResourceId,
    -- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
    name :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleSummary' value with any optional fields omitted.
mkRuleSummary ::
  -- | 'ruleId'
  Types.ResourceId ->
  -- | 'name'
  Types.ResourceName ->
  RuleSummary
mkRuleSummary ruleId name = RuleSummary' {ruleId, name}

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRuleId :: Lens.Lens' RuleSummary Types.ResourceId
rsRuleId = Lens.field @"ruleId"
{-# DEPRECATED rsRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsName :: Lens.Lens' RuleSummary Types.ResourceName
rsName = Lens.field @"name"
{-# DEPRECATED rsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON RuleSummary where
  parseJSON =
    Core.withObject "RuleSummary" Core.$
      \x ->
        RuleSummary'
          Core.<$> (x Core..: "RuleId") Core.<*> (x Core..: "Name")

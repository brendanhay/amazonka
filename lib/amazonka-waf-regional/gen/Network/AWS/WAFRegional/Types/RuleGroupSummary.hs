{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.RuleGroupSummary
  ( RuleGroupSummary (..)
  -- * Smart constructor
  , mkRuleGroupSummary
  -- * Lenses
  , rgsRuleGroupId
  , rgsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | Contains the identifier and the friendly name or description of the @RuleGroup@ .
--
-- /See:/ 'mkRuleGroupSummary' smart constructor.
data RuleGroupSummary = RuleGroupSummary'
  { ruleGroupId :: Types.ResourceId
    -- ^ A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
--
-- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
  , name :: Types.ResourceName
    -- ^ A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleGroupSummary' value with any optional fields omitted.
mkRuleGroupSummary
    :: Types.ResourceId -- ^ 'ruleGroupId'
    -> Types.ResourceName -- ^ 'name'
    -> RuleGroupSummary
mkRuleGroupSummary ruleGroupId name
  = RuleGroupSummary'{ruleGroupId, name}

-- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
--
-- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsRuleGroupId :: Lens.Lens' RuleGroupSummary Types.ResourceId
rgsRuleGroupId = Lens.field @"ruleGroupId"
{-# INLINEABLE rgsRuleGroupId #-}
{-# DEPRECATED ruleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead"  #-}

-- | A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsName :: Lens.Lens' RuleGroupSummary Types.ResourceName
rgsName = Lens.field @"name"
{-# INLINEABLE rgsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RuleGroupSummary where
        parseJSON
          = Core.withObject "RuleGroupSummary" Core.$
              \ x ->
                RuleGroupSummary' Core.<$>
                  (x Core..: "RuleGroupId") Core.<*> x Core..: "Name"

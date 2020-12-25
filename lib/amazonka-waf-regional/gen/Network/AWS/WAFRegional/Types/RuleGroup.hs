{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleGroup
  ( RuleGroup (..),

    -- * Smart constructor
    mkRuleGroup,

    -- * Lenses
    rgRuleGroupId,
    rgMetricName,
    rgName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.MetricName as Types
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | A collection of predefined rules that you can add to a web ACL.
--
-- Rule groups are subject to the following limits:
--
--     * Three rule groups per account. You can request an increase to this limit by contacting customer support.
--
--
--     * One rule group per web ACL.
--
--
--     * Ten rules per rule group.
--
--
--
-- /See:/ 'mkRuleGroup' smart constructor.
data RuleGroup = RuleGroup'
  { -- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
    --
    -- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
    ruleGroupId :: Types.ResourceId,
    -- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
    metricName :: Core.Maybe Types.MetricName,
    -- | The friendly name or description for the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
    name :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleGroup' value with any optional fields omitted.
mkRuleGroup ::
  -- | 'ruleGroupId'
  Types.ResourceId ->
  RuleGroup
mkRuleGroup ruleGroupId =
  RuleGroup'
    { ruleGroupId,
      metricName = Core.Nothing,
      name = Core.Nothing
    }

-- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
--
-- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgRuleGroupId :: Lens.Lens' RuleGroup Types.ResourceId
rgRuleGroupId = Lens.field @"ruleGroupId"
{-# DEPRECATED rgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMetricName :: Lens.Lens' RuleGroup (Core.Maybe Types.MetricName)
rgMetricName = Lens.field @"metricName"
{-# DEPRECATED rgMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The friendly name or description for the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgName :: Lens.Lens' RuleGroup (Core.Maybe Types.ResourceName)
rgName = Lens.field @"name"
{-# DEPRECATED rgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON RuleGroup where
  parseJSON =
    Core.withObject "RuleGroup" Core.$
      \x ->
        RuleGroup'
          Core.<$> (x Core..: "RuleGroupId")
          Core.<*> (x Core..:? "MetricName")
          Core.<*> (x Core..:? "Name")

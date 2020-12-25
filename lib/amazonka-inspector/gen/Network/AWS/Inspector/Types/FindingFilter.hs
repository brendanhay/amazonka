{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FindingFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FindingFilter
  ( FindingFilter (..),

    -- * Smart constructor
    mkFindingFilter,

    -- * Lenses
    ffAgentIds,
    ffAttributes,
    ffAutoScalingGroups,
    ffCreationTimeRange,
    ffRuleNames,
    ffRulesPackageArns,
    ffSeverities,
    ffUserAttributes,
  )
where

import qualified Network.AWS.Inspector.Types.AgentId as Types
import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.AutoScalingGroup as Types
import qualified Network.AWS.Inspector.Types.RuleName as Types
import qualified Network.AWS.Inspector.Types.Severity as Types
import qualified Network.AWS.Inspector.Types.TimestampRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a request parameter in the 'ListFindings' action.
--
-- /See:/ 'mkFindingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { -- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
    agentIds :: Core.Maybe [Types.AgentId],
    -- | For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
    attributes :: Core.Maybe [Types.Attribute],
    -- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
    autoScalingGroups :: Core.Maybe [Types.AutoScalingGroup],
    -- | The time range during which the finding is generated.
    creationTimeRange :: Core.Maybe Types.TimestampRange,
    -- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
    ruleNames :: Core.Maybe [Types.RuleName],
    -- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
    rulesPackageArns :: Core.Maybe [Types.Arn],
    -- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
    severities :: Core.Maybe [Types.Severity],
    -- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
    userAttributes :: Core.Maybe [Types.Attribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FindingFilter' value with any optional fields omitted.
mkFindingFilter ::
  FindingFilter
mkFindingFilter =
  FindingFilter'
    { agentIds = Core.Nothing,
      attributes = Core.Nothing,
      autoScalingGroups = Core.Nothing,
      creationTimeRange = Core.Nothing,
      ruleNames = Core.Nothing,
      rulesPackageArns = Core.Nothing,
      severities = Core.Nothing,
      userAttributes = Core.Nothing
    }

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAgentIds :: Lens.Lens' FindingFilter (Core.Maybe [Types.AgentId])
ffAgentIds = Lens.field @"agentIds"
{-# DEPRECATED ffAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

-- | For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAttributes :: Lens.Lens' FindingFilter (Core.Maybe [Types.Attribute])
ffAttributes = Lens.field @"attributes"
{-# DEPRECATED ffAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAutoScalingGroups :: Lens.Lens' FindingFilter (Core.Maybe [Types.AutoScalingGroup])
ffAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# DEPRECATED ffAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | The time range during which the finding is generated.
--
-- /Note:/ Consider using 'creationTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffCreationTimeRange :: Lens.Lens' FindingFilter (Core.Maybe Types.TimestampRange)
ffCreationTimeRange = Lens.field @"creationTimeRange"
{-# DEPRECATED ffCreationTimeRange "Use generic-lens or generic-optics with 'creationTimeRange' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffRuleNames :: Lens.Lens' FindingFilter (Core.Maybe [Types.RuleName])
ffRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED ffRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffRulesPackageArns :: Lens.Lens' FindingFilter (Core.Maybe [Types.Arn])
ffRulesPackageArns = Lens.field @"rulesPackageArns"
{-# DEPRECATED ffRulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'severities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffSeverities :: Lens.Lens' FindingFilter (Core.Maybe [Types.Severity])
ffSeverities = Lens.field @"severities"
{-# DEPRECATED ffSeverities "Use generic-lens or generic-optics with 'severities' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffUserAttributes :: Lens.Lens' FindingFilter (Core.Maybe [Types.Attribute])
ffUserAttributes = Lens.field @"userAttributes"
{-# DEPRECATED ffUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

instance Core.FromJSON FindingFilter where
  toJSON FindingFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("agentIds" Core..=) Core.<$> agentIds,
            ("attributes" Core..=) Core.<$> attributes,
            ("autoScalingGroups" Core..=) Core.<$> autoScalingGroups,
            ("creationTimeRange" Core..=) Core.<$> creationTimeRange,
            ("ruleNames" Core..=) Core.<$> ruleNames,
            ("rulesPackageArns" Core..=) Core.<$> rulesPackageArns,
            ("severities" Core..=) Core.<$> severities,
            ("userAttributes" Core..=) Core.<$> userAttributes
          ]
      )

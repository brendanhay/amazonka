{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.InsightRule
  ( InsightRule (..)
  -- * Smart constructor
  , mkInsightRule
  -- * Lenses
  , irName
  , irState
  , irSchema
  , irDefinition
  ) where

import qualified Network.AWS.CloudWatch.Types.InsightRuleDefinition as Types
import qualified Network.AWS.CloudWatch.Types.InsightRuleState as Types
import qualified Network.AWS.CloudWatch.Types.Name as Types
import qualified Network.AWS.CloudWatch.Types.Schema as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure contains the definition for a Contributor Insights rule.
--
-- /See:/ 'mkInsightRule' smart constructor.
data InsightRule = InsightRule'
  { name :: Types.Name
    -- ^ The name of the rule.
  , state :: Types.InsightRuleState
    -- ^ Indicates whether the rule is enabled or disabled.
  , schema :: Types.Schema
    -- ^ For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@ 
  , definition :: Types.InsightRuleDefinition
    -- ^ The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InsightRule' value with any optional fields omitted.
mkInsightRule
    :: Types.Name -- ^ 'name'
    -> Types.InsightRuleState -- ^ 'state'
    -> Types.Schema -- ^ 'schema'
    -> Types.InsightRuleDefinition -- ^ 'definition'
    -> InsightRule
mkInsightRule name state schema definition
  = InsightRule'{name, state, schema, definition}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irName :: Lens.Lens' InsightRule Types.Name
irName = Lens.field @"name"
{-# INLINEABLE irName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Indicates whether the rule is enabled or disabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irState :: Lens.Lens' InsightRule Types.InsightRuleState
irState = Lens.field @"state"
{-# INLINEABLE irState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@ 
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irSchema :: Lens.Lens' InsightRule Types.Schema
irSchema = Lens.field @"schema"
{-# INLINEABLE irSchema #-}
{-# DEPRECATED schema "Use generic-lens or generic-optics with 'schema' instead"  #-}

-- | The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDefinition :: Lens.Lens' InsightRule Types.InsightRuleDefinition
irDefinition = Lens.field @"definition"
{-# INLINEABLE irDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

instance Core.FromXML InsightRule where
        parseXML x
          = InsightRule' Core.<$>
              (x Core..@ "Name") Core.<*> x Core..@ "State" Core.<*>
                x Core..@ "Schema"
                Core.<*> x Core..@ "Definition"

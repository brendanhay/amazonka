{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributor
  ( InsightRuleContributor (..),

    -- * Smart constructor
    mkInsightRuleContributor,

    -- * Lenses
    ircKeys,
    ircApproximateAggregateValue,
    ircDatapoints,
  )
where

import qualified Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint as Types
import qualified Network.AWS.CloudWatch.Types.InsightRuleContributorKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | One of the unique contributors found by a Contributor Insights rule. If the rule contains multiple keys, then a unique contributor is a unique combination of values from all the keys in the rule.
--
-- If the rule contains a single key, then each unique contributor is each unique value for this key.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
-- /See:/ 'mkInsightRuleContributor' smart constructor.
data InsightRuleContributor = InsightRuleContributor'
  { -- | One of the log entry field keywords that is used to define contributors for this rule.
    keys :: [Types.InsightRuleContributorKey],
    -- | An approximation of the aggregate value that comes from this contributor.
    approximateAggregateValue :: Core.Double,
    -- | An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
    datapoints :: [Types.InsightRuleContributorDatapoint]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InsightRuleContributor' value with any optional fields omitted.
mkInsightRuleContributor ::
  -- | 'approximateAggregateValue'
  Core.Double ->
  InsightRuleContributor
mkInsightRuleContributor approximateAggregateValue =
  InsightRuleContributor'
    { keys = Core.mempty,
      approximateAggregateValue,
      datapoints = Core.mempty
    }

-- | One of the log entry field keywords that is used to define contributors for this rule.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircKeys :: Lens.Lens' InsightRuleContributor [Types.InsightRuleContributorKey]
ircKeys = Lens.field @"keys"
{-# DEPRECATED ircKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | An approximation of the aggregate value that comes from this contributor.
--
-- /Note:/ Consider using 'approximateAggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircApproximateAggregateValue :: Lens.Lens' InsightRuleContributor Core.Double
ircApproximateAggregateValue = Lens.field @"approximateAggregateValue"
{-# DEPRECATED ircApproximateAggregateValue "Use generic-lens or generic-optics with 'approximateAggregateValue' instead." #-}

-- | An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
--
-- /Note:/ Consider using 'datapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircDatapoints :: Lens.Lens' InsightRuleContributor [Types.InsightRuleContributorDatapoint]
ircDatapoints = Lens.field @"datapoints"
{-# DEPRECATED ircDatapoints "Use generic-lens or generic-optics with 'datapoints' instead." #-}

instance Core.FromXML InsightRuleContributor where
  parseXML x =
    InsightRuleContributor'
      Core.<$> ( x Core..@? "Keys" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@ "ApproximateAggregateValue")
      Core.<*> ( x Core..@? "Datapoints" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "member"
               )

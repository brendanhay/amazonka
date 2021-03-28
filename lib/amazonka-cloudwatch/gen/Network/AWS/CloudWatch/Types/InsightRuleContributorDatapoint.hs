{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
  ( InsightRuleContributorDatapoint (..)
  -- * Smart constructor
  , mkInsightRuleContributorDatapoint
  -- * Lenses
  , ircdTimestamp
  , ircdApproximateValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | One data point related to one contributor.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> and <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_InsightRuleContributor.html InsightRuleContributor> .
--
-- /See:/ 'mkInsightRuleContributorDatapoint' smart constructor.
data InsightRuleContributorDatapoint = InsightRuleContributorDatapoint'
  { timestamp :: Core.UTCTime
    -- ^ The timestamp of the data point.
  , approximateValue :: Core.Double
    -- ^ The approximate value that this contributor added during this timestamp.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InsightRuleContributorDatapoint' value with any optional fields omitted.
mkInsightRuleContributorDatapoint
    :: Core.UTCTime -- ^ 'timestamp'
    -> Core.Double -- ^ 'approximateValue'
    -> InsightRuleContributorDatapoint
mkInsightRuleContributorDatapoint timestamp approximateValue
  = InsightRuleContributorDatapoint'{timestamp, approximateValue}

-- | The timestamp of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdTimestamp :: Lens.Lens' InsightRuleContributorDatapoint Core.UTCTime
ircdTimestamp = Lens.field @"timestamp"
{-# INLINEABLE ircdTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The approximate value that this contributor added during this timestamp.
--
-- /Note:/ Consider using 'approximateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdApproximateValue :: Lens.Lens' InsightRuleContributorDatapoint Core.Double
ircdApproximateValue = Lens.field @"approximateValue"
{-# INLINEABLE ircdApproximateValue #-}
{-# DEPRECATED approximateValue "Use generic-lens or generic-optics with 'approximateValue' instead"  #-}

instance Core.FromXML InsightRuleContributorDatapoint where
        parseXML x
          = InsightRuleContributorDatapoint' Core.<$>
              (x Core..@ "Timestamp") Core.<*> x Core..@ "ApproximateValue"

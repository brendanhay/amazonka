-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
  ( InsightRuleContributorDatapoint (..),

    -- * Smart constructor
    mkInsightRuleContributorDatapoint,

    -- * Lenses
    ircdTimestamp,
    ircdApproximateValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One data point related to one contributor.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> and <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_InsightRuleContributor.html InsightRuleContributor> .
--
-- /See:/ 'mkInsightRuleContributorDatapoint' smart constructor.
data InsightRuleContributorDatapoint = InsightRuleContributorDatapoint'
  { timestamp ::
      Lude.ISO8601,
    approximateValue ::
      Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightRuleContributorDatapoint' with the minimum fields required to make a request.
--
-- * 'approximateValue' - The approximate value that this contributor added during this timestamp.
-- * 'timestamp' - The timestamp of the data point.
mkInsightRuleContributorDatapoint ::
  -- | 'timestamp'
  Lude.ISO8601 ->
  -- | 'approximateValue'
  Lude.Double ->
  InsightRuleContributorDatapoint
mkInsightRuleContributorDatapoint pTimestamp_ pApproximateValue_ =
  InsightRuleContributorDatapoint'
    { timestamp = pTimestamp_,
      approximateValue = pApproximateValue_
    }

-- | The timestamp of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdTimestamp :: Lens.Lens' InsightRuleContributorDatapoint Lude.ISO8601
ircdTimestamp = Lens.lens (timestamp :: InsightRuleContributorDatapoint -> Lude.ISO8601) (\s a -> s {timestamp = a} :: InsightRuleContributorDatapoint)
{-# DEPRECATED ircdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The approximate value that this contributor added during this timestamp.
--
-- /Note:/ Consider using 'approximateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdApproximateValue :: Lens.Lens' InsightRuleContributorDatapoint Lude.Double
ircdApproximateValue = Lens.lens (approximateValue :: InsightRuleContributorDatapoint -> Lude.Double) (\s a -> s {approximateValue = a} :: InsightRuleContributorDatapoint)
{-# DEPRECATED ircdApproximateValue "Use generic-lens or generic-optics with 'approximateValue' instead." #-}

instance Lude.FromXML InsightRuleContributorDatapoint where
  parseXML x =
    InsightRuleContributorDatapoint'
      Lude.<$> (x Lude..@ "Timestamp") Lude.<*> (x Lude..@ "ApproximateValue")

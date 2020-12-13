{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ircdApproximateValue,
    ircdTimestamp,
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
  { -- | The approximate value that this contributor added during this timestamp.
    approximateValue :: Lude.Double,
    -- | The timestamp of the data point.
    timestamp :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightRuleContributorDatapoint' with the minimum fields required to make a request.
--
-- * 'approximateValue' - The approximate value that this contributor added during this timestamp.
-- * 'timestamp' - The timestamp of the data point.
mkInsightRuleContributorDatapoint ::
  -- | 'approximateValue'
  Lude.Double ->
  -- | 'timestamp'
  Lude.DateTime ->
  InsightRuleContributorDatapoint
mkInsightRuleContributorDatapoint pApproximateValue_ pTimestamp_ =
  InsightRuleContributorDatapoint'
    { approximateValue =
        pApproximateValue_,
      timestamp = pTimestamp_
    }

-- | The approximate value that this contributor added during this timestamp.
--
-- /Note:/ Consider using 'approximateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdApproximateValue :: Lens.Lens' InsightRuleContributorDatapoint Lude.Double
ircdApproximateValue = Lens.lens (approximateValue :: InsightRuleContributorDatapoint -> Lude.Double) (\s a -> s {approximateValue = a} :: InsightRuleContributorDatapoint)
{-# DEPRECATED ircdApproximateValue "Use generic-lens or generic-optics with 'approximateValue' instead." #-}

-- | The timestamp of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircdTimestamp :: Lens.Lens' InsightRuleContributorDatapoint Lude.DateTime
ircdTimestamp = Lens.lens (timestamp :: InsightRuleContributorDatapoint -> Lude.DateTime) (\s a -> s {timestamp = a} :: InsightRuleContributorDatapoint)
{-# DEPRECATED ircdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML InsightRuleContributorDatapoint where
  parseXML x =
    InsightRuleContributorDatapoint'
      Lude.<$> (x Lude..@ "ApproximateValue") Lude.<*> (x Lude..@ "Timestamp")

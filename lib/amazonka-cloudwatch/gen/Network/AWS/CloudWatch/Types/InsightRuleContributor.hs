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
    ircDatapoints,
    ircApproximateAggregateValue,
    ircKeys,
  )
where

import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One of the unique contributors found by a Contributor Insights rule. If the rule contains multiple keys, then a unique contributor is a unique combination of values from all the keys in the rule.
--
-- If the rule contains a single key, then each unique contributor is each unique value for this key.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
-- /See:/ 'mkInsightRuleContributor' smart constructor.
data InsightRuleContributor = InsightRuleContributor'
  { -- | An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
    datapoints :: [InsightRuleContributorDatapoint],
    -- | An approximation of the aggregate value that comes from this contributor.
    approximateAggregateValue :: Lude.Double,
    -- | One of the log entry field keywords that is used to define contributors for this rule.
    keys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightRuleContributor' with the minimum fields required to make a request.
--
-- * 'datapoints' - An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
-- * 'approximateAggregateValue' - An approximation of the aggregate value that comes from this contributor.
-- * 'keys' - One of the log entry field keywords that is used to define contributors for this rule.
mkInsightRuleContributor ::
  -- | 'approximateAggregateValue'
  Lude.Double ->
  InsightRuleContributor
mkInsightRuleContributor pApproximateAggregateValue_ =
  InsightRuleContributor'
    { datapoints = Lude.mempty,
      approximateAggregateValue = pApproximateAggregateValue_,
      keys = Lude.mempty
    }

-- | An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
--
-- /Note:/ Consider using 'datapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircDatapoints :: Lens.Lens' InsightRuleContributor [InsightRuleContributorDatapoint]
ircDatapoints = Lens.lens (datapoints :: InsightRuleContributor -> [InsightRuleContributorDatapoint]) (\s a -> s {datapoints = a} :: InsightRuleContributor)
{-# DEPRECATED ircDatapoints "Use generic-lens or generic-optics with 'datapoints' instead." #-}

-- | An approximation of the aggregate value that comes from this contributor.
--
-- /Note:/ Consider using 'approximateAggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircApproximateAggregateValue :: Lens.Lens' InsightRuleContributor Lude.Double
ircApproximateAggregateValue = Lens.lens (approximateAggregateValue :: InsightRuleContributor -> Lude.Double) (\s a -> s {approximateAggregateValue = a} :: InsightRuleContributor)
{-# DEPRECATED ircApproximateAggregateValue "Use generic-lens or generic-optics with 'approximateAggregateValue' instead." #-}

-- | One of the log entry field keywords that is used to define contributors for this rule.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircKeys :: Lens.Lens' InsightRuleContributor [Lude.Text]
ircKeys = Lens.lens (keys :: InsightRuleContributor -> [Lude.Text]) (\s a -> s {keys = a} :: InsightRuleContributor)
{-# DEPRECATED ircKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

instance Lude.FromXML InsightRuleContributor where
  parseXML x =
    InsightRuleContributor'
      Lude.<$> ( x Lude..@? "Datapoints" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "member"
               )
      Lude.<*> (x Lude..@ "ApproximateAggregateValue")
      Lude.<*> ( x Lude..@? "Keys" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "member"
               )

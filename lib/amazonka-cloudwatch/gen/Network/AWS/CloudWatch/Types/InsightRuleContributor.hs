{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributor where

import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import Network.AWS.Lens
import Network.AWS.Prelude

-- | One of the unique contributors found by a Contributor Insights rule. If the rule contains multiple keys, then a unique contributor is a unique combination of values from all the keys in the rule.
--
--
-- If the rule contains a single key, then each unique contributor is each unique value for this key.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
--
-- /See:/ 'insightRuleContributor' smart constructor.
data InsightRuleContributor = InsightRuleContributor'
  { _ircKeys ::
      ![Text],
    _ircApproximateAggregateValue :: !Double,
    _ircDatapoints ::
      ![InsightRuleContributorDatapoint]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightRuleContributor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ircKeys' - One of the log entry field keywords that is used to define contributors for this rule.
--
-- * 'ircApproximateAggregateValue' - An approximation of the aggregate value that comes from this contributor.
--
-- * 'ircDatapoints' - An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
insightRuleContributor ::
  -- | 'ircApproximateAggregateValue'
  Double ->
  InsightRuleContributor
insightRuleContributor pApproximateAggregateValue_ =
  InsightRuleContributor'
    { _ircKeys = mempty,
      _ircApproximateAggregateValue = pApproximateAggregateValue_,
      _ircDatapoints = mempty
    }

-- | One of the log entry field keywords that is used to define contributors for this rule.
ircKeys :: Lens' InsightRuleContributor [Text]
ircKeys = lens _ircKeys (\s a -> s {_ircKeys = a}) . _Coerce

-- | An approximation of the aggregate value that comes from this contributor.
ircApproximateAggregateValue :: Lens' InsightRuleContributor Double
ircApproximateAggregateValue = lens _ircApproximateAggregateValue (\s a -> s {_ircApproximateAggregateValue = a})

-- | An array of the data points where this contributor is present. Only the data points when this contributor appeared are included in the array.
ircDatapoints :: Lens' InsightRuleContributor [InsightRuleContributorDatapoint]
ircDatapoints = lens _ircDatapoints (\s a -> s {_ircDatapoints = a}) . _Coerce

instance FromXML InsightRuleContributor where
  parseXML x =
    InsightRuleContributor'
      <$> (x .@? "Keys" .!@ mempty >>= parseXMLList "member")
      <*> (x .@ "ApproximateAggregateValue")
      <*> (x .@? "Datapoints" .!@ mempty >>= parseXMLList "member")

instance Hashable InsightRuleContributor

instance NFData InsightRuleContributor

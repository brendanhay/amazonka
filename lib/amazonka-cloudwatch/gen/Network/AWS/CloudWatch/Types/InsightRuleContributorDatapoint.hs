{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One data point related to one contributor.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> and <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_InsightRuleContributor.html InsightRuleContributor> .
--
--
-- /See:/ 'insightRuleContributorDatapoint' smart constructor.
data InsightRuleContributorDatapoint = InsightRuleContributorDatapoint'
  { _ircdTimestamp ::
      !ISO8601,
    _ircdApproximateValue ::
      !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightRuleContributorDatapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ircdTimestamp' - The timestamp of the data point.
--
-- * 'ircdApproximateValue' - The approximate value that this contributor added during this timestamp.
insightRuleContributorDatapoint ::
  -- | 'ircdTimestamp'
  UTCTime ->
  -- | 'ircdApproximateValue'
  Double ->
  InsightRuleContributorDatapoint
insightRuleContributorDatapoint pTimestamp_ pApproximateValue_ =
  InsightRuleContributorDatapoint'
    { _ircdTimestamp =
        _Time # pTimestamp_,
      _ircdApproximateValue = pApproximateValue_
    }

-- | The timestamp of the data point.
ircdTimestamp :: Lens' InsightRuleContributorDatapoint UTCTime
ircdTimestamp = lens _ircdTimestamp (\s a -> s {_ircdTimestamp = a}) . _Time

-- | The approximate value that this contributor added during this timestamp.
ircdApproximateValue :: Lens' InsightRuleContributorDatapoint Double
ircdApproximateValue = lens _ircdApproximateValue (\s a -> s {_ircdApproximateValue = a})

instance FromXML InsightRuleContributorDatapoint where
  parseXML x =
    InsightRuleContributorDatapoint'
      <$> (x .@ "Timestamp") <*> (x .@ "ApproximateValue")

instance Hashable InsightRuleContributorDatapoint

instance NFData InsightRuleContributorDatapoint

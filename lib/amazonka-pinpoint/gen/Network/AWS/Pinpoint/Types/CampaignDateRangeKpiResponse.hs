{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to a campaign, and provides information about that query.
--
--
--
-- /See:/ 'campaignDateRangeKpiResponse' smart constructor.
data CampaignDateRangeKpiResponse = CampaignDateRangeKpiResponse'
  { _cdrkNextToken ::
      !(Maybe Text),
    _cdrkKpiResult :: !BaseKpiResult,
    _cdrkKpiName :: !Text,
    _cdrkEndTime :: !POSIX,
    _cdrkCampaignId :: !Text,
    _cdrkStartTime :: !POSIX,
    _cdrkApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrkNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Campaign Metrics resource because the resource returns all results in a single page.
--
-- * 'cdrkKpiResult' - An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- * 'cdrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'cdrkEndTime' - The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'cdrkCampaignId' - The unique identifier for the campaign that the metric applies to.
--
-- * 'cdrkStartTime' - The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'cdrkApplicationId' - The unique identifier for the application that the metric applies to.
campaignDateRangeKpiResponse ::
  -- | 'cdrkKpiResult'
  BaseKpiResult ->
  -- | 'cdrkKpiName'
  Text ->
  -- | 'cdrkEndTime'
  UTCTime ->
  -- | 'cdrkCampaignId'
  Text ->
  -- | 'cdrkStartTime'
  UTCTime ->
  -- | 'cdrkApplicationId'
  Text ->
  CampaignDateRangeKpiResponse
campaignDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pEndTime_
  pCampaignId_
  pStartTime_
  pApplicationId_ =
    CampaignDateRangeKpiResponse'
      { _cdrkNextToken = Nothing,
        _cdrkKpiResult = pKpiResult_,
        _cdrkKpiName = pKpiName_,
        _cdrkEndTime = _Time # pEndTime_,
        _cdrkCampaignId = pCampaignId_,
        _cdrkStartTime = _Time # pStartTime_,
        _cdrkApplicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Campaign Metrics resource because the resource returns all results in a single page.
cdrkNextToken :: Lens' CampaignDateRangeKpiResponse (Maybe Text)
cdrkNextToken = lens _cdrkNextToken (\s a -> s {_cdrkNextToken = a})

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
cdrkKpiResult :: Lens' CampaignDateRangeKpiResponse BaseKpiResult
cdrkKpiResult = lens _cdrkKpiResult (\s a -> s {_cdrkKpiResult = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
cdrkKpiName :: Lens' CampaignDateRangeKpiResponse Text
cdrkKpiName = lens _cdrkKpiName (\s a -> s {_cdrkKpiName = a})

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
cdrkEndTime :: Lens' CampaignDateRangeKpiResponse UTCTime
cdrkEndTime = lens _cdrkEndTime (\s a -> s {_cdrkEndTime = a}) . _Time

-- | The unique identifier for the campaign that the metric applies to.
cdrkCampaignId :: Lens' CampaignDateRangeKpiResponse Text
cdrkCampaignId = lens _cdrkCampaignId (\s a -> s {_cdrkCampaignId = a})

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
cdrkStartTime :: Lens' CampaignDateRangeKpiResponse UTCTime
cdrkStartTime = lens _cdrkStartTime (\s a -> s {_cdrkStartTime = a}) . _Time

-- | The unique identifier for the application that the metric applies to.
cdrkApplicationId :: Lens' CampaignDateRangeKpiResponse Text
cdrkApplicationId = lens _cdrkApplicationId (\s a -> s {_cdrkApplicationId = a})

instance FromJSON CampaignDateRangeKpiResponse where
  parseJSON =
    withObject
      "CampaignDateRangeKpiResponse"
      ( \x ->
          CampaignDateRangeKpiResponse'
            <$> (x .:? "NextToken")
            <*> (x .: "KpiResult")
            <*> (x .: "KpiName")
            <*> (x .: "EndTime")
            <*> (x .: "CampaignId")
            <*> (x .: "StartTime")
            <*> (x .: "ApplicationId")
      )

instance Hashable CampaignDateRangeKpiResponse

instance NFData CampaignDateRangeKpiResponse

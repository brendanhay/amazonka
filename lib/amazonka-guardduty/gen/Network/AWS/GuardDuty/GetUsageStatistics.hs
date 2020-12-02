{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetUsageStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty usage statistics over the last 30 days for the specified detector ID. For newly enabled detectors or data sources the cost returned will include only the usage so far under 30 days, this may differ from the cost metrics in the console, which projects usage over 30 days to provide a monthly cost estimate. For more information see <https://docs.aws.amazon.com/guardduty/latest/ug/monitoring_costs.html#usage-calculations Understanding How Usage Costs are Calculated> .
module Network.AWS.GuardDuty.GetUsageStatistics
  ( -- * Creating a Request
    getUsageStatistics,
    GetUsageStatistics,

    -- * Request Lenses
    gusNextToken,
    gusUnit,
    gusMaxResults,
    gusDetectorId,
    gusUsageStatisticType,
    gusUsageCriteria,

    -- * Destructuring the Response
    getUsageStatisticsResponse,
    GetUsageStatisticsResponse,

    -- * Response Lenses
    gusrsUsageStatistics,
    gusrsNextToken,
    gusrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { _gusNextToken ::
      !(Maybe Text),
    _gusUnit :: !(Maybe Text),
    _gusMaxResults :: !(Maybe Nat),
    _gusDetectorId :: !Text,
    _gusUsageStatisticType :: !UsageStatisticType,
    _gusUsageCriteria :: !UsageCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUsageStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gusNextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
--
-- * 'gusUnit' - The currency unit you would like to view your usage statistics in. Current valid values are USD.
--
-- * 'gusMaxResults' - The maximum number of results to return in the response.
--
-- * 'gusDetectorId' - The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
--
-- * 'gusUsageStatisticType' - The type of usage statistics to retrieve.
--
-- * 'gusUsageCriteria' - Represents the criteria used for querying usage.
getUsageStatistics ::
  -- | 'gusDetectorId'
  Text ->
  -- | 'gusUsageStatisticType'
  UsageStatisticType ->
  -- | 'gusUsageCriteria'
  UsageCriteria ->
  GetUsageStatistics
getUsageStatistics
  pDetectorId_
  pUsageStatisticType_
  pUsageCriteria_ =
    GetUsageStatistics'
      { _gusNextToken = Nothing,
        _gusUnit = Nothing,
        _gusMaxResults = Nothing,
        _gusDetectorId = pDetectorId_,
        _gusUsageStatisticType = pUsageStatisticType_,
        _gusUsageCriteria = pUsageCriteria_
      }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
gusNextToken :: Lens' GetUsageStatistics (Maybe Text)
gusNextToken = lens _gusNextToken (\s a -> s {_gusNextToken = a})

-- | The currency unit you would like to view your usage statistics in. Current valid values are USD.
gusUnit :: Lens' GetUsageStatistics (Maybe Text)
gusUnit = lens _gusUnit (\s a -> s {_gusUnit = a})

-- | The maximum number of results to return in the response.
gusMaxResults :: Lens' GetUsageStatistics (Maybe Natural)
gusMaxResults = lens _gusMaxResults (\s a -> s {_gusMaxResults = a}) . mapping _Nat

-- | The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
gusDetectorId :: Lens' GetUsageStatistics Text
gusDetectorId = lens _gusDetectorId (\s a -> s {_gusDetectorId = a})

-- | The type of usage statistics to retrieve.
gusUsageStatisticType :: Lens' GetUsageStatistics UsageStatisticType
gusUsageStatisticType = lens _gusUsageStatisticType (\s a -> s {_gusUsageStatisticType = a})

-- | Represents the criteria used for querying usage.
gusUsageCriteria :: Lens' GetUsageStatistics UsageCriteria
gusUsageCriteria = lens _gusUsageCriteria (\s a -> s {_gusUsageCriteria = a})

instance AWSRequest GetUsageStatistics where
  type Rs GetUsageStatistics = GetUsageStatisticsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetUsageStatisticsResponse'
            <$> (x .?> "usageStatistics")
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetUsageStatistics

instance NFData GetUsageStatistics

instance ToHeaders GetUsageStatistics where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetUsageStatistics where
  toJSON GetUsageStatistics' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _gusNextToken,
            ("unit" .=) <$> _gusUnit,
            ("maxResults" .=) <$> _gusMaxResults,
            Just ("usageStatisticsType" .= _gusUsageStatisticType),
            Just ("usageCriteria" .= _gusUsageCriteria)
          ]
      )

instance ToPath GetUsageStatistics where
  toPath GetUsageStatistics' {..} =
    mconcat ["/detector/", toBS _gusDetectorId, "/usage/statistics"]

instance ToQuery GetUsageStatistics where
  toQuery = const mempty

-- | /See:/ 'getUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { _gusrsUsageStatistics ::
      !(Maybe UsageStatistics),
    _gusrsNextToken :: !(Maybe Text),
    _gusrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUsageStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gusrsUsageStatistics' - The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
--
-- * 'gusrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'gusrsResponseStatus' - -- | The response status code.
getUsageStatisticsResponse ::
  -- | 'gusrsResponseStatus'
  Int ->
  GetUsageStatisticsResponse
getUsageStatisticsResponse pResponseStatus_ =
  GetUsageStatisticsResponse'
    { _gusrsUsageStatistics = Nothing,
      _gusrsNextToken = Nothing,
      _gusrsResponseStatus = pResponseStatus_
    }

-- | The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
gusrsUsageStatistics :: Lens' GetUsageStatisticsResponse (Maybe UsageStatistics)
gusrsUsageStatistics = lens _gusrsUsageStatistics (\s a -> s {_gusrsUsageStatistics = a})

-- | The pagination parameter to be used on the next list operation to retrieve more items.
gusrsNextToken :: Lens' GetUsageStatisticsResponse (Maybe Text)
gusrsNextToken = lens _gusrsNextToken (\s a -> s {_gusrsNextToken = a})

-- | -- | The response status code.
gusrsResponseStatus :: Lens' GetUsageStatisticsResponse Int
gusrsResponseStatus = lens _gusrsResponseStatus (\s a -> s {_gusrsResponseStatus = a})

instance NFData GetUsageStatisticsResponse

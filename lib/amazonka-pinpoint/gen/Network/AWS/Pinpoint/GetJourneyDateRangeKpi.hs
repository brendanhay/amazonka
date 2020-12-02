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
-- Module      : Network.AWS.Pinpoint.GetJourneyDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard engagement metric that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyDateRangeKpi
  ( -- * Creating a Request
    getJourneyDateRangeKpi,
    GetJourneyDateRangeKpi,

    -- * Request Lenses
    gjdrkStartTime,
    gjdrkNextToken,
    gjdrkEndTime,
    gjdrkPageSize,
    gjdrkJourneyId,
    gjdrkApplicationId,
    gjdrkKpiName,

    -- * Destructuring the Response
    getJourneyDateRangeKpiResponse,
    GetJourneyDateRangeKpiResponse,

    -- * Response Lenses
    gjdrkrsResponseStatus,
    gjdrkrsJourneyDateRangeKpiResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJourneyDateRangeKpi' smart constructor.
data GetJourneyDateRangeKpi = GetJourneyDateRangeKpi'
  { _gjdrkStartTime ::
      !(Maybe POSIX),
    _gjdrkNextToken :: !(Maybe Text),
    _gjdrkEndTime :: !(Maybe POSIX),
    _gjdrkPageSize :: !(Maybe Text),
    _gjdrkJourneyId :: !Text,
    _gjdrkApplicationId :: !Text,
    _gjdrkKpiName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJourneyDateRangeKpi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdrkStartTime' - The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- * 'gjdrkNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjdrkEndTime' - The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- * 'gjdrkPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjdrkJourneyId' - The unique identifier for the journey.
--
-- * 'gjdrkApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gjdrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
getJourneyDateRangeKpi ::
  -- | 'gjdrkJourneyId'
  Text ->
  -- | 'gjdrkApplicationId'
  Text ->
  -- | 'gjdrkKpiName'
  Text ->
  GetJourneyDateRangeKpi
getJourneyDateRangeKpi pJourneyId_ pApplicationId_ pKpiName_ =
  GetJourneyDateRangeKpi'
    { _gjdrkStartTime = Nothing,
      _gjdrkNextToken = Nothing,
      _gjdrkEndTime = Nothing,
      _gjdrkPageSize = Nothing,
      _gjdrkJourneyId = pJourneyId_,
      _gjdrkApplicationId = pApplicationId_,
      _gjdrkKpiName = pKpiName_
    }

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
gjdrkStartTime :: Lens' GetJourneyDateRangeKpi (Maybe UTCTime)
gjdrkStartTime = lens _gjdrkStartTime (\s a -> s {_gjdrkStartTime = a}) . mapping _Time

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjdrkNextToken :: Lens' GetJourneyDateRangeKpi (Maybe Text)
gjdrkNextToken = lens _gjdrkNextToken (\s a -> s {_gjdrkNextToken = a})

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
gjdrkEndTime :: Lens' GetJourneyDateRangeKpi (Maybe UTCTime)
gjdrkEndTime = lens _gjdrkEndTime (\s a -> s {_gjdrkEndTime = a}) . mapping _Time

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjdrkPageSize :: Lens' GetJourneyDateRangeKpi (Maybe Text)
gjdrkPageSize = lens _gjdrkPageSize (\s a -> s {_gjdrkPageSize = a})

-- | The unique identifier for the journey.
gjdrkJourneyId :: Lens' GetJourneyDateRangeKpi Text
gjdrkJourneyId = lens _gjdrkJourneyId (\s a -> s {_gjdrkJourneyId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gjdrkApplicationId :: Lens' GetJourneyDateRangeKpi Text
gjdrkApplicationId = lens _gjdrkApplicationId (\s a -> s {_gjdrkApplicationId = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
gjdrkKpiName :: Lens' GetJourneyDateRangeKpi Text
gjdrkKpiName = lens _gjdrkKpiName (\s a -> s {_gjdrkKpiName = a})

instance AWSRequest GetJourneyDateRangeKpi where
  type Rs GetJourneyDateRangeKpi = GetJourneyDateRangeKpiResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetJourneyDateRangeKpiResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetJourneyDateRangeKpi

instance NFData GetJourneyDateRangeKpi

instance ToHeaders GetJourneyDateRangeKpi where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetJourneyDateRangeKpi where
  toPath GetJourneyDateRangeKpi' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gjdrkApplicationId,
        "/journeys/",
        toBS _gjdrkJourneyId,
        "/kpis/daterange/",
        toBS _gjdrkKpiName
      ]

instance ToQuery GetJourneyDateRangeKpi where
  toQuery GetJourneyDateRangeKpi' {..} =
    mconcat
      [ "start-time" =: _gjdrkStartTime,
        "next-token" =: _gjdrkNextToken,
        "end-time" =: _gjdrkEndTime,
        "page-size" =: _gjdrkPageSize
      ]

-- | /See:/ 'getJourneyDateRangeKpiResponse' smart constructor.
data GetJourneyDateRangeKpiResponse = GetJourneyDateRangeKpiResponse'
  { _gjdrkrsResponseStatus ::
      !Int,
    _gjdrkrsJourneyDateRangeKpiResponse ::
      !JourneyDateRangeKpiResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJourneyDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdrkrsResponseStatus' - -- | The response status code.
--
-- * 'gjdrkrsJourneyDateRangeKpiResponse' - Undocumented member.
getJourneyDateRangeKpiResponse ::
  -- | 'gjdrkrsResponseStatus'
  Int ->
  -- | 'gjdrkrsJourneyDateRangeKpiResponse'
  JourneyDateRangeKpiResponse ->
  GetJourneyDateRangeKpiResponse
getJourneyDateRangeKpiResponse
  pResponseStatus_
  pJourneyDateRangeKpiResponse_ =
    GetJourneyDateRangeKpiResponse'
      { _gjdrkrsResponseStatus =
          pResponseStatus_,
        _gjdrkrsJourneyDateRangeKpiResponse =
          pJourneyDateRangeKpiResponse_
      }

-- | -- | The response status code.
gjdrkrsResponseStatus :: Lens' GetJourneyDateRangeKpiResponse Int
gjdrkrsResponseStatus = lens _gjdrkrsResponseStatus (\s a -> s {_gjdrkrsResponseStatus = a})

-- | Undocumented member.
gjdrkrsJourneyDateRangeKpiResponse :: Lens' GetJourneyDateRangeKpiResponse JourneyDateRangeKpiResponse
gjdrkrsJourneyDateRangeKpiResponse = lens _gjdrkrsJourneyDateRangeKpiResponse (\s a -> s {_gjdrkrsJourneyDateRangeKpiResponse = a})

instance NFData GetJourneyDateRangeKpiResponse

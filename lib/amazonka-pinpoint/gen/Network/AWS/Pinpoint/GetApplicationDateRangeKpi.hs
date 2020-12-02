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
-- Module      : Network.AWS.Pinpoint.GetApplicationDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that applies to an application.
module Network.AWS.Pinpoint.GetApplicationDateRangeKpi
  ( -- * Creating a Request
    getApplicationDateRangeKpi,
    GetApplicationDateRangeKpi,

    -- * Request Lenses
    gadrkStartTime,
    gadrkNextToken,
    gadrkEndTime,
    gadrkPageSize,
    gadrkApplicationId,
    gadrkKpiName,

    -- * Destructuring the Response
    getApplicationDateRangeKpiResponse,
    GetApplicationDateRangeKpiResponse,

    -- * Response Lenses
    gadrkrsResponseStatus,
    gadrkrsApplicationDateRangeKpiResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getApplicationDateRangeKpi' smart constructor.
data GetApplicationDateRangeKpi = GetApplicationDateRangeKpi'
  { _gadrkStartTime ::
      !(Maybe POSIX),
    _gadrkNextToken :: !(Maybe Text),
    _gadrkEndTime :: !(Maybe POSIX),
    _gadrkPageSize :: !(Maybe Text),
    _gadrkApplicationId :: !Text,
    _gadrkKpiName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetApplicationDateRangeKpi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadrkStartTime' - The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- * 'gadrkNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gadrkEndTime' - The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- * 'gadrkPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gadrkApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gadrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
getApplicationDateRangeKpi ::
  -- | 'gadrkApplicationId'
  Text ->
  -- | 'gadrkKpiName'
  Text ->
  GetApplicationDateRangeKpi
getApplicationDateRangeKpi pApplicationId_ pKpiName_ =
  GetApplicationDateRangeKpi'
    { _gadrkStartTime = Nothing,
      _gadrkNextToken = Nothing,
      _gadrkEndTime = Nothing,
      _gadrkPageSize = Nothing,
      _gadrkApplicationId = pApplicationId_,
      _gadrkKpiName = pKpiName_
    }

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
gadrkStartTime :: Lens' GetApplicationDateRangeKpi (Maybe UTCTime)
gadrkStartTime = lens _gadrkStartTime (\s a -> s {_gadrkStartTime = a}) . mapping _Time

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gadrkNextToken :: Lens' GetApplicationDateRangeKpi (Maybe Text)
gadrkNextToken = lens _gadrkNextToken (\s a -> s {_gadrkNextToken = a})

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
gadrkEndTime :: Lens' GetApplicationDateRangeKpi (Maybe UTCTime)
gadrkEndTime = lens _gadrkEndTime (\s a -> s {_gadrkEndTime = a}) . mapping _Time

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gadrkPageSize :: Lens' GetApplicationDateRangeKpi (Maybe Text)
gadrkPageSize = lens _gadrkPageSize (\s a -> s {_gadrkPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gadrkApplicationId :: Lens' GetApplicationDateRangeKpi Text
gadrkApplicationId = lens _gadrkApplicationId (\s a -> s {_gadrkApplicationId = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
gadrkKpiName :: Lens' GetApplicationDateRangeKpi Text
gadrkKpiName = lens _gadrkKpiName (\s a -> s {_gadrkKpiName = a})

instance AWSRequest GetApplicationDateRangeKpi where
  type
    Rs GetApplicationDateRangeKpi =
      GetApplicationDateRangeKpiResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetApplicationDateRangeKpiResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetApplicationDateRangeKpi

instance NFData GetApplicationDateRangeKpi

instance ToHeaders GetApplicationDateRangeKpi where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetApplicationDateRangeKpi where
  toPath GetApplicationDateRangeKpi' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gadrkApplicationId,
        "/kpis/daterange/",
        toBS _gadrkKpiName
      ]

instance ToQuery GetApplicationDateRangeKpi where
  toQuery GetApplicationDateRangeKpi' {..} =
    mconcat
      [ "start-time" =: _gadrkStartTime,
        "next-token" =: _gadrkNextToken,
        "end-time" =: _gadrkEndTime,
        "page-size" =: _gadrkPageSize
      ]

-- | /See:/ 'getApplicationDateRangeKpiResponse' smart constructor.
data GetApplicationDateRangeKpiResponse = GetApplicationDateRangeKpiResponse'
  { _gadrkrsResponseStatus ::
      !Int,
    _gadrkrsApplicationDateRangeKpiResponse ::
      !ApplicationDateRangeKpiResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetApplicationDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadrkrsResponseStatus' - -- | The response status code.
--
-- * 'gadrkrsApplicationDateRangeKpiResponse' - Undocumented member.
getApplicationDateRangeKpiResponse ::
  -- | 'gadrkrsResponseStatus'
  Int ->
  -- | 'gadrkrsApplicationDateRangeKpiResponse'
  ApplicationDateRangeKpiResponse ->
  GetApplicationDateRangeKpiResponse
getApplicationDateRangeKpiResponse
  pResponseStatus_
  pApplicationDateRangeKpiResponse_ =
    GetApplicationDateRangeKpiResponse'
      { _gadrkrsResponseStatus =
          pResponseStatus_,
        _gadrkrsApplicationDateRangeKpiResponse =
          pApplicationDateRangeKpiResponse_
      }

-- | -- | The response status code.
gadrkrsResponseStatus :: Lens' GetApplicationDateRangeKpiResponse Int
gadrkrsResponseStatus = lens _gadrkrsResponseStatus (\s a -> s {_gadrkrsResponseStatus = a})

-- | Undocumented member.
gadrkrsApplicationDateRangeKpiResponse :: Lens' GetApplicationDateRangeKpiResponse ApplicationDateRangeKpiResponse
gadrkrsApplicationDateRangeKpiResponse = lens _gadrkrsApplicationDateRangeKpiResponse (\s a -> s {_gadrkrsApplicationDateRangeKpiResponse = a})

instance NFData GetApplicationDateRangeKpiResponse

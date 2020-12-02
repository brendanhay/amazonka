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
-- Module      : Network.AWS.Pinpoint.GetCampaignDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that applies to a campaign.
module Network.AWS.Pinpoint.GetCampaignDateRangeKpi
  ( -- * Creating a Request
    getCampaignDateRangeKpi,
    GetCampaignDateRangeKpi,

    -- * Request Lenses
    gcdrkStartTime,
    gcdrkNextToken,
    gcdrkEndTime,
    gcdrkPageSize,
    gcdrkApplicationId,
    gcdrkKpiName,
    gcdrkCampaignId,

    -- * Destructuring the Response
    getCampaignDateRangeKpiResponse,
    GetCampaignDateRangeKpiResponse,

    -- * Response Lenses
    gcdrkrsResponseStatus,
    gcdrkrsCampaignDateRangeKpiResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaignDateRangeKpi' smart constructor.
data GetCampaignDateRangeKpi = GetCampaignDateRangeKpi'
  { _gcdrkStartTime ::
      !(Maybe POSIX),
    _gcdrkNextToken :: !(Maybe Text),
    _gcdrkEndTime :: !(Maybe POSIX),
    _gcdrkPageSize :: !(Maybe Text),
    _gcdrkApplicationId :: !Text,
    _gcdrkKpiName :: !Text,
    _gcdrkCampaignId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignDateRangeKpi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdrkStartTime' - The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- * 'gcdrkNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gcdrkEndTime' - The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- * 'gcdrkPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gcdrkApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gcdrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'gcdrkCampaignId' - The unique identifier for the campaign.
getCampaignDateRangeKpi ::
  -- | 'gcdrkApplicationId'
  Text ->
  -- | 'gcdrkKpiName'
  Text ->
  -- | 'gcdrkCampaignId'
  Text ->
  GetCampaignDateRangeKpi
getCampaignDateRangeKpi pApplicationId_ pKpiName_ pCampaignId_ =
  GetCampaignDateRangeKpi'
    { _gcdrkStartTime = Nothing,
      _gcdrkNextToken = Nothing,
      _gcdrkEndTime = Nothing,
      _gcdrkPageSize = Nothing,
      _gcdrkApplicationId = pApplicationId_,
      _gcdrkKpiName = pKpiName_,
      _gcdrkCampaignId = pCampaignId_
    }

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
gcdrkStartTime :: Lens' GetCampaignDateRangeKpi (Maybe UTCTime)
gcdrkStartTime = lens _gcdrkStartTime (\s a -> s {_gcdrkStartTime = a}) . mapping _Time

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gcdrkNextToken :: Lens' GetCampaignDateRangeKpi (Maybe Text)
gcdrkNextToken = lens _gcdrkNextToken (\s a -> s {_gcdrkNextToken = a})

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
gcdrkEndTime :: Lens' GetCampaignDateRangeKpi (Maybe UTCTime)
gcdrkEndTime = lens _gcdrkEndTime (\s a -> s {_gcdrkEndTime = a}) . mapping _Time

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gcdrkPageSize :: Lens' GetCampaignDateRangeKpi (Maybe Text)
gcdrkPageSize = lens _gcdrkPageSize (\s a -> s {_gcdrkPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gcdrkApplicationId :: Lens' GetCampaignDateRangeKpi Text
gcdrkApplicationId = lens _gcdrkApplicationId (\s a -> s {_gcdrkApplicationId = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
gcdrkKpiName :: Lens' GetCampaignDateRangeKpi Text
gcdrkKpiName = lens _gcdrkKpiName (\s a -> s {_gcdrkKpiName = a})

-- | The unique identifier for the campaign.
gcdrkCampaignId :: Lens' GetCampaignDateRangeKpi Text
gcdrkCampaignId = lens _gcdrkCampaignId (\s a -> s {_gcdrkCampaignId = a})

instance AWSRequest GetCampaignDateRangeKpi where
  type Rs GetCampaignDateRangeKpi = GetCampaignDateRangeKpiResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetCampaignDateRangeKpiResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetCampaignDateRangeKpi

instance NFData GetCampaignDateRangeKpi

instance ToHeaders GetCampaignDateRangeKpi where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetCampaignDateRangeKpi where
  toPath GetCampaignDateRangeKpi' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gcdrkApplicationId,
        "/campaigns/",
        toBS _gcdrkCampaignId,
        "/kpis/daterange/",
        toBS _gcdrkKpiName
      ]

instance ToQuery GetCampaignDateRangeKpi where
  toQuery GetCampaignDateRangeKpi' {..} =
    mconcat
      [ "start-time" =: _gcdrkStartTime,
        "next-token" =: _gcdrkNextToken,
        "end-time" =: _gcdrkEndTime,
        "page-size" =: _gcdrkPageSize
      ]

-- | /See:/ 'getCampaignDateRangeKpiResponse' smart constructor.
data GetCampaignDateRangeKpiResponse = GetCampaignDateRangeKpiResponse'
  { _gcdrkrsResponseStatus ::
      !Int,
    _gcdrkrsCampaignDateRangeKpiResponse ::
      !CampaignDateRangeKpiResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdrkrsResponseStatus' - -- | The response status code.
--
-- * 'gcdrkrsCampaignDateRangeKpiResponse' - Undocumented member.
getCampaignDateRangeKpiResponse ::
  -- | 'gcdrkrsResponseStatus'
  Int ->
  -- | 'gcdrkrsCampaignDateRangeKpiResponse'
  CampaignDateRangeKpiResponse ->
  GetCampaignDateRangeKpiResponse
getCampaignDateRangeKpiResponse
  pResponseStatus_
  pCampaignDateRangeKpiResponse_ =
    GetCampaignDateRangeKpiResponse'
      { _gcdrkrsResponseStatus =
          pResponseStatus_,
        _gcdrkrsCampaignDateRangeKpiResponse =
          pCampaignDateRangeKpiResponse_
      }

-- | -- | The response status code.
gcdrkrsResponseStatus :: Lens' GetCampaignDateRangeKpiResponse Int
gcdrkrsResponseStatus = lens _gcdrkrsResponseStatus (\s a -> s {_gcdrkrsResponseStatus = a})

-- | Undocumented member.
gcdrkrsCampaignDateRangeKpiResponse :: Lens' GetCampaignDateRangeKpiResponse CampaignDateRangeKpiResponse
gcdrkrsCampaignDateRangeKpiResponse = lens _gcdrkrsCampaignDateRangeKpiResponse (\s a -> s {_gcdrkrsCampaignDateRangeKpiResponse = a})

instance NFData GetCampaignDateRangeKpiResponse

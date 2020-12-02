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
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric that applies to a journey activity.
module Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics
  ( -- * Creating a Request
    getJourneyExecutionActivityMetrics,
    GetJourneyExecutionActivityMetrics,

    -- * Request Lenses
    gjeamNextToken,
    gjeamPageSize,
    gjeamJourneyActivityId,
    gjeamApplicationId,
    gjeamJourneyId,

    -- * Destructuring the Response
    getJourneyExecutionActivityMetricsResponse,
    GetJourneyExecutionActivityMetricsResponse,

    -- * Response Lenses
    gjeamrsResponseStatus,
    gjeamrsJourneyExecutionActivityMetricsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJourneyExecutionActivityMetrics' smart constructor.
data GetJourneyExecutionActivityMetrics = GetJourneyExecutionActivityMetrics'
  { _gjeamNextToken ::
      !(Maybe Text),
    _gjeamPageSize ::
      !(Maybe Text),
    _gjeamJourneyActivityId ::
      !Text,
    _gjeamApplicationId ::
      !Text,
    _gjeamJourneyId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJourneyExecutionActivityMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjeamNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjeamPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjeamJourneyActivityId' - The unique identifier for the journey activity.
--
-- * 'gjeamApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gjeamJourneyId' - The unique identifier for the journey.
getJourneyExecutionActivityMetrics ::
  -- | 'gjeamJourneyActivityId'
  Text ->
  -- | 'gjeamApplicationId'
  Text ->
  -- | 'gjeamJourneyId'
  Text ->
  GetJourneyExecutionActivityMetrics
getJourneyExecutionActivityMetrics
  pJourneyActivityId_
  pApplicationId_
  pJourneyId_ =
    GetJourneyExecutionActivityMetrics'
      { _gjeamNextToken = Nothing,
        _gjeamPageSize = Nothing,
        _gjeamJourneyActivityId = pJourneyActivityId_,
        _gjeamApplicationId = pApplicationId_,
        _gjeamJourneyId = pJourneyId_
      }

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjeamNextToken :: Lens' GetJourneyExecutionActivityMetrics (Maybe Text)
gjeamNextToken = lens _gjeamNextToken (\s a -> s {_gjeamNextToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjeamPageSize :: Lens' GetJourneyExecutionActivityMetrics (Maybe Text)
gjeamPageSize = lens _gjeamPageSize (\s a -> s {_gjeamPageSize = a})

-- | The unique identifier for the journey activity.
gjeamJourneyActivityId :: Lens' GetJourneyExecutionActivityMetrics Text
gjeamJourneyActivityId = lens _gjeamJourneyActivityId (\s a -> s {_gjeamJourneyActivityId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gjeamApplicationId :: Lens' GetJourneyExecutionActivityMetrics Text
gjeamApplicationId = lens _gjeamApplicationId (\s a -> s {_gjeamApplicationId = a})

-- | The unique identifier for the journey.
gjeamJourneyId :: Lens' GetJourneyExecutionActivityMetrics Text
gjeamJourneyId = lens _gjeamJourneyId (\s a -> s {_gjeamJourneyId = a})

instance AWSRequest GetJourneyExecutionActivityMetrics where
  type
    Rs GetJourneyExecutionActivityMetrics =
      GetJourneyExecutionActivityMetricsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetJourneyExecutionActivityMetricsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetJourneyExecutionActivityMetrics

instance NFData GetJourneyExecutionActivityMetrics

instance ToHeaders GetJourneyExecutionActivityMetrics where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetJourneyExecutionActivityMetrics where
  toPath GetJourneyExecutionActivityMetrics' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gjeamApplicationId,
        "/journeys/",
        toBS _gjeamJourneyId,
        "/activities/",
        toBS _gjeamJourneyActivityId,
        "/execution-metrics"
      ]

instance ToQuery GetJourneyExecutionActivityMetrics where
  toQuery GetJourneyExecutionActivityMetrics' {..} =
    mconcat
      ["next-token" =: _gjeamNextToken, "page-size" =: _gjeamPageSize]

-- | /See:/ 'getJourneyExecutionActivityMetricsResponse' smart constructor.
data GetJourneyExecutionActivityMetricsResponse = GetJourneyExecutionActivityMetricsResponse'
  { _gjeamrsResponseStatus ::
      !Int,
    _gjeamrsJourneyExecutionActivityMetricsResponse ::
      !JourneyExecutionActivityMetricsResponse
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetJourneyExecutionActivityMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjeamrsResponseStatus' - -- | The response status code.
--
-- * 'gjeamrsJourneyExecutionActivityMetricsResponse' - Undocumented member.
getJourneyExecutionActivityMetricsResponse ::
  -- | 'gjeamrsResponseStatus'
  Int ->
  -- | 'gjeamrsJourneyExecutionActivityMetricsResponse'
  JourneyExecutionActivityMetricsResponse ->
  GetJourneyExecutionActivityMetricsResponse
getJourneyExecutionActivityMetricsResponse
  pResponseStatus_
  pJourneyExecutionActivityMetricsResponse_ =
    GetJourneyExecutionActivityMetricsResponse'
      { _gjeamrsResponseStatus =
          pResponseStatus_,
        _gjeamrsJourneyExecutionActivityMetricsResponse =
          pJourneyExecutionActivityMetricsResponse_
      }

-- | -- | The response status code.
gjeamrsResponseStatus :: Lens' GetJourneyExecutionActivityMetricsResponse Int
gjeamrsResponseStatus = lens _gjeamrsResponseStatus (\s a -> s {_gjeamrsResponseStatus = a})

-- | Undocumented member.
gjeamrsJourneyExecutionActivityMetricsResponse :: Lens' GetJourneyExecutionActivityMetricsResponse JourneyExecutionActivityMetricsResponse
gjeamrsJourneyExecutionActivityMetricsResponse = lens _gjeamrsJourneyExecutionActivityMetricsResponse (\s a -> s {_gjeamrsJourneyExecutionActivityMetricsResponse = a})

instance NFData GetJourneyExecutionActivityMetricsResponse

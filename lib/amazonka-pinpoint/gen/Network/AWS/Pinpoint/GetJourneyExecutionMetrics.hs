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
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyExecutionMetrics
  ( -- * Creating a Request
    getJourneyExecutionMetrics,
    GetJourneyExecutionMetrics,

    -- * Request Lenses
    gjemNextToken,
    gjemPageSize,
    gjemApplicationId,
    gjemJourneyId,

    -- * Destructuring the Response
    getJourneyExecutionMetricsResponse,
    GetJourneyExecutionMetricsResponse,

    -- * Response Lenses
    gjemrsResponseStatus,
    gjemrsJourneyExecutionMetricsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJourneyExecutionMetrics' smart constructor.
data GetJourneyExecutionMetrics = GetJourneyExecutionMetrics'
  { _gjemNextToken ::
      !(Maybe Text),
    _gjemPageSize :: !(Maybe Text),
    _gjemApplicationId :: !Text,
    _gjemJourneyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJourneyExecutionMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjemNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjemPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gjemApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gjemJourneyId' - The unique identifier for the journey.
getJourneyExecutionMetrics ::
  -- | 'gjemApplicationId'
  Text ->
  -- | 'gjemJourneyId'
  Text ->
  GetJourneyExecutionMetrics
getJourneyExecutionMetrics pApplicationId_ pJourneyId_ =
  GetJourneyExecutionMetrics'
    { _gjemNextToken = Nothing,
      _gjemPageSize = Nothing,
      _gjemApplicationId = pApplicationId_,
      _gjemJourneyId = pJourneyId_
    }

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjemNextToken :: Lens' GetJourneyExecutionMetrics (Maybe Text)
gjemNextToken = lens _gjemNextToken (\s a -> s {_gjemNextToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gjemPageSize :: Lens' GetJourneyExecutionMetrics (Maybe Text)
gjemPageSize = lens _gjemPageSize (\s a -> s {_gjemPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gjemApplicationId :: Lens' GetJourneyExecutionMetrics Text
gjemApplicationId = lens _gjemApplicationId (\s a -> s {_gjemApplicationId = a})

-- | The unique identifier for the journey.
gjemJourneyId :: Lens' GetJourneyExecutionMetrics Text
gjemJourneyId = lens _gjemJourneyId (\s a -> s {_gjemJourneyId = a})

instance AWSRequest GetJourneyExecutionMetrics where
  type
    Rs GetJourneyExecutionMetrics =
      GetJourneyExecutionMetricsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetJourneyExecutionMetricsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetJourneyExecutionMetrics

instance NFData GetJourneyExecutionMetrics

instance ToHeaders GetJourneyExecutionMetrics where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetJourneyExecutionMetrics where
  toPath GetJourneyExecutionMetrics' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gjemApplicationId,
        "/journeys/",
        toBS _gjemJourneyId,
        "/execution-metrics"
      ]

instance ToQuery GetJourneyExecutionMetrics where
  toQuery GetJourneyExecutionMetrics' {..} =
    mconcat
      ["next-token" =: _gjemNextToken, "page-size" =: _gjemPageSize]

-- | /See:/ 'getJourneyExecutionMetricsResponse' smart constructor.
data GetJourneyExecutionMetricsResponse = GetJourneyExecutionMetricsResponse'
  { _gjemrsResponseStatus ::
      !Int,
    _gjemrsJourneyExecutionMetricsResponse ::
      !JourneyExecutionMetricsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJourneyExecutionMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjemrsResponseStatus' - -- | The response status code.
--
-- * 'gjemrsJourneyExecutionMetricsResponse' - Undocumented member.
getJourneyExecutionMetricsResponse ::
  -- | 'gjemrsResponseStatus'
  Int ->
  -- | 'gjemrsJourneyExecutionMetricsResponse'
  JourneyExecutionMetricsResponse ->
  GetJourneyExecutionMetricsResponse
getJourneyExecutionMetricsResponse
  pResponseStatus_
  pJourneyExecutionMetricsResponse_ =
    GetJourneyExecutionMetricsResponse'
      { _gjemrsResponseStatus =
          pResponseStatus_,
        _gjemrsJourneyExecutionMetricsResponse =
          pJourneyExecutionMetricsResponse_
      }

-- | -- | The response status code.
gjemrsResponseStatus :: Lens' GetJourneyExecutionMetricsResponse Int
gjemrsResponseStatus = lens _gjemrsResponseStatus (\s a -> s {_gjemrsResponseStatus = a})

-- | Undocumented member.
gjemrsJourneyExecutionMetricsResponse :: Lens' GetJourneyExecutionMetricsResponse JourneyExecutionMetricsResponse
gjemrsJourneyExecutionMetricsResponse = lens _gjemrsJourneyExecutionMetricsResponse (\s a -> s {_gjemrsJourneyExecutionMetricsResponse = a})

instance NFData GetJourneyExecutionMetricsResponse

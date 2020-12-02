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
-- Module      : Network.AWS.XRay.GetInsightImpactGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph structure filtered by the specified insight. The service graph is limited to only structural information. For a complete service graph, use this API with the GetServiceGraph API.
module Network.AWS.XRay.GetInsightImpactGraph
  ( -- * Creating a Request
    getInsightImpactGraph,
    GetInsightImpactGraph,

    -- * Request Lenses
    giigNextToken,
    giigInsightId,
    giigStartTime,
    giigEndTime,

    -- * Destructuring the Response
    getInsightImpactGraphResponse,
    GetInsightImpactGraphResponse,

    -- * Response Lenses
    giigrsServiceGraphStartTime,
    giigrsStartTime,
    giigrsInsightId,
    giigrsNextToken,
    giigrsEndTime,
    giigrsServiceGraphEndTime,
    giigrsServices,
    giigrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'getInsightImpactGraph' smart constructor.
data GetInsightImpactGraph = GetInsightImpactGraph'
  { _giigNextToken ::
      !(Maybe Text),
    _giigInsightId :: !Text,
    _giigStartTime :: !POSIX,
    _giigEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightImpactGraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giigNextToken' - Specify the pagination token returned by a previous request to retrieve the next page of results.
--
-- * 'giigInsightId' - The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- * 'giigStartTime' - The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
--
-- * 'giigEndTime' - The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours.
getInsightImpactGraph ::
  -- | 'giigInsightId'
  Text ->
  -- | 'giigStartTime'
  UTCTime ->
  -- | 'giigEndTime'
  UTCTime ->
  GetInsightImpactGraph
getInsightImpactGraph pInsightId_ pStartTime_ pEndTime_ =
  GetInsightImpactGraph'
    { _giigNextToken = Nothing,
      _giigInsightId = pInsightId_,
      _giigStartTime = _Time # pStartTime_,
      _giigEndTime = _Time # pEndTime_
    }

-- | Specify the pagination token returned by a previous request to retrieve the next page of results.
giigNextToken :: Lens' GetInsightImpactGraph (Maybe Text)
giigNextToken = lens _giigNextToken (\s a -> s {_giigNextToken = a})

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
giigInsightId :: Lens' GetInsightImpactGraph Text
giigInsightId = lens _giigInsightId (\s a -> s {_giigInsightId = a})

-- | The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
giigStartTime :: Lens' GetInsightImpactGraph UTCTime
giigStartTime = lens _giigStartTime (\s a -> s {_giigStartTime = a}) . _Time

-- | The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours.
giigEndTime :: Lens' GetInsightImpactGraph UTCTime
giigEndTime = lens _giigEndTime (\s a -> s {_giigEndTime = a}) . _Time

instance AWSRequest GetInsightImpactGraph where
  type Rs GetInsightImpactGraph = GetInsightImpactGraphResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          GetInsightImpactGraphResponse'
            <$> (x .?> "ServiceGraphStartTime")
            <*> (x .?> "StartTime")
            <*> (x .?> "InsightId")
            <*> (x .?> "NextToken")
            <*> (x .?> "EndTime")
            <*> (x .?> "ServiceGraphEndTime")
            <*> (x .?> "Services" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetInsightImpactGraph

instance NFData GetInsightImpactGraph

instance ToHeaders GetInsightImpactGraph where
  toHeaders = const mempty

instance ToJSON GetInsightImpactGraph where
  toJSON GetInsightImpactGraph' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _giigNextToken,
            Just ("InsightId" .= _giigInsightId),
            Just ("StartTime" .= _giigStartTime),
            Just ("EndTime" .= _giigEndTime)
          ]
      )

instance ToPath GetInsightImpactGraph where
  toPath = const "/InsightImpactGraph"

instance ToQuery GetInsightImpactGraph where
  toQuery = const mempty

-- | /See:/ 'getInsightImpactGraphResponse' smart constructor.
data GetInsightImpactGraphResponse = GetInsightImpactGraphResponse'
  { _giigrsServiceGraphStartTime ::
      !(Maybe POSIX),
    _giigrsStartTime ::
      !(Maybe POSIX),
    _giigrsInsightId ::
      !(Maybe Text),
    _giigrsNextToken ::
      !(Maybe Text),
    _giigrsEndTime ::
      !(Maybe POSIX),
    _giigrsServiceGraphEndTime ::
      !(Maybe POSIX),
    _giigrsServices ::
      !( Maybe
           [InsightImpactGraphService]
       ),
    _giigrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightImpactGraphResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giigrsServiceGraphStartTime' - The time, in Unix seconds, at which the service graph started.
--
-- * 'giigrsStartTime' - The provided start time.
--
-- * 'giigrsInsightId' - The insight's unique identifier.
--
-- * 'giigrsNextToken' - Pagination token.
--
-- * 'giigrsEndTime' - The provided end time.
--
-- * 'giigrsServiceGraphEndTime' - The time, in Unix seconds, at which the service graph ended.
--
-- * 'giigrsServices' - The AWS instrumented services related to the insight.
--
-- * 'giigrsResponseStatus' - -- | The response status code.
getInsightImpactGraphResponse ::
  -- | 'giigrsResponseStatus'
  Int ->
  GetInsightImpactGraphResponse
getInsightImpactGraphResponse pResponseStatus_ =
  GetInsightImpactGraphResponse'
    { _giigrsServiceGraphStartTime =
        Nothing,
      _giigrsStartTime = Nothing,
      _giigrsInsightId = Nothing,
      _giigrsNextToken = Nothing,
      _giigrsEndTime = Nothing,
      _giigrsServiceGraphEndTime = Nothing,
      _giigrsServices = Nothing,
      _giigrsResponseStatus = pResponseStatus_
    }

-- | The time, in Unix seconds, at which the service graph started.
giigrsServiceGraphStartTime :: Lens' GetInsightImpactGraphResponse (Maybe UTCTime)
giigrsServiceGraphStartTime = lens _giigrsServiceGraphStartTime (\s a -> s {_giigrsServiceGraphStartTime = a}) . mapping _Time

-- | The provided start time.
giigrsStartTime :: Lens' GetInsightImpactGraphResponse (Maybe UTCTime)
giigrsStartTime = lens _giigrsStartTime (\s a -> s {_giigrsStartTime = a}) . mapping _Time

-- | The insight's unique identifier.
giigrsInsightId :: Lens' GetInsightImpactGraphResponse (Maybe Text)
giigrsInsightId = lens _giigrsInsightId (\s a -> s {_giigrsInsightId = a})

-- | Pagination token.
giigrsNextToken :: Lens' GetInsightImpactGraphResponse (Maybe Text)
giigrsNextToken = lens _giigrsNextToken (\s a -> s {_giigrsNextToken = a})

-- | The provided end time.
giigrsEndTime :: Lens' GetInsightImpactGraphResponse (Maybe UTCTime)
giigrsEndTime = lens _giigrsEndTime (\s a -> s {_giigrsEndTime = a}) . mapping _Time

-- | The time, in Unix seconds, at which the service graph ended.
giigrsServiceGraphEndTime :: Lens' GetInsightImpactGraphResponse (Maybe UTCTime)
giigrsServiceGraphEndTime = lens _giigrsServiceGraphEndTime (\s a -> s {_giigrsServiceGraphEndTime = a}) . mapping _Time

-- | The AWS instrumented services related to the insight.
giigrsServices :: Lens' GetInsightImpactGraphResponse [InsightImpactGraphService]
giigrsServices = lens _giigrsServices (\s a -> s {_giigrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
giigrsResponseStatus :: Lens' GetInsightImpactGraphResponse Int
giigrsResponseStatus = lens _giigrsResponseStatus (\s a -> s {_giigrsResponseStatus = a})

instance NFData GetInsightImpactGraphResponse

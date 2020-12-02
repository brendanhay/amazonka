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
-- Module      : Network.AWS.XRay.GetTimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an aggregation of service statistics defined by a specific time range.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTimeSeriesServiceStatistics
  ( -- * Creating a Request
    getTimeSeriesServiceStatistics,
    GetTimeSeriesServiceStatistics,

    -- * Request Lenses
    gtsssEntitySelectorExpression,
    gtsssPeriod,
    gtsssForecastStatistics,
    gtsssNextToken,
    gtsssGroupARN,
    gtsssGroupName,
    gtsssStartTime,
    gtsssEndTime,

    -- * Destructuring the Response
    getTimeSeriesServiceStatisticsResponse,
    GetTimeSeriesServiceStatisticsResponse,

    -- * Response Lenses
    gtsssrsContainsOldGroupVersions,
    gtsssrsTimeSeriesServiceStatistics,
    gtsssrsNextToken,
    gtsssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'getTimeSeriesServiceStatistics' smart constructor.
data GetTimeSeriesServiceStatistics = GetTimeSeriesServiceStatistics'
  { _gtsssEntitySelectorExpression ::
      !(Maybe Text),
    _gtsssPeriod :: !(Maybe Int),
    _gtsssForecastStatistics ::
      !(Maybe Bool),
    _gtsssNextToken ::
      !(Maybe Text),
    _gtsssGroupARN ::
      !(Maybe Text),
    _gtsssGroupName ::
      !(Maybe Text),
    _gtsssStartTime :: !POSIX,
    _gtsssEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTimeSeriesServiceStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsssEntitySelectorExpression' - A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
--
-- * 'gtsssPeriod' - Aggregation period in seconds.
--
-- * 'gtsssForecastStatistics' - The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
--
-- * 'gtsssNextToken' - Pagination token.
--
-- * 'gtsssGroupARN' - The Amazon Resource Name (ARN) of the group for which to pull statistics from.
--
-- * 'gtsssGroupName' - The case-sensitive name of the group for which to pull statistics from.
--
-- * 'gtsssStartTime' - The start of the time frame for which to aggregate statistics.
--
-- * 'gtsssEndTime' - The end of the time frame for which to aggregate statistics.
getTimeSeriesServiceStatistics ::
  -- | 'gtsssStartTime'
  UTCTime ->
  -- | 'gtsssEndTime'
  UTCTime ->
  GetTimeSeriesServiceStatistics
getTimeSeriesServiceStatistics pStartTime_ pEndTime_ =
  GetTimeSeriesServiceStatistics'
    { _gtsssEntitySelectorExpression =
        Nothing,
      _gtsssPeriod = Nothing,
      _gtsssForecastStatistics = Nothing,
      _gtsssNextToken = Nothing,
      _gtsssGroupARN = Nothing,
      _gtsssGroupName = Nothing,
      _gtsssStartTime = _Time # pStartTime_,
      _gtsssEndTime = _Time # pEndTime_
    }

-- | A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
gtsssEntitySelectorExpression :: Lens' GetTimeSeriesServiceStatistics (Maybe Text)
gtsssEntitySelectorExpression = lens _gtsssEntitySelectorExpression (\s a -> s {_gtsssEntitySelectorExpression = a})

-- | Aggregation period in seconds.
gtsssPeriod :: Lens' GetTimeSeriesServiceStatistics (Maybe Int)
gtsssPeriod = lens _gtsssPeriod (\s a -> s {_gtsssPeriod = a})

-- | The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
gtsssForecastStatistics :: Lens' GetTimeSeriesServiceStatistics (Maybe Bool)
gtsssForecastStatistics = lens _gtsssForecastStatistics (\s a -> s {_gtsssForecastStatistics = a})

-- | Pagination token.
gtsssNextToken :: Lens' GetTimeSeriesServiceStatistics (Maybe Text)
gtsssNextToken = lens _gtsssNextToken (\s a -> s {_gtsssNextToken = a})

-- | The Amazon Resource Name (ARN) of the group for which to pull statistics from.
gtsssGroupARN :: Lens' GetTimeSeriesServiceStatistics (Maybe Text)
gtsssGroupARN = lens _gtsssGroupARN (\s a -> s {_gtsssGroupARN = a})

-- | The case-sensitive name of the group for which to pull statistics from.
gtsssGroupName :: Lens' GetTimeSeriesServiceStatistics (Maybe Text)
gtsssGroupName = lens _gtsssGroupName (\s a -> s {_gtsssGroupName = a})

-- | The start of the time frame for which to aggregate statistics.
gtsssStartTime :: Lens' GetTimeSeriesServiceStatistics UTCTime
gtsssStartTime = lens _gtsssStartTime (\s a -> s {_gtsssStartTime = a}) . _Time

-- | The end of the time frame for which to aggregate statistics.
gtsssEndTime :: Lens' GetTimeSeriesServiceStatistics UTCTime
gtsssEndTime = lens _gtsssEndTime (\s a -> s {_gtsssEndTime = a}) . _Time

instance AWSPager GetTimeSeriesServiceStatistics where
  page rq rs
    | stop (rs ^. gtsssrsNextToken) = Nothing
    | stop (rs ^. gtsssrsTimeSeriesServiceStatistics) = Nothing
    | otherwise = Just $ rq & gtsssNextToken .~ rs ^. gtsssrsNextToken

instance AWSRequest GetTimeSeriesServiceStatistics where
  type
    Rs GetTimeSeriesServiceStatistics =
      GetTimeSeriesServiceStatisticsResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          GetTimeSeriesServiceStatisticsResponse'
            <$> (x .?> "ContainsOldGroupVersions")
            <*> (x .?> "TimeSeriesServiceStatistics" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetTimeSeriesServiceStatistics

instance NFData GetTimeSeriesServiceStatistics

instance ToHeaders GetTimeSeriesServiceStatistics where
  toHeaders = const mempty

instance ToJSON GetTimeSeriesServiceStatistics where
  toJSON GetTimeSeriesServiceStatistics' {..} =
    object
      ( catMaybes
          [ ("EntitySelectorExpression" .=)
              <$> _gtsssEntitySelectorExpression,
            ("Period" .=) <$> _gtsssPeriod,
            ("ForecastStatistics" .=) <$> _gtsssForecastStatistics,
            ("NextToken" .=) <$> _gtsssNextToken,
            ("GroupARN" .=) <$> _gtsssGroupARN,
            ("GroupName" .=) <$> _gtsssGroupName,
            Just ("StartTime" .= _gtsssStartTime),
            Just ("EndTime" .= _gtsssEndTime)
          ]
      )

instance ToPath GetTimeSeriesServiceStatistics where
  toPath = const "/TimeSeriesServiceStatistics"

instance ToQuery GetTimeSeriesServiceStatistics where
  toQuery = const mempty

-- | /See:/ 'getTimeSeriesServiceStatisticsResponse' smart constructor.
data GetTimeSeriesServiceStatisticsResponse = GetTimeSeriesServiceStatisticsResponse'
  { _gtsssrsContainsOldGroupVersions ::
      !(Maybe Bool),
    _gtsssrsTimeSeriesServiceStatistics ::
      !( Maybe
           [TimeSeriesServiceStatistics]
       ),
    _gtsssrsNextToken ::
      !(Maybe Text),
    _gtsssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTimeSeriesServiceStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsssrsContainsOldGroupVersions' - A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
--
-- * 'gtsssrsTimeSeriesServiceStatistics' - The collection of statistics.
--
-- * 'gtsssrsNextToken' - Pagination token.
--
-- * 'gtsssrsResponseStatus' - -- | The response status code.
getTimeSeriesServiceStatisticsResponse ::
  -- | 'gtsssrsResponseStatus'
  Int ->
  GetTimeSeriesServiceStatisticsResponse
getTimeSeriesServiceStatisticsResponse pResponseStatus_ =
  GetTimeSeriesServiceStatisticsResponse'
    { _gtsssrsContainsOldGroupVersions =
        Nothing,
      _gtsssrsTimeSeriesServiceStatistics = Nothing,
      _gtsssrsNextToken = Nothing,
      _gtsssrsResponseStatus = pResponseStatus_
    }

-- | A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
gtsssrsContainsOldGroupVersions :: Lens' GetTimeSeriesServiceStatisticsResponse (Maybe Bool)
gtsssrsContainsOldGroupVersions = lens _gtsssrsContainsOldGroupVersions (\s a -> s {_gtsssrsContainsOldGroupVersions = a})

-- | The collection of statistics.
gtsssrsTimeSeriesServiceStatistics :: Lens' GetTimeSeriesServiceStatisticsResponse [TimeSeriesServiceStatistics]
gtsssrsTimeSeriesServiceStatistics = lens _gtsssrsTimeSeriesServiceStatistics (\s a -> s {_gtsssrsTimeSeriesServiceStatistics = a}) . _Default . _Coerce

-- | Pagination token.
gtsssrsNextToken :: Lens' GetTimeSeriesServiceStatisticsResponse (Maybe Text)
gtsssrsNextToken = lens _gtsssrsNextToken (\s a -> s {_gtsssrsNextToken = a})

-- | -- | The response status code.
gtsssrsResponseStatus :: Lens' GetTimeSeriesServiceStatisticsResponse Int
gtsssrsResponseStatus = lens _gtsssrsResponseStatus (\s a -> s {_gtsssrsResponseStatus = a})

instance NFData GetTimeSeriesServiceStatisticsResponse

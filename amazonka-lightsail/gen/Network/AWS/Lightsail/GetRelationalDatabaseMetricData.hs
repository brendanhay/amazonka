{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points of the specified metric for a database in Amazon Lightsail.
--
--
module Network.AWS.Lightsail.GetRelationalDatabaseMetricData
    (
    -- * Creating a Request
      getRelationalDatabaseMetricData
    , GetRelationalDatabaseMetricData
    -- * Request Lenses
    , grdmdRelationalDatabaseName
    , grdmdMetricName
    , grdmdPeriod
    , grdmdStartTime
    , grdmdEndTime
    , grdmdUnit
    , grdmdStatistics

    -- * Destructuring the Response
    , getRelationalDatabaseMetricDataResponse
    , GetRelationalDatabaseMetricDataResponse
    -- * Response Lenses
    , grdmdrsMetricName
    , grdmdrsMetricData
    , grdmdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseMetricData' smart constructor.
data GetRelationalDatabaseMetricData = GetRelationalDatabaseMetricData'
  { _grdmdRelationalDatabaseName :: !Text
  , _grdmdMetricName             :: !RelationalDatabaseMetricName
  , _grdmdPeriod                 :: !Nat
  , _grdmdStartTime              :: !POSIX
  , _grdmdEndTime                :: !POSIX
  , _grdmdUnit                   :: !MetricUnit
  , _grdmdStatistics             :: ![MetricStatistic]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdmdRelationalDatabaseName' - The name of your database from which to get metric data.
--
-- * 'grdmdMetricName' - The name of the metric data to return.
--
-- * 'grdmdPeriod' - The granularity, in seconds, of the returned data points.
--
-- * 'grdmdStartTime' - The start of the time interval from which to get metric data. Constraints:     * Specified in Universal Coordinated Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
--
-- * 'grdmdEndTime' - The end of the time interval from which to get metric data. Constraints:     * Specified in Universal Coordinated Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
--
-- * 'grdmdUnit' - The unit for the metric data request.
--
-- * 'grdmdStatistics' - The array of statistics for your metric data request.
getRelationalDatabaseMetricData
    :: Text -- ^ 'grdmdRelationalDatabaseName'
    -> RelationalDatabaseMetricName -- ^ 'grdmdMetricName'
    -> Natural -- ^ 'grdmdPeriod'
    -> UTCTime -- ^ 'grdmdStartTime'
    -> UTCTime -- ^ 'grdmdEndTime'
    -> MetricUnit -- ^ 'grdmdUnit'
    -> GetRelationalDatabaseMetricData
getRelationalDatabaseMetricData pRelationalDatabaseName_ pMetricName_ pPeriod_ pStartTime_ pEndTime_ pUnit_ =
  GetRelationalDatabaseMetricData'
    { _grdmdRelationalDatabaseName = pRelationalDatabaseName_
    , _grdmdMetricName = pMetricName_
    , _grdmdPeriod = _Nat # pPeriod_
    , _grdmdStartTime = _Time # pStartTime_
    , _grdmdEndTime = _Time # pEndTime_
    , _grdmdUnit = pUnit_
    , _grdmdStatistics = mempty
    }


-- | The name of your database from which to get metric data.
grdmdRelationalDatabaseName :: Lens' GetRelationalDatabaseMetricData Text
grdmdRelationalDatabaseName = lens _grdmdRelationalDatabaseName (\ s a -> s{_grdmdRelationalDatabaseName = a})

-- | The name of the metric data to return.
grdmdMetricName :: Lens' GetRelationalDatabaseMetricData RelationalDatabaseMetricName
grdmdMetricName = lens _grdmdMetricName (\ s a -> s{_grdmdMetricName = a})

-- | The granularity, in seconds, of the returned data points.
grdmdPeriod :: Lens' GetRelationalDatabaseMetricData Natural
grdmdPeriod = lens _grdmdPeriod (\ s a -> s{_grdmdPeriod = a}) . _Nat

-- | The start of the time interval from which to get metric data. Constraints:     * Specified in Universal Coordinated Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
grdmdStartTime :: Lens' GetRelationalDatabaseMetricData UTCTime
grdmdStartTime = lens _grdmdStartTime (\ s a -> s{_grdmdStartTime = a}) . _Time

-- | The end of the time interval from which to get metric data. Constraints:     * Specified in Universal Coordinated Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
grdmdEndTime :: Lens' GetRelationalDatabaseMetricData UTCTime
grdmdEndTime = lens _grdmdEndTime (\ s a -> s{_grdmdEndTime = a}) . _Time

-- | The unit for the metric data request.
grdmdUnit :: Lens' GetRelationalDatabaseMetricData MetricUnit
grdmdUnit = lens _grdmdUnit (\ s a -> s{_grdmdUnit = a})

-- | The array of statistics for your metric data request.
grdmdStatistics :: Lens' GetRelationalDatabaseMetricData [MetricStatistic]
grdmdStatistics = lens _grdmdStatistics (\ s a -> s{_grdmdStatistics = a}) . _Coerce

instance AWSRequest GetRelationalDatabaseMetricData
         where
        type Rs GetRelationalDatabaseMetricData =
             GetRelationalDatabaseMetricDataResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseMetricDataResponse' <$>
                   (x .?> "metricName") <*>
                     (x .?> "metricData" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseMetricData
         where

instance NFData GetRelationalDatabaseMetricData where

instance ToHeaders GetRelationalDatabaseMetricData
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseMetricData"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseMetricData where
        toJSON GetRelationalDatabaseMetricData'{..}
          = object
              (catMaybes
                 [Just
                    ("relationalDatabaseName" .=
                       _grdmdRelationalDatabaseName),
                  Just ("metricName" .= _grdmdMetricName),
                  Just ("period" .= _grdmdPeriod),
                  Just ("startTime" .= _grdmdStartTime),
                  Just ("endTime" .= _grdmdEndTime),
                  Just ("unit" .= _grdmdUnit),
                  Just ("statistics" .= _grdmdStatistics)])

instance ToPath GetRelationalDatabaseMetricData where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseMetricData
         where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseMetricDataResponse' smart constructor.
data GetRelationalDatabaseMetricDataResponse = GetRelationalDatabaseMetricDataResponse'
  { _grdmdrsMetricName     :: !(Maybe RelationalDatabaseMetricName)
  , _grdmdrsMetricData     :: !(Maybe [MetricDatapoint])
  , _grdmdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdmdrsMetricName' - The name of the metric.
--
-- * 'grdmdrsMetricData' - An object describing the result of your get relational database metric data request.
--
-- * 'grdmdrsResponseStatus' - -- | The response status code.
getRelationalDatabaseMetricDataResponse
    :: Int -- ^ 'grdmdrsResponseStatus'
    -> GetRelationalDatabaseMetricDataResponse
getRelationalDatabaseMetricDataResponse pResponseStatus_ =
  GetRelationalDatabaseMetricDataResponse'
    { _grdmdrsMetricName = Nothing
    , _grdmdrsMetricData = Nothing
    , _grdmdrsResponseStatus = pResponseStatus_
    }


-- | The name of the metric.
grdmdrsMetricName :: Lens' GetRelationalDatabaseMetricDataResponse (Maybe RelationalDatabaseMetricName)
grdmdrsMetricName = lens _grdmdrsMetricName (\ s a -> s{_grdmdrsMetricName = a})

-- | An object describing the result of your get relational database metric data request.
grdmdrsMetricData :: Lens' GetRelationalDatabaseMetricDataResponse [MetricDatapoint]
grdmdrsMetricData = lens _grdmdrsMetricData (\ s a -> s{_grdmdrsMetricData = a}) . _Default . _Coerce

-- | -- | The response status code.
grdmdrsResponseStatus :: Lens' GetRelationalDatabaseMetricDataResponse Int
grdmdrsResponseStatus = lens _grdmdrsResponseStatus (\ s a -> s{_grdmdrsResponseStatus = a})

instance NFData
           GetRelationalDatabaseMetricDataResponse
         where

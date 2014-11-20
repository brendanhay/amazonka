{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets statistics for the specified metric. The maximum number of data points
-- returned from a single GetMetricStatistics request is 1,440, wereas the
-- maximum number of data points that can be queried is 50,850. If you make a
-- request that generates more than 1,440 data points, Amazon CloudWatch
-- returns an error. In such a case, you can alter the request by narrowing
-- the specified time range or increasing the specified period. Alternatively,
-- you can make multiple requests across adjacent time ranges. Amazon
-- CloudWatch aggregates data points based on the length of the period that
-- you specify. For example, if you request statistics with a one-minute
-- granularity, Amazon CloudWatch aggregates data points with time stamps that
-- fall within the same one-minute period. In such a case, the data points
-- queried can greatly outnumber the data points returned. The following
-- examples show various statistics allowed by the data point query maximum of
-- 50,850 when you call GetMetricStatistics on Amazon EC2 instances with
-- detailed (one-minute) monitoring enabled: Statistics for up to 400
-- instances for a span of one hour Statistics for up to 35 instances over a
-- span of 24 hours Statistics for up to 2 instances over a span of 2 weeks
-- For information about the namespace, metric names, and dimensions that
-- other Amazon Web Services products use to send metrics to Cloudwatch, go to
-- Amazon CloudWatch Metrics, Namespaces, and Dimensions Reference in the
-- Amazon CloudWatch Developer Guide.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html>
module Network.AWS.CloudWatch.GetMetricStatistics
    (
    -- * Request
      GetMetricStatistics
    -- ** Request constructor
    , getMetricStatistics
    -- ** Request lenses
    , gmsDimensions
    , gmsEndTime
    , gmsMetricName
    , gmsNamespace
    , gmsPeriod
    , gmsStartTime
    , gmsStatistics
    , gmsUnit

    -- * Response
    , GetMetricStatisticsResponse
    -- ** Response constructor
    , getMetricStatisticsResponse
    -- ** Response lenses
    , gmsrDatapoints
    , gmsrLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data GetMetricStatistics = GetMetricStatistics
    { _gmsDimensions :: List "Dimensions" Dimension
    , _gmsEndTime    :: RFC822
    , _gmsMetricName :: Text
    , _gmsNamespace  :: Text
    , _gmsPeriod     :: Nat
    , _gmsStartTime  :: RFC822
    , _gmsStatistics :: List1 "Statistics" Text
    , _gmsUnit       :: Maybe Text
    } deriving (Eq, Show)

-- | 'GetMetricStatistics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsDimensions' @::@ ['Dimension']
--
-- * 'gmsEndTime' @::@ 'UTCTime'
--
-- * 'gmsMetricName' @::@ 'Text'
--
-- * 'gmsNamespace' @::@ 'Text'
--
-- * 'gmsPeriod' @::@ 'Natural'
--
-- * 'gmsStartTime' @::@ 'UTCTime'
--
-- * 'gmsStatistics' @::@ 'NonEmpty' 'Text'
--
-- * 'gmsUnit' @::@ 'Maybe' 'Text'
--
getMetricStatistics :: Text -- ^ 'gmsNamespace'
                    -> Text -- ^ 'gmsMetricName'
                    -> UTCTime -- ^ 'gmsStartTime'
                    -> UTCTime -- ^ 'gmsEndTime'
                    -> Natural -- ^ 'gmsPeriod'
                    -> NonEmpty Text -- ^ 'gmsStatistics'
                    -> GetMetricStatistics
getMetricStatistics p1 p2 p3 p4 p5 p6 = GetMetricStatistics
    { _gmsNamespace  = p1
    , _gmsMetricName = p2
    , _gmsStartTime  = withIso _Time (const id) p3
    , _gmsEndTime    = withIso _Time (const id) p4
    , _gmsPeriod     = withIso _Nat (const id) p5
    , _gmsStatistics = withIso _List1 (const id) p6
    , _gmsDimensions = mempty
    , _gmsUnit       = Nothing
    }

-- | A list of dimensions describing qualities of the metric.
gmsDimensions :: Lens' GetMetricStatistics [Dimension]
gmsDimensions = lens _gmsDimensions (\s a -> s { _gmsDimensions = a }) . _List

-- | The time stamp to use for determining the last datapoint to return. The
-- value specified is exclusive; results will include datapoints up to the
-- time stamp specified.
gmsEndTime :: Lens' GetMetricStatistics UTCTime
gmsEndTime = lens _gmsEndTime (\s a -> s { _gmsEndTime = a }) . _Time

-- | The name of the metric, with or without spaces.
gmsMetricName :: Lens' GetMetricStatistics Text
gmsMetricName = lens _gmsMetricName (\s a -> s { _gmsMetricName = a })

-- | The namespace of the metric, with or without spaces.
gmsNamespace :: Lens' GetMetricStatistics Text
gmsNamespace = lens _gmsNamespace (\s a -> s { _gmsNamespace = a })

-- | The granularity, in seconds, of the returned datapoints. Period must be
-- at least 60 seconds and must be a multiple of 60. The default value is
-- 60.
gmsPeriod :: Lens' GetMetricStatistics Natural
gmsPeriod = lens _gmsPeriod (\s a -> s { _gmsPeriod = a }) . _Nat

-- | The time stamp to use for determining the first datapoint to return. The
-- value specified is inclusive; results include datapoints with the time
-- stamp specified.
gmsStartTime :: Lens' GetMetricStatistics UTCTime
gmsStartTime = lens _gmsStartTime (\s a -> s { _gmsStartTime = a }) . _Time

-- | The metric statistics to return. For information about specific
-- statistics returned by GetMetricStatistics, go to Statistics in the
-- Amazon CloudWatch Developer Guide. Valid Values: Average | Sum |
-- SampleCount | Maximum | Minimum.
gmsStatistics :: Lens' GetMetricStatistics (NonEmpty Text)
gmsStatistics = lens _gmsStatistics (\s a -> s { _gmsStatistics = a }) . _List1

-- | The unit for the metric.
gmsUnit :: Lens' GetMetricStatistics (Maybe Text)
gmsUnit = lens _gmsUnit (\s a -> s { _gmsUnit = a })

data GetMetricStatisticsResponse = GetMetricStatisticsResponse
    { _gmsrDatapoints :: List "Datapoints" Datapoint
    , _gmsrLabel      :: Maybe Text
    } deriving (Eq, Show)

-- | 'GetMetricStatisticsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsrDatapoints' @::@ ['Datapoint']
--
-- * 'gmsrLabel' @::@ 'Maybe' 'Text'
--
getMetricStatisticsResponse :: GetMetricStatisticsResponse
getMetricStatisticsResponse = GetMetricStatisticsResponse
    { _gmsrLabel      = Nothing
    , _gmsrDatapoints = mempty
    }

-- | The datapoints for the specified metric.
gmsrDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrDatapoints = lens _gmsrDatapoints (\s a -> s { _gmsrDatapoints = a }) . _List

-- | A label describing the specified metric.
gmsrLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrLabel = lens _gmsrLabel (\s a -> s { _gmsrLabel = a })

instance ToPath GetMetricStatistics where
    toPath = const "/"

instance ToQuery GetMetricStatistics where
    toQuery GetMetricStatistics{..} = mconcat
        [ "Dimensions" =? _gmsDimensions
        , "EndTime"    =? _gmsEndTime
        , "MetricName" =? _gmsMetricName
        , "Namespace"  =? _gmsNamespace
        , "Period"     =? _gmsPeriod
        , "StartTime"  =? _gmsStartTime
        , "Statistics" =? _gmsStatistics
        , "Unit"       =? _gmsUnit
        ]

instance ToHeaders GetMetricStatistics

query

instance AWSRequest GetMetricStatistics where
    type Sv GetMetricStatistics = CloudWatch
    type Rs GetMetricStatistics = GetMetricStatisticsResponse

    request  = post "GetMetricStatistics"
    response = xmlResponse

instance FromXML GetMetricStatisticsResponse where
    parseXML = withElement "GetMetricStatisticsResult" $ \x -> GetMetricStatisticsResponse
        <$> x .@  "Datapoints"
        <*> x .@? "Label"

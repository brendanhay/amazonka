{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets statistics for the specified metric. The maximum number of data points
-- returned from a single GetMetricStatistics request is 1,440. If a request
-- is made that generates more than 1,440 data points, Amazon CloudWatch
-- returns an error. In such a case, alter the request by narrowing the
-- specified time range or increasing the specified period. Alternatively,
-- make multiple requests across adjacent time ranges. Amazon CloudWatch
-- aggregates data points based on the length of the period that you specify.
-- For example, if you request statistics with a one-minute granularity,
-- Amazon CloudWatch aggregates data points with time stamps that fall within
-- the same one-minute period. In such a case, the data points queried can
-- greatly outnumber the data points returned. The maximum number of data
-- points that can be queried is 50,850; whereas the maximum number of data
-- points returned is 1,440. The following examples show various statistics
-- allowed by the data point query maximum of 50,850 when you call
-- GetMetricStatistics on Amazon EC2 instances with detailed (one-minute)
-- monitoring enabled: Statistics for up to 400 instances for a span of one
-- hour Statistics for up to 35 instances over a span of 24 hours Statistics
-- for up to 2 instances over a span of 2 weeks.
module Network.AWS.CloudWatch
    (
    -- * Request
      GetMetricStatistics
    -- ** Request constructor
    , mkGetMetricStatistics
    -- ** Request lenses
    , gmsNamespace
    , gmsMetricName
    , gmsDimensions
    , gmsStartTime
    , gmsEndTime
    , gmsPeriod
    , gmsStatistics
    , gmsUnit

    -- * Response
    , GetMetricStatisticsResponse
    -- ** Response constructor
    , mkGetMetricStatisticsResponse
    -- ** Response lenses
    , gmsrLabel
    , gmsrDatapoints
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude

data GetMetricStatistics = GetMetricStatistics
    { _gmsNamespace :: !Text
    , _gmsMetricName :: !Text
    , _gmsDimensions :: [Dimension]
    , _gmsStartTime :: !ISO8601
    , _gmsEndTime :: !ISO8601
    , _gmsPeriod :: !Integer
    , _gmsStatistics :: List1 Statistic
    , _gmsUnit :: Maybe StandardUnit
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetMetricStatistics' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Namespace ::@ @Text@
--
-- * @MetricName ::@ @Text@
--
-- * @Dimensions ::@ @[Dimension]@
--
-- * @StartTime ::@ @ISO8601@
--
-- * @EndTime ::@ @ISO8601@
--
-- * @Period ::@ @Integer@
--
-- * @Statistics ::@ @List1 Statistic@
--
-- * @Unit ::@ @Maybe StandardUnit@
--
mkGetMetricStatistics :: Text -- ^ 'gmsNamespace'
                      -> Text -- ^ 'gmsMetricName'
                      -> ISO8601 -- ^ 'gmsStartTime'
                      -> ISO8601 -- ^ 'gmsEndTime'
                      -> Integer -- ^ 'gmsPeriod'
                      -> List1 Statistic -- ^ 'gmsStatistics'
                      -> GetMetricStatistics
mkGetMetricStatistics p1 p2 p4 p5 p6 p7 = GetMetricStatistics
    { _gmsNamespace = p1
    , _gmsMetricName = p2
    , _gmsDimensions = mempty
    , _gmsStartTime = p4
    , _gmsEndTime = p5
    , _gmsPeriod = p6
    , _gmsStatistics = p7
    , _gmsUnit = Nothing
    }

-- | The namespace of the metric.
gmsNamespace :: Lens' GetMetricStatistics Text
gmsNamespace = lens _gmsNamespace (\s a -> s { _gmsNamespace = a })

-- | The name of the metric.
gmsMetricName :: Lens' GetMetricStatistics Text
gmsMetricName = lens _gmsMetricName (\s a -> s { _gmsMetricName = a })

-- | A list of dimensions describing qualities of the metric.
gmsDimensions :: Lens' GetMetricStatistics [Dimension]
gmsDimensions = lens _gmsDimensions (\s a -> s { _gmsDimensions = a })

-- | The time stamp to use for determining the first datapoint to return. The
-- value specified is inclusive; results include datapoints with the time
-- stamp specified. The specified start time is rounded down to the nearest
-- value. Datapoints are returned for start times up to two weeks in the past.
-- Specified start times that are more than two weeks in the past will not
-- return datapoints for metrics that are older than two weeks.
gmsStartTime :: Lens' GetMetricStatistics ISO8601
gmsStartTime = lens _gmsStartTime (\s a -> s { _gmsStartTime = a })

-- | The time stamp to use for determining the last datapoint to return. The
-- value specified is exclusive; results will include datapoints up to the
-- time stamp specified.
gmsEndTime :: Lens' GetMetricStatistics ISO8601
gmsEndTime = lens _gmsEndTime (\s a -> s { _gmsEndTime = a })

-- | The granularity, in seconds, of the returned datapoints. Period must be at
-- least 60 seconds and must be a multiple of 60. The default value is 60.
gmsPeriod :: Lens' GetMetricStatistics Integer
gmsPeriod = lens _gmsPeriod (\s a -> s { _gmsPeriod = a })

-- | The metric statistics to return.
gmsStatistics :: Lens' GetMetricStatistics (List1 Statistic)
gmsStatistics = lens _gmsStatistics (\s a -> s { _gmsStatistics = a })

-- | The unit for the metric.
gmsUnit :: Lens' GetMetricStatistics (Maybe StandardUnit)
gmsUnit = lens _gmsUnit (\s a -> s { _gmsUnit = a })

instance ToQuery GetMetricStatistics where
    toQuery = genericQuery def

-- | The output for the GetMetricStatistics action.
data GetMetricStatisticsResponse = GetMetricStatisticsResponse
    { _gmsrLabel :: !(Maybe Text)
    , _gmsrDatapoints :: [Datapoint]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetMetricStatisticsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Label ::@ @Maybe Text@
--
-- * @Datapoints ::@ @[Datapoint]@
--
mkGetMetricStatisticsResponse :: GetMetricStatisticsResponse
mkGetMetricStatisticsResponse = GetMetricStatisticsResponse
    { _gmsrLabel = Nothing
    , _gmsrDatapoints = mempty
    }

-- | A label describing the specified metric.
gmsrLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrLabel = lens _gmsrLabel (\s a -> s { _gmsrLabel = a })

-- | The datapoints for the specified metric.
gmsrDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrDatapoints = lens _gmsrDatapoints (\s a -> s { _gmsrDatapoints = a })

instance FromXML GetMetricStatisticsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetMetricStatistics where
    type Sv GetMetricStatistics = CloudWatch
    type Rs GetMetricStatistics = GetMetricStatisticsResponse

    request = post "GetMetricStatistics"
    response _ = xmlResponse

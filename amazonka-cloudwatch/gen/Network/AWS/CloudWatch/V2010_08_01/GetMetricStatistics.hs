{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.GetMetricStatistics
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
module Network.AWS.CloudWatch.V2010_08_01.GetMetricStatistics where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetMetricStatistics' request.
getMetricStatistics :: Text -- ^ '_gmsiMetricName'
                    -> Text -- ^ '_gmsiNamespace'
                    -> Integer -- ^ '_gmsiPeriod'
                    -> [Statistic] -- ^ '_gmsiStatistics'
                    -> ISO8601 -- ^ '_gmsiStartTime'
                    -> ISO8601 -- ^ '_gmsiEndTime'
                    -> GetMetricStatistics
getMetricStatistics p1 p2 p3 p4 p5 p6 = GetMetricStatistics
    { _gmsiMetricName = p1
    , _gmsiNamespace = p2
    , _gmsiPeriod = p3
    , _gmsiStatistics = p4
    , _gmsiStartTime = p5
    , _gmsiEndTime = p6
    , _gmsiDimensions = mempty
    , _gmsiUnit = Nothing
    }

data GetMetricStatistics = GetMetricStatistics
    { _gmsiMetricName :: Text
      -- ^ The name of the metric.
    , _gmsiNamespace :: Text
      -- ^ The namespace of the metric.
    , _gmsiPeriod :: Integer
      -- ^ The granularity, in seconds, of the returned datapoints. Period
      -- must be at least 60 seconds and must be a multiple of 60. The
      -- default value is 60.
    , _gmsiStatistics :: [Statistic]
      -- ^ The metric statistics to return.
    , _gmsiStartTime :: ISO8601
      -- ^ The time stamp to use for determining the first datapoint to
      -- return. The value specified is inclusive; results include
      -- datapoints with the time stamp specified. The specified start
      -- time is rounded down to the nearest value. Datapoints are
      -- returned for start times up to two weeks in the past. Specified
      -- start times that are more than two weeks in the past will not
      -- return datapoints for metrics that are older than two weeks.
    , _gmsiEndTime :: ISO8601
      -- ^ The time stamp to use for determining the last datapoint to
      -- return. The value specified is exclusive; results will include
      -- datapoints up to the time stamp specified.
    , _gmsiDimensions :: [Dimension]
      -- ^ A list of dimensions describing qualities of the metric.
    , _gmsiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    } deriving (Show, Generic)

makeLenses ''GetMetricStatistics

instance ToQuery GetMetricStatistics where
    toQuery = genericQuery def

data GetMetricStatisticsResponse = GetMetricStatisticsResponse
    { _gmsoDatapoints :: [Datapoint]
      -- ^ The datapoints for the specified metric.
    , _gmsoLabel :: Maybe Text
      -- ^ A label describing the specified metric.
    } deriving (Show, Generic)

makeLenses ''GetMetricStatisticsResponse

instance FromXML GetMetricStatisticsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetMetricStatistics where
    type Sv GetMetricStatistics = CloudWatch
    type Rs GetMetricStatistics = GetMetricStatisticsResponse

    request = post "GetMetricStatistics"
    response _ = xmlResponse

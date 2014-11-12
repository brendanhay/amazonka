{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.CloudWatch.GetMetricStatistics
    (
    -- * Request
      GetMetricStatisticsInput
    -- ** Request constructor
    , getMetricStatisticsInput
    -- ** Request lenses
    , gmsiDimensions
    , gmsiEndTime
    , gmsiMetricName
    , gmsiNamespace
    , gmsiPeriod
    , gmsiStartTime
    , gmsiStatistics
    , gmsiUnit

    -- * Response
    , GetMetricStatisticsOutput
    -- ** Response constructor
    , getMetricStatisticsOutput
    -- ** Response lenses
    , gmsoDatapoints
    , gmsoLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data GetMetricStatisticsInput = GetMetricStatisticsInput
    { _gmsiDimensions :: [Dimension]
    , _gmsiEndTime    :: RFC822
    , _gmsiMetricName :: Text
    , _gmsiNamespace  :: Text
    , _gmsiPeriod     :: Natural
    , _gmsiStartTime  :: RFC822
    , _gmsiStatistics :: List1 Text
    , _gmsiUnit       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetMetricStatisticsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsiDimensions' @::@ ['Dimension']
--
-- * 'gmsiEndTime' @::@ 'UTCTime'
--
-- * 'gmsiMetricName' @::@ 'Text'
--
-- * 'gmsiNamespace' @::@ 'Text'
--
-- * 'gmsiPeriod' @::@ 'Natural'
--
-- * 'gmsiStartTime' @::@ 'UTCTime'
--
-- * 'gmsiStatistics' @::@ 'NonEmpty' 'Text'
--
-- * 'gmsiUnit' @::@ 'Maybe' 'Text'
--
getMetricStatisticsInput :: Text -- ^ 'gmsiNamespace'
                         -> Text -- ^ 'gmsiMetricName'
                         -> UTCTime -- ^ 'gmsiStartTime'
                         -> UTCTime -- ^ 'gmsiEndTime'
                         -> Natural -- ^ 'gmsiPeriod'
                         -> NonEmpty Text -- ^ 'gmsiStatistics'
                         -> GetMetricStatisticsInput
getMetricStatisticsInput p1 p2 p3 p4 p5 p6 = GetMetricStatisticsInput
    { _gmsiNamespace  = p1
    , _gmsiMetricName = p2
    , _gmsiStartTime  = withIso _Time (const id) p3
    , _gmsiEndTime    = withIso _Time (const id) p4
    , _gmsiPeriod     = p5
    , _gmsiStatistics = withIso _List1 (const id) p6
    , _gmsiDimensions = mempty
    , _gmsiUnit       = Nothing
    }

-- | A list of dimensions describing qualities of the metric.
gmsiDimensions :: Lens' GetMetricStatisticsInput [Dimension]
gmsiDimensions = lens _gmsiDimensions (\s a -> s { _gmsiDimensions = a })

-- | The time stamp to use for determining the last datapoint to return. The
-- value specified is exclusive; results will include datapoints up to the
-- time stamp specified.
gmsiEndTime :: Lens' GetMetricStatisticsInput UTCTime
gmsiEndTime = lens _gmsiEndTime (\s a -> s { _gmsiEndTime = a })
    . _Time

-- | The name of the metric, with or without spaces.
gmsiMetricName :: Lens' GetMetricStatisticsInput Text
gmsiMetricName = lens _gmsiMetricName (\s a -> s { _gmsiMetricName = a })

-- | The namespace of the metric, with or without spaces.
gmsiNamespace :: Lens' GetMetricStatisticsInput Text
gmsiNamespace = lens _gmsiNamespace (\s a -> s { _gmsiNamespace = a })

-- | The granularity, in seconds, of the returned datapoints. Period must be
-- at least 60 seconds and must be a multiple of 60. The default value is
-- 60.
gmsiPeriod :: Lens' GetMetricStatisticsInput Natural
gmsiPeriod = lens _gmsiPeriod (\s a -> s { _gmsiPeriod = a })

-- | The time stamp to use for determining the first datapoint to return. The
-- value specified is inclusive; results include datapoints with the time
-- stamp specified.
gmsiStartTime :: Lens' GetMetricStatisticsInput UTCTime
gmsiStartTime = lens _gmsiStartTime (\s a -> s { _gmsiStartTime = a })
    . _Time

-- | The metric statistics to return. For information about specific
-- statistics returned by GetMetricStatistics, go to Statistics in the
-- Amazon CloudWatch Developer Guide. Valid Values: Average | Sum |
-- SampleCount | Maximum | Minimum.
gmsiStatistics :: Lens' GetMetricStatisticsInput (NonEmpty Text)
gmsiStatistics = lens _gmsiStatistics (\s a -> s { _gmsiStatistics = a })
    . _List1

-- | The unit for the metric.
gmsiUnit :: Lens' GetMetricStatisticsInput (Maybe Text)
gmsiUnit = lens _gmsiUnit (\s a -> s { _gmsiUnit = a })

instance ToQuery GetMetricStatisticsInput

instance ToPath GetMetricStatisticsInput where
    toPath = const "/"

data GetMetricStatisticsOutput = GetMetricStatisticsOutput
    { _gmsoDatapoints :: [Datapoint]
    , _gmsoLabel      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetMetricStatisticsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsoDatapoints' @::@ ['Datapoint']
--
-- * 'gmsoLabel' @::@ 'Maybe' 'Text'
--
getMetricStatisticsOutput :: GetMetricStatisticsOutput
getMetricStatisticsOutput = GetMetricStatisticsOutput
    { _gmsoLabel      = Nothing
    , _gmsoDatapoints = mempty
    }

-- | The datapoints for the specified metric.
gmsoDatapoints :: Lens' GetMetricStatisticsOutput [Datapoint]
gmsoDatapoints = lens _gmsoDatapoints (\s a -> s { _gmsoDatapoints = a })

-- | A label describing the specified metric.
gmsoLabel :: Lens' GetMetricStatisticsOutput (Maybe Text)
gmsoLabel = lens _gmsoLabel (\s a -> s { _gmsoLabel = a })

instance FromXML GetMetricStatisticsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetMetricStatisticsOutput"

instance AWSRequest GetMetricStatisticsInput where
    type Sv GetMetricStatisticsInput = CloudWatch
    type Rs GetMetricStatisticsInput = GetMetricStatisticsOutput

    request  = post "GetMetricStatistics"
    response = xmlResponse $ \h x -> GetMetricStatisticsOutput
        <$> x %| "Datapoints"
        <*> x %| "Label"

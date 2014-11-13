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

-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
module Network.AWS.CloudWatch.DescribeAlarmsForMetric
    (
    -- * Request
      DescribeAlarmsForMetric
    -- ** Request constructor
    , describeAlarmsForMetric
    -- ** Request lenses
    , dafmDimensions
    , dafmMetricName
    , dafmNamespace
    , dafmPeriod
    , dafmStatistic
    , dafmUnit

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response constructor
    , describeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmrMetricAlarms
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmDimensions :: [Dimension]
    , _dafmMetricName :: Text
    , _dafmNamespace  :: Text
    , _dafmPeriod     :: Maybe Natural
    , _dafmStatistic  :: Maybe Text
    , _dafmUnit       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAlarmsForMetric' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmDimensions' @::@ ['Dimension']
--
-- * 'dafmMetricName' @::@ 'Text'
--
-- * 'dafmNamespace' @::@ 'Text'
--
-- * 'dafmPeriod' @::@ 'Maybe' 'Natural'
--
-- * 'dafmStatistic' @::@ 'Maybe' 'Text'
--
-- * 'dafmUnit' @::@ 'Maybe' 'Text'
--
describeAlarmsForMetric :: Text -- ^ 'dafmMetricName'
                        -> Text -- ^ 'dafmNamespace'
                        -> DescribeAlarmsForMetric
describeAlarmsForMetric p1 p2 = DescribeAlarmsForMetric
    { _dafmMetricName = p1
    , _dafmNamespace  = p2
    , _dafmStatistic  = Nothing
    , _dafmDimensions = mempty
    , _dafmPeriod     = Nothing
    , _dafmUnit       = Nothing
    }

-- | The list of dimensions associated with the metric.
dafmDimensions :: Lens' DescribeAlarmsForMetric [Dimension]
dafmDimensions = lens _dafmDimensions (\s a -> s { _dafmDimensions = a })

-- | The name of the metric.
dafmMetricName :: Lens' DescribeAlarmsForMetric Text
dafmMetricName = lens _dafmMetricName (\s a -> s { _dafmMetricName = a })

-- | The namespace of the metric.
dafmNamespace :: Lens' DescribeAlarmsForMetric Text
dafmNamespace = lens _dafmNamespace (\s a -> s { _dafmNamespace = a })

-- | The period in seconds over which the statistic is applied.
dafmPeriod :: Lens' DescribeAlarmsForMetric (Maybe Natural)
dafmPeriod = lens _dafmPeriod (\s a -> s { _dafmPeriod = a })

-- | The statistic for the metric.
dafmStatistic :: Lens' DescribeAlarmsForMetric (Maybe Text)
dafmStatistic = lens _dafmStatistic (\s a -> s { _dafmStatistic = a })

-- | The unit for the metric.
dafmUnit :: Lens' DescribeAlarmsForMetric (Maybe Text)
dafmUnit = lens _dafmUnit (\s a -> s { _dafmUnit = a })

instance ToQuery DescribeAlarmsForMetric

instance ToPath DescribeAlarmsForMetric where
    toPath = const "/"

newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmrMetricAlarms :: [MetricAlarm]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeAlarmsForMetricResponse where
    type Item DescribeAlarmsForMetricResponse = MetricAlarm

    fromList = DescribeAlarmsForMetricResponse . fromList
    toList   = toList . _dafmrMetricAlarms

-- | 'DescribeAlarmsForMetricResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrMetricAlarms' @::@ ['MetricAlarm']
--
describeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse
describeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmrMetricAlarms = mempty
    }

-- | A list of information for each alarm with the specified metric.
dafmrMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrMetricAlarms =
    lens _dafmrMetricAlarms (\s a -> s { _dafmrMetricAlarms = a })

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAlarmsForMetricResponse"

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request  = post "DescribeAlarmsForMetric"
    response = xmlResponse $ \h x -> DescribeAlarmsForMetricResponse
        <$> x %| "MetricAlarms"

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
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsForMetric.html>
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
import qualified GHC.Exts

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmDimensions :: List "Dimensions" Dimension
    , _dafmMetricName :: Text
    , _dafmNamespace  :: Text
    , _dafmPeriod     :: Maybe Nat
    , _dafmStatistic  :: Maybe Text
    , _dafmUnit       :: Maybe Text
    } deriving (Eq, Show)

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
dafmDimensions = lens _dafmDimensions (\s a -> s { _dafmDimensions = a }) . _List

-- | The name of the metric.
dafmMetricName :: Lens' DescribeAlarmsForMetric Text
dafmMetricName = lens _dafmMetricName (\s a -> s { _dafmMetricName = a })

-- | The namespace of the metric.
dafmNamespace :: Lens' DescribeAlarmsForMetric Text
dafmNamespace = lens _dafmNamespace (\s a -> s { _dafmNamespace = a })

-- | The period in seconds over which the statistic is applied.
dafmPeriod :: Lens' DescribeAlarmsForMetric (Maybe Natural)
dafmPeriod = lens _dafmPeriod (\s a -> s { _dafmPeriod = a }) . mapping _Nat

-- | The statistic for the metric.
dafmStatistic :: Lens' DescribeAlarmsForMetric (Maybe Text)
dafmStatistic = lens _dafmStatistic (\s a -> s { _dafmStatistic = a })

-- | The unit for the metric.
dafmUnit :: Lens' DescribeAlarmsForMetric (Maybe Text)
dafmUnit = lens _dafmUnit (\s a -> s { _dafmUnit = a })

newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmrMetricAlarms :: List "MetricAlarms" MetricAlarm
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAlarmsForMetricResponse where
    type Item DescribeAlarmsForMetricResponse = MetricAlarm

    fromList = DescribeAlarmsForMetricResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dafmrMetricAlarms

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
        . _List

instance ToPath DescribeAlarmsForMetric where
    toPath = const "/"

instance ToQuery DescribeAlarmsForMetric where
    toQuery DescribeAlarmsForMetric{..} = mconcat
        [ "Dimensions" =? _dafmDimensions
        , "MetricName" =? _dafmMetricName
        , "Namespace"  =? _dafmNamespace
        , "Period"     =? _dafmPeriod
        , "Statistic"  =? _dafmStatistic
        , "Unit"       =? _dafmUnit
        ]

instance ToHeaders DescribeAlarmsForMetric

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request  = post "DescribeAlarmsForMetric"
    response = xmlResponse

instance FromXML DescribeAlarmsForMetricResponse where
    parseXML = withElement "DescribeAlarmsForMetricResult" $ \x -> DescribeAlarmsForMetricResponse
        <$> x .@  "MetricAlarms"

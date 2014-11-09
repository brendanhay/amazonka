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
      DescribeAlarmsForMetricInput
    -- ** Request constructor
    , describeAlarmsForMetricInput
    -- ** Request lenses
    , dafmiDimensions
    , dafmiMetricName
    , dafmiNamespace
    , dafmiPeriod
    , dafmiStatistic
    , dafmiUnit

    -- * Response
    , DescribeAlarmsForMetricOutput
    -- ** Response constructor
    , describeAlarmsForMetricOutput
    -- ** Response lenses
    , dafmoMetricAlarms
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data DescribeAlarmsForMetricInput = DescribeAlarmsForMetricInput
    { _dafmiDimensions :: [Dimension]
    , _dafmiMetricName :: Text
    , _dafmiNamespace  :: Text
    , _dafmiPeriod     :: Maybe Int
    , _dafmiStatistic  :: Maybe Text
    , _dafmiUnit       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAlarmsForMetricInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmiDimensions' @::@ ['Dimension']
--
-- * 'dafmiMetricName' @::@ 'Text'
--
-- * 'dafmiNamespace' @::@ 'Text'
--
-- * 'dafmiPeriod' @::@ 'Maybe' 'Int'
--
-- * 'dafmiStatistic' @::@ 'Maybe' 'Text'
--
-- * 'dafmiUnit' @::@ 'Maybe' 'Text'
--
describeAlarmsForMetricInput :: Text -- ^ 'dafmiMetricName'
                             -> Text -- ^ 'dafmiNamespace'
                             -> DescribeAlarmsForMetricInput
describeAlarmsForMetricInput p1 p2 = DescribeAlarmsForMetricInput
    { _dafmiMetricName = p1
    , _dafmiNamespace  = p2
    , _dafmiStatistic  = Nothing
    , _dafmiDimensions = mempty
    , _dafmiPeriod     = Nothing
    , _dafmiUnit       = Nothing
    }

-- | The list of dimensions associated with the metric.
dafmiDimensions :: Lens' DescribeAlarmsForMetricInput [Dimension]
dafmiDimensions = lens _dafmiDimensions (\s a -> s { _dafmiDimensions = a })

-- | The name of the metric.
dafmiMetricName :: Lens' DescribeAlarmsForMetricInput Text
dafmiMetricName = lens _dafmiMetricName (\s a -> s { _dafmiMetricName = a })

-- | The namespace of the metric.
dafmiNamespace :: Lens' DescribeAlarmsForMetricInput Text
dafmiNamespace = lens _dafmiNamespace (\s a -> s { _dafmiNamespace = a })

-- | The period in seconds over which the statistic is applied.
dafmiPeriod :: Lens' DescribeAlarmsForMetricInput (Maybe Int)
dafmiPeriod = lens _dafmiPeriod (\s a -> s { _dafmiPeriod = a })

-- | The statistic for the metric.
dafmiStatistic :: Lens' DescribeAlarmsForMetricInput (Maybe Text)
dafmiStatistic = lens _dafmiStatistic (\s a -> s { _dafmiStatistic = a })

-- | The unit for the metric.
dafmiUnit :: Lens' DescribeAlarmsForMetricInput (Maybe Text)
dafmiUnit = lens _dafmiUnit (\s a -> s { _dafmiUnit = a })

instance ToPath DescribeAlarmsForMetricInput where
    toPath = const "/"

instance ToQuery DescribeAlarmsForMetricInput

newtype DescribeAlarmsForMetricOutput = DescribeAlarmsForMetricOutput
    { _dafmoMetricAlarms :: [MetricAlarm]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeAlarmsForMetricOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmoMetricAlarms' @::@ ['MetricAlarm']
--
describeAlarmsForMetricOutput :: DescribeAlarmsForMetricOutput
describeAlarmsForMetricOutput = DescribeAlarmsForMetricOutput
    { _dafmoMetricAlarms = mempty
    }

-- | A list of information for each alarm with the specified metric.
dafmoMetricAlarms :: Lens' DescribeAlarmsForMetricOutput [MetricAlarm]
dafmoMetricAlarms =
    lens _dafmoMetricAlarms (\s a -> s { _dafmoMetricAlarms = a })

instance AWSRequest DescribeAlarmsForMetricInput where
    type Sv DescribeAlarmsForMetricInput = CloudWatch
    type Rs DescribeAlarmsForMetricInput = DescribeAlarmsForMetricOutput

    request  = post "DescribeAlarmsForMetric"
    response = const . xmlResponse $ \h x -> DescribeAlarmsForMetricOutput
newtype

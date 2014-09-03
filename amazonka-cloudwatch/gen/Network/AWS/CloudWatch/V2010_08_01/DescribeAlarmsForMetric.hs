{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric
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
module Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric
    (
    -- * Request
      DescribeAlarmsForMetric
    -- ** Request constructor
    , describeAlarmsForMetric
    -- ** Request lenses
    , dafmiMetricName
    , dafmiNamespace
    , dafmiDimensions
    , dafmiPeriod
    , dafmiUnit
    , dafmiStatistic

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmoMetricAlarms
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAlarmsForMetric' request.
describeAlarmsForMetric :: Text -- ^ 'dafmiMetricName'
                        -> Text -- ^ 'dafmiNamespace'
                        -> DescribeAlarmsForMetric
describeAlarmsForMetric p1 p2 = DescribeAlarmsForMetric
    { _dafmiMetricName = p1
    , _dafmiNamespace = p2
    , _dafmiDimensions = mempty
    , _dafmiPeriod = Nothing
    , _dafmiUnit = Nothing
    , _dafmiStatistic = Nothing
    }

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmiMetricName :: Text
      -- ^ The name of the metric.
    , _dafmiNamespace :: Text
      -- ^ The namespace of the metric.
    , _dafmiDimensions :: [Dimension]
      -- ^ The list of dimensions associated with the metric.
    , _dafmiPeriod :: Maybe Integer
      -- ^ The period in seconds over which the statistic is applied.
    , _dafmiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    , _dafmiStatistic :: Maybe Statistic
      -- ^ The statistic for the metric.
    } deriving (Show, Generic)

-- | The name of the metric.
dafmiMetricName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiMetricName f x =
    (\y -> x { _dafmiMetricName = y })
       <$> f (_dafmiMetricName x)
{-# INLINE dafmiMetricName #-}

-- | The namespace of the metric.
dafmiNamespace
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiNamespace f x =
    (\y -> x { _dafmiNamespace = y })
       <$> f (_dafmiNamespace x)
{-# INLINE dafmiNamespace #-}

-- | The list of dimensions associated with the metric.
dafmiDimensions
    :: Functor f
    => ([Dimension]
    -> f ([Dimension]))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiDimensions f x =
    (\y -> x { _dafmiDimensions = y })
       <$> f (_dafmiDimensions x)
{-# INLINE dafmiDimensions #-}

-- | The period in seconds over which the statistic is applied.
dafmiPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiPeriod f x =
    (\y -> x { _dafmiPeriod = y })
       <$> f (_dafmiPeriod x)
{-# INLINE dafmiPeriod #-}

-- | The unit for the metric.
dafmiUnit
    :: Functor f
    => (Maybe StandardUnit
    -> f (Maybe StandardUnit))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiUnit f x =
    (\y -> x { _dafmiUnit = y })
       <$> f (_dafmiUnit x)
{-# INLINE dafmiUnit #-}

-- | The statistic for the metric.
dafmiStatistic
    :: Functor f
    => (Maybe Statistic
    -> f (Maybe Statistic))
    -> DescribeAlarmsForMetric
    -> f DescribeAlarmsForMetric
dafmiStatistic f x =
    (\y -> x { _dafmiStatistic = y })
       <$> f (_dafmiStatistic x)
{-# INLINE dafmiStatistic #-}

instance ToQuery DescribeAlarmsForMetric where
    toQuery = genericQuery def

data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmoMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for each alarm with the specified metric.
    } deriving (Show, Generic)

-- | A list of information for each alarm with the specified metric.
dafmoMetricAlarms
    :: Functor f
    => ([MetricAlarm]
    -> f ([MetricAlarm]))
    -> DescribeAlarmsForMetricResponse
    -> f DescribeAlarmsForMetricResponse
dafmoMetricAlarms f x =
    (\y -> x { _dafmoMetricAlarms = y })
       <$> f (_dafmoMetricAlarms x)
{-# INLINE dafmoMetricAlarms #-}

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request = post "DescribeAlarmsForMetric"
    response _ = xmlResponse

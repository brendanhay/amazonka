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
    , mkDescribeAlarmsForMetricInput
    -- ** Request lenses
    , dafmiMetricName
    , dafmiNamespace
    , dafmiStatistic
    , dafmiDimensions
    , dafmiPeriod
    , dafmiUnit

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmoMetricAlarms
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmsForMetric' request.
mkDescribeAlarmsForMetricInput :: Text -- ^ 'dafmiMetricName'
                               -> Text -- ^ 'dafmiNamespace'
                               -> DescribeAlarmsForMetric
mkDescribeAlarmsForMetricInput p1 p2 = DescribeAlarmsForMetric
    { _dafmiMetricName = p1
    , _dafmiNamespace = p2
    , _dafmiStatistic = Nothing
    , _dafmiDimensions = mempty
    , _dafmiPeriod = Nothing
    , _dafmiUnit = Nothing
    }
{-# INLINE mkDescribeAlarmsForMetricInput #-}

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmiMetricName :: Text
      -- ^ The name of the metric.
    , _dafmiNamespace :: Text
      -- ^ The namespace of the metric.
    , _dafmiStatistic :: Maybe Statistic
      -- ^ The statistic for the metric.
    , _dafmiDimensions :: [Dimension]
      -- ^ The list of dimensions associated with the metric.
    , _dafmiPeriod :: Maybe Integer
      -- ^ The period in seconds over which the statistic is applied.
    , _dafmiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    } deriving (Show, Generic)

-- | The name of the metric.
dafmiMetricName :: Lens' DescribeAlarmsForMetric (Text)
dafmiMetricName = lens _dafmiMetricName (\s a -> s { _dafmiMetricName = a })
{-# INLINE dafmiMetricName #-}

-- | The namespace of the metric.
dafmiNamespace :: Lens' DescribeAlarmsForMetric (Text)
dafmiNamespace = lens _dafmiNamespace (\s a -> s { _dafmiNamespace = a })
{-# INLINE dafmiNamespace #-}

-- | The statistic for the metric.
dafmiStatistic :: Lens' DescribeAlarmsForMetric (Maybe Statistic)
dafmiStatistic = lens _dafmiStatistic (\s a -> s { _dafmiStatistic = a })
{-# INLINE dafmiStatistic #-}

-- | The list of dimensions associated with the metric.
dafmiDimensions :: Lens' DescribeAlarmsForMetric ([Dimension])
dafmiDimensions = lens _dafmiDimensions (\s a -> s { _dafmiDimensions = a })
{-# INLINE dafmiDimensions #-}

-- | The period in seconds over which the statistic is applied.
dafmiPeriod :: Lens' DescribeAlarmsForMetric (Maybe Integer)
dafmiPeriod = lens _dafmiPeriod (\s a -> s { _dafmiPeriod = a })
{-# INLINE dafmiPeriod #-}

-- | The unit for the metric.
dafmiUnit :: Lens' DescribeAlarmsForMetric (Maybe StandardUnit)
dafmiUnit = lens _dafmiUnit (\s a -> s { _dafmiUnit = a })
{-# INLINE dafmiUnit #-}

instance ToQuery DescribeAlarmsForMetric where
    toQuery = genericQuery def

newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmoMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for each alarm with the specified metric.
    } deriving (Show, Generic)

-- | A list of information for each alarm with the specified metric.
dafmoMetricAlarms :: Lens' DescribeAlarmsForMetricResponse ([MetricAlarm])
dafmoMetricAlarms = lens _dafmoMetricAlarms (\s a -> s { _dafmoMetricAlarms = a })
{-# INLINE dafmoMetricAlarms #-}

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request = post "DescribeAlarmsForMetric"
    response _ = xmlResponse

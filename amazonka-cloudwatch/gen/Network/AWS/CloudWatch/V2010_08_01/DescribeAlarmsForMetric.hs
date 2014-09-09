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
    , mkDescribeAlarmsForMetric
    -- ** Request lenses
    , dafmMetricName
    , dafmNamespace
    , dafmStatistic
    , dafmDimensions
    , dafmPeriod
    , dafmUnit

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response constructor
    , mkDescribeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmrMetricAlarms
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmMetricName :: Text
    , _dafmNamespace :: Text
    , _dafmStatistic :: Maybe Statistic
    , _dafmDimensions :: [Dimension]
    , _dafmPeriod :: Maybe Integer
    , _dafmUnit :: Maybe StandardUnit
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmsForMetric' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MetricName ::@ @Text@
--
-- * @Namespace ::@ @Text@
--
-- * @Statistic ::@ @Maybe Statistic@
--
-- * @Dimensions ::@ @[Dimension]@
--
-- * @Period ::@ @Maybe Integer@
--
-- * @Unit ::@ @Maybe StandardUnit@
--
mkDescribeAlarmsForMetric :: Text -- ^ 'dafmMetricName'
                          -> Text -- ^ 'dafmNamespace'
                          -> DescribeAlarmsForMetric
mkDescribeAlarmsForMetric p1 p2 = DescribeAlarmsForMetric
    { _dafmMetricName = p1
    , _dafmNamespace = p2
    , _dafmStatistic = Nothing
    , _dafmDimensions = mempty
    , _dafmPeriod = Nothing
    , _dafmUnit = Nothing
    }

-- | The name of the metric.
dafmMetricName :: Lens' DescribeAlarmsForMetric Text
dafmMetricName = lens _dafmMetricName (\s a -> s { _dafmMetricName = a })

-- | The namespace of the metric.
dafmNamespace :: Lens' DescribeAlarmsForMetric Text
dafmNamespace = lens _dafmNamespace (\s a -> s { _dafmNamespace = a })

-- | The statistic for the metric.
dafmStatistic :: Lens' DescribeAlarmsForMetric (Maybe Statistic)
dafmStatistic = lens _dafmStatistic (\s a -> s { _dafmStatistic = a })

-- | The list of dimensions associated with the metric.
dafmDimensions :: Lens' DescribeAlarmsForMetric [Dimension]
dafmDimensions = lens _dafmDimensions (\s a -> s { _dafmDimensions = a })

-- | The period in seconds over which the statistic is applied.
dafmPeriod :: Lens' DescribeAlarmsForMetric (Maybe Integer)
dafmPeriod = lens _dafmPeriod (\s a -> s { _dafmPeriod = a })

-- | The unit for the metric.
dafmUnit :: Lens' DescribeAlarmsForMetric (Maybe StandardUnit)
dafmUnit = lens _dafmUnit (\s a -> s { _dafmUnit = a })

instance ToQuery DescribeAlarmsForMetric where
    toQuery = genericQuery def

-- | The output for the DescribeAlarmsForMetric action.
newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmrMetricAlarms :: [MetricAlarm]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmsForMetricResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MetricAlarms ::@ @[MetricAlarm]@
--
mkDescribeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse
mkDescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmrMetricAlarms = mempty
    }

-- | A list of information for each alarm with the specified metric.
dafmrMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrMetricAlarms =
    lens _dafmrMetricAlarms (\s a -> s { _dafmrMetricAlarms = a })

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request = post "DescribeAlarmsForMetric"
    response _ = xmlResponse

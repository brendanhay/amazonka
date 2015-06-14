{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves all alarms for a single metric. Specify a statistic, period,
-- or unit to filter the set of alarms further.
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
    , dafmStatistic
    , dafmUnit
    , dafmMetricName
    , dafmNamespace
    , dafmPeriod

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response constructor
    , describeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmrMetricAlarms
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatch.Types

-- | /See:/ 'describeAlarmsForMetric' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmDimensions'
--
-- * 'dafmStatistic'
--
-- * 'dafmUnit'
--
-- * 'dafmMetricName'
--
-- * 'dafmNamespace'
--
-- * 'dafmPeriod'
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'{_dafmDimensions :: [Dimension], _dafmStatistic :: Maybe Statistic, _dafmUnit :: Maybe StandardUnit, _dafmMetricName :: Text, _dafmNamespace :: Text, _dafmPeriod :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeAlarmsForMetric' smart constructor.
describeAlarmsForMetric :: Text -> Text -> Natural -> DescribeAlarmsForMetric
describeAlarmsForMetric pMetricName pNamespace pPeriod = DescribeAlarmsForMetric'{_dafmDimensions = mempty, _dafmStatistic = Nothing, _dafmUnit = Nothing, _dafmMetricName = pMetricName, _dafmNamespace = pNamespace, _dafmPeriod = _Nat # pPeriod};

-- | The list of dimensions associated with the metric.
dafmDimensions :: Lens' DescribeAlarmsForMetric [Dimension]
dafmDimensions = lens _dafmDimensions (\ s a -> s{_dafmDimensions = a});

-- | The statistic for the metric.
dafmStatistic :: Lens' DescribeAlarmsForMetric (Maybe Statistic)
dafmStatistic = lens _dafmStatistic (\ s a -> s{_dafmStatistic = a});

-- | The unit for the metric.
dafmUnit :: Lens' DescribeAlarmsForMetric (Maybe StandardUnit)
dafmUnit = lens _dafmUnit (\ s a -> s{_dafmUnit = a});

-- | The name of the metric.
dafmMetricName :: Lens' DescribeAlarmsForMetric Text
dafmMetricName = lens _dafmMetricName (\ s a -> s{_dafmMetricName = a});

-- | The namespace of the metric.
dafmNamespace :: Lens' DescribeAlarmsForMetric Text
dafmNamespace = lens _dafmNamespace (\ s a -> s{_dafmNamespace = a});

-- | The period in seconds over which the statistic is applied.
dafmPeriod :: Lens' DescribeAlarmsForMetric Natural
dafmPeriod = lens _dafmPeriod (\ s a -> s{_dafmPeriod = a}) . _Nat;

instance AWSRequest DescribeAlarmsForMetric where
        type Sv DescribeAlarmsForMetric = CloudWatch
        type Rs DescribeAlarmsForMetric =
             DescribeAlarmsForMetricResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAlarmsForMetricResult"
              (\ s h x ->
                 DescribeAlarmsForMetricResponse' <$>
                   (x .@? "MetricAlarms" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeAlarmsForMetric where
        toHeaders = const mempty

instance ToPath DescribeAlarmsForMetric where
        toPath = const "/"

instance ToQuery DescribeAlarmsForMetric where
        toQuery DescribeAlarmsForMetric'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAlarmsForMetric" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "Dimensions" =: "member" =: _dafmDimensions,
               "Statistic" =: _dafmStatistic, "Unit" =: _dafmUnit,
               "MetricName" =: _dafmMetricName,
               "Namespace" =: _dafmNamespace,
               "Period" =: _dafmPeriod]

-- | /See:/ 'describeAlarmsForMetricResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrMetricAlarms'
newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'{_dafmrMetricAlarms :: [MetricAlarm]} deriving (Eq, Read, Show)

-- | 'DescribeAlarmsForMetricResponse' smart constructor.
describeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse
describeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'{_dafmrMetricAlarms = mempty};

-- | A list of information for each alarm with the specified metric.
dafmrMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrMetricAlarms = lens _dafmrMetricAlarms (\ s a -> s{_dafmrMetricAlarms = a});

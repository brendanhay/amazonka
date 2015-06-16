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
    , dafmPeriod
    , dafmDimensions
    , dafmStatistic
    , dafmUnit
    , dafmMetricName
    , dafmNamespace

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
-- * 'dafmPeriod'
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
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'{_dafmPeriod :: Maybe Nat, _dafmDimensions :: Maybe [Dimension], _dafmStatistic :: Maybe Statistic, _dafmUnit :: Maybe StandardUnit, _dafmMetricName :: Text, _dafmNamespace :: Text} deriving (Eq, Read, Show)

-- | 'DescribeAlarmsForMetric' smart constructor.
describeAlarmsForMetric :: Text -> Text -> DescribeAlarmsForMetric
describeAlarmsForMetric pMetricName pNamespace = DescribeAlarmsForMetric'{_dafmPeriod = Nothing, _dafmDimensions = Nothing, _dafmStatistic = Nothing, _dafmUnit = Nothing, _dafmMetricName = pMetricName, _dafmNamespace = pNamespace};

-- | The period in seconds over which the statistic is applied.
dafmPeriod :: Lens' DescribeAlarmsForMetric (Maybe Natural)
dafmPeriod = lens _dafmPeriod (\ s a -> s{_dafmPeriod = a}) . mapping _Nat;

-- | The list of dimensions associated with the metric.
dafmDimensions :: Lens' DescribeAlarmsForMetric [Dimension]
dafmDimensions = lens _dafmDimensions (\ s a -> s{_dafmDimensions = a}) . _Default;

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
                      may (parseXMLList "member")))

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
               "Period" =: _dafmPeriod,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _dafmDimensions),
               "Statistic" =: _dafmStatistic, "Unit" =: _dafmUnit,
               "MetricName" =: _dafmMetricName,
               "Namespace" =: _dafmNamespace]

-- | /See:/ 'describeAlarmsForMetricResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrMetricAlarms'
newtype DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'{_dafmrMetricAlarms :: Maybe [MetricAlarm]} deriving (Eq, Read, Show)

-- | 'DescribeAlarmsForMetricResponse' smart constructor.
describeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse
describeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'{_dafmrMetricAlarms = Nothing};

-- | A list of information for each alarm with the specified metric.
dafmrMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrMetricAlarms = lens _dafmrMetricAlarms (\ s a -> s{_dafmrMetricAlarms = a}) . _Default;

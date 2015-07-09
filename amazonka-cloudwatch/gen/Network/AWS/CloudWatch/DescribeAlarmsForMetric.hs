{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , dafmrStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
    { _dafmPeriod     :: !(Maybe Nat)
    , _dafmDimensions :: !(Maybe [Dimension])
    , _dafmStatistic  :: !(Maybe Statistic)
    , _dafmUnit       :: !(Maybe StandardUnit)
    , _dafmMetricName :: !Text
    , _dafmNamespace  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmsForMetric' smart constructor.
describeAlarmsForMetric :: Text -> Text -> DescribeAlarmsForMetric
describeAlarmsForMetric pMetricName pNamespace =
    DescribeAlarmsForMetric'
    { _dafmPeriod = Nothing
    , _dafmDimensions = Nothing
    , _dafmStatistic = Nothing
    , _dafmUnit = Nothing
    , _dafmMetricName = pMetricName
    , _dafmNamespace = pNamespace
    }

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | The output for the DescribeAlarmsForMetric action.
--
-- /See:/ 'describeAlarmsForMetricResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrMetricAlarms'
--
-- * 'dafmrStatus'
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
    { _dafmrMetricAlarms :: !(Maybe [MetricAlarm])
    , _dafmrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmsForMetricResponse' smart constructor.
describeAlarmsForMetricResponse :: Int -> DescribeAlarmsForMetricResponse
describeAlarmsForMetricResponse pStatus =
    DescribeAlarmsForMetricResponse'
    { _dafmrMetricAlarms = Nothing
    , _dafmrStatus = pStatus
    }

-- | A list of information for each alarm with the specified metric.
dafmrMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrMetricAlarms = lens _dafmrMetricAlarms (\ s a -> s{_dafmrMetricAlarms = a}) . _Default;

-- | FIXME: Undocumented member.
dafmrStatus :: Lens' DescribeAlarmsForMetricResponse Int
dafmrStatus = lens _dafmrStatus (\ s a -> s{_dafmrStatus = a});

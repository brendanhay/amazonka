{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all alarms for a single metric. Specify a statistic, period,
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
    , dafmrqPeriod
    , dafmrqDimensions
    , dafmrqStatistic
    , dafmrqUnit
    , dafmrqMetricName
    , dafmrqNamespace

    -- * Response
    , DescribeAlarmsForMetricResponse
    -- ** Response constructor
    , describeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmrsMetricAlarms
    , dafmrsStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAlarmsForMetric' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrqPeriod'
--
-- * 'dafmrqDimensions'
--
-- * 'dafmrqStatistic'
--
-- * 'dafmrqUnit'
--
-- * 'dafmrqMetricName'
--
-- * 'dafmrqNamespace'
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
    { _dafmrqPeriod     :: !(Maybe Nat)
    , _dafmrqDimensions :: !(Maybe [Dimension])
    , _dafmrqStatistic  :: !(Maybe Statistic)
    , _dafmrqUnit       :: !(Maybe StandardUnit)
    , _dafmrqMetricName :: !Text
    , _dafmrqNamespace  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmsForMetric' smart constructor.
describeAlarmsForMetric :: Text -> Text -> DescribeAlarmsForMetric
describeAlarmsForMetric pMetricName pNamespace =
    DescribeAlarmsForMetric'
    { _dafmrqPeriod = Nothing
    , _dafmrqDimensions = Nothing
    , _dafmrqStatistic = Nothing
    , _dafmrqUnit = Nothing
    , _dafmrqMetricName = pMetricName
    , _dafmrqNamespace = pNamespace
    }

-- | The period in seconds over which the statistic is applied.
dafmrqPeriod :: Lens' DescribeAlarmsForMetric (Maybe Natural)
dafmrqPeriod = lens _dafmrqPeriod (\ s a -> s{_dafmrqPeriod = a}) . mapping _Nat;

-- | The list of dimensions associated with the metric.
dafmrqDimensions :: Lens' DescribeAlarmsForMetric [Dimension]
dafmrqDimensions = lens _dafmrqDimensions (\ s a -> s{_dafmrqDimensions = a}) . _Default;

-- | The statistic for the metric.
dafmrqStatistic :: Lens' DescribeAlarmsForMetric (Maybe Statistic)
dafmrqStatistic = lens _dafmrqStatistic (\ s a -> s{_dafmrqStatistic = a});

-- | The unit for the metric.
dafmrqUnit :: Lens' DescribeAlarmsForMetric (Maybe StandardUnit)
dafmrqUnit = lens _dafmrqUnit (\ s a -> s{_dafmrqUnit = a});

-- | The name of the metric.
dafmrqMetricName :: Lens' DescribeAlarmsForMetric Text
dafmrqMetricName = lens _dafmrqMetricName (\ s a -> s{_dafmrqMetricName = a});

-- | The namespace of the metric.
dafmrqNamespace :: Lens' DescribeAlarmsForMetric Text
dafmrqNamespace = lens _dafmrqNamespace (\ s a -> s{_dafmrqNamespace = a});

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
               "Period" =: _dafmrqPeriod,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _dafmrqDimensions),
               "Statistic" =: _dafmrqStatistic,
               "Unit" =: _dafmrqUnit,
               "MetricName" =: _dafmrqMetricName,
               "Namespace" =: _dafmrqNamespace]

-- | The output for the DescribeAlarmsForMetric action.
--
-- /See:/ 'describeAlarmsForMetricResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dafmrsMetricAlarms'
--
-- * 'dafmrsStatus'
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
    { _dafmrsMetricAlarms :: !(Maybe [MetricAlarm])
    , _dafmrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmsForMetricResponse' smart constructor.
describeAlarmsForMetricResponse :: Int -> DescribeAlarmsForMetricResponse
describeAlarmsForMetricResponse pStatus =
    DescribeAlarmsForMetricResponse'
    { _dafmrsMetricAlarms = Nothing
    , _dafmrsStatus = pStatus
    }

-- | A list of information for each alarm with the specified metric.
dafmrsMetricAlarms :: Lens' DescribeAlarmsForMetricResponse [MetricAlarm]
dafmrsMetricAlarms = lens _dafmrsMetricAlarms (\ s a -> s{_dafmrsMetricAlarms = a}) . _Default;

-- | FIXME: Undocumented member.
dafmrsStatus :: Lens' DescribeAlarmsForMetricResponse Int
dafmrsStatus = lens _dafmrsStatus (\ s a -> s{_dafmrsStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only
-- a prefix for the alarm name, the alarm state, or a prefix for any
-- action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html>
module Network.AWS.CloudWatch.DescribeAlarms
    (
    -- * Request
      DescribeAlarms
    -- ** Request constructor
    , describeAlarms
    -- ** Request lenses
    , darqAlarmNamePrefix
    , darqActionPrefix
    , darqNextToken
    , darqStateValue
    , darqAlarmNames
    , darqMaxRecords

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response constructor
    , describeAlarmsResponse
    -- ** Response lenses
    , darsMetricAlarms
    , darsNextToken
    , darsStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAlarms' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqAlarmNamePrefix'
--
-- * 'darqActionPrefix'
--
-- * 'darqNextToken'
--
-- * 'darqStateValue'
--
-- * 'darqAlarmNames'
--
-- * 'darqMaxRecords'
data DescribeAlarms = DescribeAlarms'
    { _darqAlarmNamePrefix :: !(Maybe Text)
    , _darqActionPrefix    :: !(Maybe Text)
    , _darqNextToken       :: !(Maybe Text)
    , _darqStateValue      :: !(Maybe StateValue)
    , _darqAlarmNames      :: !(Maybe [Text])
    , _darqMaxRecords      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarms' smart constructor.
describeAlarms :: DescribeAlarms
describeAlarms =
    DescribeAlarms'
    { _darqAlarmNamePrefix = Nothing
    , _darqActionPrefix = Nothing
    , _darqNextToken = Nothing
    , _darqStateValue = Nothing
    , _darqAlarmNames = Nothing
    , _darqMaxRecords = Nothing
    }

-- | The alarm name prefix. @AlarmNames@ cannot be specified if this
-- parameter is specified.
darqAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
darqAlarmNamePrefix = lens _darqAlarmNamePrefix (\ s a -> s{_darqAlarmNamePrefix = a});

-- | The action name prefix.
darqActionPrefix :: Lens' DescribeAlarms (Maybe Text)
darqActionPrefix = lens _darqActionPrefix (\ s a -> s{_darqActionPrefix = a});

-- | The token returned by a previous call to indicate that there is more
-- data available.
darqNextToken :: Lens' DescribeAlarms (Maybe Text)
darqNextToken = lens _darqNextToken (\ s a -> s{_darqNextToken = a});

-- | The state value to be used in matching alarms.
darqStateValue :: Lens' DescribeAlarms (Maybe StateValue)
darqStateValue = lens _darqStateValue (\ s a -> s{_darqStateValue = a});

-- | A list of alarm names to retrieve information for.
darqAlarmNames :: Lens' DescribeAlarms [Text]
darqAlarmNames = lens _darqAlarmNames (\ s a -> s{_darqAlarmNames = a}) . _Default;

-- | The maximum number of alarm descriptions to retrieve.
darqMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
darqMaxRecords = lens _darqMaxRecords (\ s a -> s{_darqMaxRecords = a}) . mapping _Nat;

instance AWSPager DescribeAlarms where
        page rq rs
          | stop (rs ^. darsNextToken) = Nothing
          | stop (rs ^. darsMetricAlarms) = Nothing
          | otherwise =
            Just $ rq & darqNextToken .~ rs ^. darsNextToken

instance AWSRequest DescribeAlarms where
        type Sv DescribeAlarms = CloudWatch
        type Rs DescribeAlarms = DescribeAlarmsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAlarmsResult"
              (\ s h x ->
                 DescribeAlarmsResponse' <$>
                   (x .@? "MetricAlarms" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAlarms where
        toHeaders = const mempty

instance ToPath DescribeAlarms where
        toPath = const "/"

instance ToQuery DescribeAlarms where
        toQuery DescribeAlarms'{..}
          = mconcat
              ["Action" =: ("DescribeAlarms" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNamePrefix" =: _darqAlarmNamePrefix,
               "ActionPrefix" =: _darqActionPrefix,
               "NextToken" =: _darqNextToken,
               "StateValue" =: _darqStateValue,
               "AlarmNames" =:
                 toQuery (toQueryList "member" <$> _darqAlarmNames),
               "MaxRecords" =: _darqMaxRecords]

-- | The output for the DescribeAlarms action.
--
-- /See:/ 'describeAlarmsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darsMetricAlarms'
--
-- * 'darsNextToken'
--
-- * 'darsStatus'
data DescribeAlarmsResponse = DescribeAlarmsResponse'
    { _darsMetricAlarms :: !(Maybe [MetricAlarm])
    , _darsNextToken    :: !(Maybe Text)
    , _darsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmsResponse' smart constructor.
describeAlarmsResponse :: Int -> DescribeAlarmsResponse
describeAlarmsResponse pStatus_ =
    DescribeAlarmsResponse'
    { _darsMetricAlarms = Nothing
    , _darsNextToken = Nothing
    , _darsStatus = pStatus_
    }

-- | A list of information for the specified alarms.
darsMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darsMetricAlarms = lens _darsMetricAlarms (\ s a -> s{_darsMetricAlarms = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
darsNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\ s a -> s{_darsNextToken = a});

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeAlarmsResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});

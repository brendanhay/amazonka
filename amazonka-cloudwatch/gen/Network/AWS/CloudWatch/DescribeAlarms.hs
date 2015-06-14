{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarms
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

-- | Retrieves alarms with the specified names. If no name is specified, all
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
    , daNextToken
    , daStateValue
    , daAlarmNames
    , daAlarmNamePrefix
    , daActionPrefix
    , daMaxRecords

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response constructor
    , describeAlarmsResponse
    -- ** Response lenses
    , darMetricAlarms
    , darNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatch.Types

-- | /See:/ 'describeAlarms' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daNextToken'
--
-- * 'daStateValue'
--
-- * 'daAlarmNames'
--
-- * 'daAlarmNamePrefix'
--
-- * 'daActionPrefix'
--
-- * 'daMaxRecords'
data DescribeAlarms = DescribeAlarms'{_daNextToken :: Maybe Text, _daStateValue :: Maybe StateValue, _daAlarmNames :: [Text], _daAlarmNamePrefix :: Text, _daActionPrefix :: Text, _daMaxRecords :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeAlarms' smart constructor.
describeAlarms :: Text -> Text -> Natural -> DescribeAlarms
describeAlarms pAlarmNamePrefix pActionPrefix pMaxRecords = DescribeAlarms'{_daNextToken = Nothing, _daStateValue = Nothing, _daAlarmNames = mempty, _daAlarmNamePrefix = pAlarmNamePrefix, _daActionPrefix = pActionPrefix, _daMaxRecords = _Nat # pMaxRecords};

-- | The token returned by a previous call to indicate that there is more
-- data available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a});

-- | The state value to be used in matching alarms.
daStateValue :: Lens' DescribeAlarms (Maybe StateValue)
daStateValue = lens _daStateValue (\ s a -> s{_daStateValue = a});

-- | A list of alarm names to retrieve information for.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\ s a -> s{_daAlarmNames = a});

-- | The alarm name prefix. @AlarmNames@ cannot be specified if this
-- parameter is specified.
daAlarmNamePrefix :: Lens' DescribeAlarms Text
daAlarmNamePrefix = lens _daAlarmNamePrefix (\ s a -> s{_daAlarmNamePrefix = a});

-- | The action name prefix.
daActionPrefix :: Lens' DescribeAlarms Text
daActionPrefix = lens _daActionPrefix (\ s a -> s{_daActionPrefix = a});

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms Natural
daMaxRecords = lens _daMaxRecords (\ s a -> s{_daMaxRecords = a}) . _Nat;

instance AWSRequest DescribeAlarms where
        type Sv DescribeAlarms = CloudWatch
        type Rs DescribeAlarms = DescribeAlarmsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAlarmsResult"
              (\ s h x ->
                 DescribeAlarmsResponse' <$>
                   (x .@? "MetricAlarms" .!@ mempty >>=
                      parseXMLList "member")
                     <*> x .@? "NextToken")

instance ToHeaders DescribeAlarms where
        toHeaders = const mempty

instance ToPath DescribeAlarms where
        toPath = const "/"

instance ToQuery DescribeAlarms where
        toQuery DescribeAlarms'{..}
          = mconcat
              ["Action" =: ("DescribeAlarms" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "NextToken" =: _daNextToken,
               "StateValue" =: _daStateValue,
               "AlarmNames" =: "member" =: _daAlarmNames,
               "AlarmNamePrefix" =: _daAlarmNamePrefix,
               "ActionPrefix" =: _daActionPrefix,
               "MaxRecords" =: _daMaxRecords]

-- | /See:/ 'describeAlarmsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darMetricAlarms'
--
-- * 'darNextToken'
data DescribeAlarmsResponse = DescribeAlarmsResponse'{_darMetricAlarms :: [MetricAlarm], _darNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeAlarmsResponse' smart constructor.
describeAlarmsResponse :: DescribeAlarmsResponse
describeAlarmsResponse = DescribeAlarmsResponse'{_darMetricAlarms = mempty, _darNextToken = Nothing};

-- | A list of information for the specified alarms.
darMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darMetricAlarms = lens _darMetricAlarms (\ s a -> s{_darMetricAlarms = a});

-- | A string that marks the start of the next batch of returned results.
darNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darNextToken = lens _darNextToken (\ s a -> s{_darNextToken = a});

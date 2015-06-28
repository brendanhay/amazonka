{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , daAlarmNamePrefix
    , daActionPrefix
    , daNextToken
    , daStateValue
    , daAlarmNames
    , daMaxRecords

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response constructor
    , describeAlarmsResponse
    -- ** Response lenses
    , darMetricAlarms
    , darNextToken
    , darStatus
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
-- * 'daAlarmNamePrefix'
--
-- * 'daActionPrefix'
--
-- * 'daNextToken'
--
-- * 'daStateValue'
--
-- * 'daAlarmNames'
--
-- * 'daMaxRecords'
data DescribeAlarms = DescribeAlarms'
    { _daAlarmNamePrefix :: !(Maybe Text)
    , _daActionPrefix    :: !(Maybe Text)
    , _daNextToken       :: !(Maybe Text)
    , _daStateValue      :: !(Maybe StateValue)
    , _daAlarmNames      :: !(Maybe [Text])
    , _daMaxRecords      :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'DescribeAlarms' smart constructor.
describeAlarms :: DescribeAlarms
describeAlarms =
    DescribeAlarms'
    { _daAlarmNamePrefix = Nothing
    , _daActionPrefix = Nothing
    , _daNextToken = Nothing
    , _daStateValue = Nothing
    , _daAlarmNames = Nothing
    , _daMaxRecords = Nothing
    }

-- | The alarm name prefix. @AlarmNames@ cannot be specified if this
-- parameter is specified.
daAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
daAlarmNamePrefix = lens _daAlarmNamePrefix (\ s a -> s{_daAlarmNamePrefix = a});

-- | The action name prefix.
daActionPrefix :: Lens' DescribeAlarms (Maybe Text)
daActionPrefix = lens _daActionPrefix (\ s a -> s{_daActionPrefix = a});

-- | The token returned by a previous call to indicate that there is more
-- data available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a});

-- | The state value to be used in matching alarms.
daStateValue :: Lens' DescribeAlarms (Maybe StateValue)
daStateValue = lens _daStateValue (\ s a -> s{_daStateValue = a});

-- | A list of alarm names to retrieve information for.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\ s a -> s{_daAlarmNames = a}) . _Default;

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\ s a -> s{_daMaxRecords = a}) . mapping _Nat;

instance AWSPager DescribeAlarms where
        page rq rs
          | stop (rs ^. darNextToken) = Nothing
          | stop (rs ^. darMetricAlarms) = Nothing
          | otherwise =
            Just $ rq & daNextToken .~ rs ^. darNextToken

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
                     <*> (pure s))

instance ToHeaders DescribeAlarms where
        toHeaders = const mempty

instance ToPath DescribeAlarms where
        toPath = const "/"

instance ToQuery DescribeAlarms where
        toQuery DescribeAlarms'{..}
          = mconcat
              ["Action" =: ("DescribeAlarms" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNamePrefix" =: _daAlarmNamePrefix,
               "ActionPrefix" =: _daActionPrefix,
               "NextToken" =: _daNextToken,
               "StateValue" =: _daStateValue,
               "AlarmNames" =:
                 toQuery (toQueryList "member" <$> _daAlarmNames),
               "MaxRecords" =: _daMaxRecords]

-- | The output for the DescribeAlarms action.
--
-- /See:/ 'describeAlarmsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darMetricAlarms'
--
-- * 'darNextToken'
--
-- * 'darStatus'
data DescribeAlarmsResponse = DescribeAlarmsResponse'
    { _darMetricAlarms :: !(Maybe [MetricAlarm])
    , _darNextToken    :: !(Maybe Text)
    , _darStatus       :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeAlarmsResponse' smart constructor.
describeAlarmsResponse :: Status -> DescribeAlarmsResponse
describeAlarmsResponse pStatus =
    DescribeAlarmsResponse'
    { _darMetricAlarms = Nothing
    , _darNextToken = Nothing
    , _darStatus = pStatus
    }

-- | A list of information for the specified alarms.
darMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darMetricAlarms = lens _darMetricAlarms (\ s a -> s{_darMetricAlarms = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
darNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darNextToken = lens _darNextToken (\ s a -> s{_darNextToken = a});

-- | FIXME: Undocumented member.
darStatus :: Lens' DescribeAlarmsResponse Status
darStatus = lens _darStatus (\ s a -> s{_darStatus = a});

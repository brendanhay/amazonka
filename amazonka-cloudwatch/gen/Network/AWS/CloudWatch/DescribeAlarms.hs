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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
daAlarmNames = lens _daAlarmNames (\ s a -> s{_daAlarmNames = a}) . _Default . _Coerce;

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\ s a -> s{_daMaxRecords = a}) . mapping _Nat;

instance AWSPager DescribeAlarms where
        page rq rs
          | stop (rs ^. darsNextToken) = Nothing
          | stop (rs ^. darsMetricAlarms) = Nothing
          | otherwise =
            Just $ rq & daNextToken .~ rs ^. darsNextToken

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
darsMetricAlarms = lens _darsMetricAlarms (\ s a -> s{_darsMetricAlarms = a}) . _Default . _Coerce;

-- | A string that marks the start of the next batch of returned results.
darsNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\ s a -> s{_darsNextToken = a});

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeAlarmsResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});

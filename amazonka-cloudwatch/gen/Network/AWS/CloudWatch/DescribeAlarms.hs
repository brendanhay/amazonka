{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified alarms. If no alarms are specified, all alarms are returned. Alarms can be retrieved by using only a prefix for the alarm name, the alarm state, or a prefix for any action.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarms
    (
    -- * Creating a Request
      describeAlarms
    , DescribeAlarms
    -- * Request Lenses
    , daAlarmNamePrefix
    , daActionPrefix
    , daNextToken
    , daStateValue
    , daAlarmNames
    , daMaxRecords

    -- * Destructuring the Response
    , describeAlarmsResponse
    , DescribeAlarmsResponse
    -- * Response Lenses
    , darsMetricAlarms
    , darsNextToken
    , darsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAlarms' smart constructor.
data DescribeAlarms = DescribeAlarms'
  { _daAlarmNamePrefix :: !(Maybe Text)
  , _daActionPrefix    :: !(Maybe Text)
  , _daNextToken       :: !(Maybe Text)
  , _daStateValue      :: !(Maybe StateValue)
  , _daAlarmNames      :: !(Maybe [Text])
  , _daMaxRecords      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlarms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAlarmNamePrefix' - The alarm name prefix. If this parameter is specified, you cannot specify @AlarmNames@ .
--
-- * 'daActionPrefix' - The action name prefix.
--
-- * 'daNextToken' - The token returned by a previous call to indicate that there is more data available.
--
-- * 'daStateValue' - The state value to be used in matching alarms.
--
-- * 'daAlarmNames' - The names of the alarms.
--
-- * 'daMaxRecords' - The maximum number of alarm descriptions to retrieve.
describeAlarms
    :: DescribeAlarms
describeAlarms =
  DescribeAlarms'
    { _daAlarmNamePrefix = Nothing
    , _daActionPrefix = Nothing
    , _daNextToken = Nothing
    , _daStateValue = Nothing
    , _daAlarmNames = Nothing
    , _daMaxRecords = Nothing
    }


-- | The alarm name prefix. If this parameter is specified, you cannot specify @AlarmNames@ .
daAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
daAlarmNamePrefix = lens _daAlarmNamePrefix (\ s a -> s{_daAlarmNamePrefix = a})

-- | The action name prefix.
daActionPrefix :: Lens' DescribeAlarms (Maybe Text)
daActionPrefix = lens _daActionPrefix (\ s a -> s{_daActionPrefix = a})

-- | The token returned by a previous call to indicate that there is more data available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a})

-- | The state value to be used in matching alarms.
daStateValue :: Lens' DescribeAlarms (Maybe StateValue)
daStateValue = lens _daStateValue (\ s a -> s{_daStateValue = a})

-- | The names of the alarms.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\ s a -> s{_daAlarmNames = a}) . _Default . _Coerce

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\ s a -> s{_daMaxRecords = a}) . mapping _Nat

instance AWSPager DescribeAlarms where
        page rq rs
          | stop (rs ^. darsNextToken) = Nothing
          | stop (rs ^. darsMetricAlarms) = Nothing
          | otherwise =
            Just $ rq & daNextToken .~ rs ^. darsNextToken

instance AWSRequest DescribeAlarms where
        type Rs DescribeAlarms = DescribeAlarmsResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "DescribeAlarmsResult"
              (\ s h x ->
                 DescribeAlarmsResponse' <$>
                   (x .@? "MetricAlarms" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAlarms where

instance NFData DescribeAlarms where

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

-- | /See:/ 'describeAlarmsResponse' smart constructor.
data DescribeAlarmsResponse = DescribeAlarmsResponse'
  { _darsMetricAlarms   :: !(Maybe [MetricAlarm])
  , _darsNextToken      :: !(Maybe Text)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlarmsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsMetricAlarms' - The information for the specified alarms.
--
-- * 'darsNextToken' - The token that marks the start of the next batch of returned results.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAlarmsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAlarmsResponse
describeAlarmsResponse pResponseStatus_ =
  DescribeAlarmsResponse'
    { _darsMetricAlarms = Nothing
    , _darsNextToken = Nothing
    , _darsResponseStatus = pResponseStatus_
    }


-- | The information for the specified alarms.
darsMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darsMetricAlarms = lens _darsMetricAlarms (\ s a -> s{_darsMetricAlarms = a}) . _Default . _Coerce

-- | The token that marks the start of the next batch of returned results.
darsNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\ s a -> s{_darsNextToken = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAlarmsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAlarmsResponse where

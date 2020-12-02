{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified alarms. You can filter the results by specifying a a prefix for the alarm name, the alarm state, or a prefix for any action.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarms
  ( -- * Creating a Request
    describeAlarms,
    DescribeAlarms,

    -- * Request Lenses
    daAlarmNamePrefix,
    daAlarmTypes,
    daActionPrefix,
    daNextToken,
    daStateValue,
    daAlarmNames,
    daMaxRecords,
    daParentsOfAlarmName,
    daChildrenOfAlarmName,

    -- * Destructuring the Response
    describeAlarmsResponse,
    DescribeAlarmsResponse,

    -- * Response Lenses
    darsMetricAlarms,
    darsCompositeAlarms,
    darsNextToken,
    darsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAlarms' smart constructor.
data DescribeAlarms = DescribeAlarms'
  { _daAlarmNamePrefix ::
      !(Maybe Text),
    _daAlarmTypes :: !(Maybe [AlarmType]),
    _daActionPrefix :: !(Maybe Text),
    _daNextToken :: !(Maybe Text),
    _daStateValue :: !(Maybe StateValue),
    _daAlarmNames :: !(Maybe [Text]),
    _daMaxRecords :: !(Maybe Nat),
    _daParentsOfAlarmName :: !(Maybe Text),
    _daChildrenOfAlarmName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlarms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAlarmNamePrefix' - An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix. If this parameter is specified, you cannot specify @AlarmNames@ .
--
-- * 'daAlarmTypes' - Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- * 'daActionPrefix' - Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
--
-- * 'daNextToken' - The token returned by a previous call to indicate that there is more data available.
--
-- * 'daStateValue' - Specify this parameter to receive information only about alarms that are currently in the state that you specify.
--
-- * 'daAlarmNames' - The names of the alarms to retrieve information about.
--
-- * 'daMaxRecords' - The maximum number of alarm descriptions to retrieve.
--
-- * 'daParentsOfAlarmName' - If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned. If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- * 'daChildrenOfAlarmName' - If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned. If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
describeAlarms ::
  DescribeAlarms
describeAlarms =
  DescribeAlarms'
    { _daAlarmNamePrefix = Nothing,
      _daAlarmTypes = Nothing,
      _daActionPrefix = Nothing,
      _daNextToken = Nothing,
      _daStateValue = Nothing,
      _daAlarmNames = Nothing,
      _daMaxRecords = Nothing,
      _daParentsOfAlarmName = Nothing,
      _daChildrenOfAlarmName = Nothing
    }

-- | An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix. If this parameter is specified, you cannot specify @AlarmNames@ .
daAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
daAlarmNamePrefix = lens _daAlarmNamePrefix (\s a -> s {_daAlarmNamePrefix = a})

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
daAlarmTypes :: Lens' DescribeAlarms [AlarmType]
daAlarmTypes = lens _daAlarmTypes (\s a -> s {_daAlarmTypes = a}) . _Default . _Coerce

-- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
daActionPrefix :: Lens' DescribeAlarms (Maybe Text)
daActionPrefix = lens _daActionPrefix (\s a -> s {_daActionPrefix = a})

-- | The token returned by a previous call to indicate that there is more data available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\s a -> s {_daNextToken = a})

-- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
daStateValue :: Lens' DescribeAlarms (Maybe StateValue)
daStateValue = lens _daStateValue (\s a -> s {_daStateValue = a})

-- | The names of the alarms to retrieve information about.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\s a -> s {_daAlarmNames = a}) . _Default . _Coerce

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\s a -> s {_daMaxRecords = a}) . mapping _Nat

-- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned. If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
daParentsOfAlarmName :: Lens' DescribeAlarms (Maybe Text)
daParentsOfAlarmName = lens _daParentsOfAlarmName (\s a -> s {_daParentsOfAlarmName = a})

-- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned. If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
daChildrenOfAlarmName :: Lens' DescribeAlarms (Maybe Text)
daChildrenOfAlarmName = lens _daChildrenOfAlarmName (\s a -> s {_daChildrenOfAlarmName = a})

instance AWSPager DescribeAlarms where
  page rq rs
    | stop (rs ^. darsNextToken) = Nothing
    | stop (rs ^. darsMetricAlarms) = Nothing
    | stop (rs ^. darsCompositeAlarms) = Nothing
    | otherwise = Just $ rq & daNextToken .~ rs ^. darsNextToken

instance AWSRequest DescribeAlarms where
  type Rs DescribeAlarms = DescribeAlarmsResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "DescribeAlarmsResult"
      ( \s h x ->
          DescribeAlarmsResponse'
            <$> (x .@? "MetricAlarms" .!@ mempty >>= may (parseXMLList "member"))
            <*> ( x .@? "CompositeAlarms" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAlarms

instance NFData DescribeAlarms

instance ToHeaders DescribeAlarms where
  toHeaders = const mempty

instance ToPath DescribeAlarms where
  toPath = const "/"

instance ToQuery DescribeAlarms where
  toQuery DescribeAlarms' {..} =
    mconcat
      [ "Action" =: ("DescribeAlarms" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "AlarmNamePrefix" =: _daAlarmNamePrefix,
        "AlarmTypes" =: toQuery (toQueryList "member" <$> _daAlarmTypes),
        "ActionPrefix" =: _daActionPrefix,
        "NextToken" =: _daNextToken,
        "StateValue" =: _daStateValue,
        "AlarmNames" =: toQuery (toQueryList "member" <$> _daAlarmNames),
        "MaxRecords" =: _daMaxRecords,
        "ParentsOfAlarmName" =: _daParentsOfAlarmName,
        "ChildrenOfAlarmName" =: _daChildrenOfAlarmName
      ]

-- | /See:/ 'describeAlarmsResponse' smart constructor.
data DescribeAlarmsResponse = DescribeAlarmsResponse'
  { _darsMetricAlarms ::
      !(Maybe [MetricAlarm]),
    _darsCompositeAlarms ::
      !(Maybe [CompositeAlarm]),
    _darsNextToken :: !(Maybe Text),
    _darsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlarmsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsMetricAlarms' - The information about any metric alarms returned by the operation.
--
-- * 'darsCompositeAlarms' - The information about any composite alarms returned by the operation.
--
-- * 'darsNextToken' - The token that marks the start of the next batch of returned results.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAlarmsResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DescribeAlarmsResponse
describeAlarmsResponse pResponseStatus_ =
  DescribeAlarmsResponse'
    { _darsMetricAlarms = Nothing,
      _darsCompositeAlarms = Nothing,
      _darsNextToken = Nothing,
      _darsResponseStatus = pResponseStatus_
    }

-- | The information about any metric alarms returned by the operation.
darsMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darsMetricAlarms = lens _darsMetricAlarms (\s a -> s {_darsMetricAlarms = a}) . _Default . _Coerce

-- | The information about any composite alarms returned by the operation.
darsCompositeAlarms :: Lens' DescribeAlarmsResponse [CompositeAlarm]
darsCompositeAlarms = lens _darsCompositeAlarms (\s a -> s {_darsCompositeAlarms = a}) . _Default . _Coerce

-- | The token that marks the start of the next batch of returned results.
darsNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\s a -> s {_darsNextToken = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAlarmsResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DescribeAlarmsResponse

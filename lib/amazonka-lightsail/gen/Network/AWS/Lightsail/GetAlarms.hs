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
-- Module      : Network.AWS.Lightsail.GetAlarms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured alarms. Specify an alarm name in your request to return information about a specific alarm, or specify a monitored resource name to return information about all alarms for a specific resource.
--
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.GetAlarms
  ( -- * Creating a Request
    getAlarms,
    GetAlarms,

    -- * Request Lenses
    gaAlarmName,
    gaMonitoredResourceName,
    gaPageToken,

    -- * Destructuring the Response
    getAlarmsResponse,
    GetAlarmsResponse,

    -- * Response Lenses
    garsNextPageToken,
    garsAlarms,
    garsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAlarms' smart constructor.
data GetAlarms = GetAlarms'
  { _gaAlarmName :: !(Maybe Text),
    _gaMonitoredResourceName :: !(Maybe Text),
    _gaPageToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAlarms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaAlarmName' - The name of the alarm. Specify an alarm name to return information about a specific alarm.
--
-- * 'gaMonitoredResourceName' - The name of the Lightsail resource being monitored by the alarm. Specify a monitored resource name to return information about all alarms for a specific resource.
--
-- * 'gaPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getAlarms ::
  GetAlarms
getAlarms =
  GetAlarms'
    { _gaAlarmName = Nothing,
      _gaMonitoredResourceName = Nothing,
      _gaPageToken = Nothing
    }

-- | The name of the alarm. Specify an alarm name to return information about a specific alarm.
gaAlarmName :: Lens' GetAlarms (Maybe Text)
gaAlarmName = lens _gaAlarmName (\s a -> s {_gaAlarmName = a})

-- | The name of the Lightsail resource being monitored by the alarm. Specify a monitored resource name to return information about all alarms for a specific resource.
gaMonitoredResourceName :: Lens' GetAlarms (Maybe Text)
gaMonitoredResourceName = lens _gaMonitoredResourceName (\s a -> s {_gaMonitoredResourceName = a})

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gaPageToken :: Lens' GetAlarms (Maybe Text)
gaPageToken = lens _gaPageToken (\s a -> s {_gaPageToken = a})

instance AWSRequest GetAlarms where
  type Rs GetAlarms = GetAlarmsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetAlarmsResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "alarms" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetAlarms

instance NFData GetAlarms

instance ToHeaders GetAlarms where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.GetAlarms" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAlarms where
  toJSON GetAlarms' {..} =
    object
      ( catMaybes
          [ ("alarmName" .=) <$> _gaAlarmName,
            ("monitoredResourceName" .=) <$> _gaMonitoredResourceName,
            ("pageToken" .=) <$> _gaPageToken
          ]
      )

instance ToPath GetAlarms where
  toPath = const "/"

instance ToQuery GetAlarms where
  toQuery = const mempty

-- | /See:/ 'getAlarmsResponse' smart constructor.
data GetAlarmsResponse = GetAlarmsResponse'
  { _garsNextPageToken ::
      !(Maybe Text),
    _garsAlarms :: !(Maybe [Alarm]),
    _garsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAlarmsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'garsAlarms' - An array of objects that describe the alarms.
--
-- * 'garsResponseStatus' - -- | The response status code.
getAlarmsResponse ::
  -- | 'garsResponseStatus'
  Int ->
  GetAlarmsResponse
getAlarmsResponse pResponseStatus_ =
  GetAlarmsResponse'
    { _garsNextPageToken = Nothing,
      _garsAlarms = Nothing,
      _garsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
garsNextPageToken :: Lens' GetAlarmsResponse (Maybe Text)
garsNextPageToken = lens _garsNextPageToken (\s a -> s {_garsNextPageToken = a})

-- | An array of objects that describe the alarms.
garsAlarms :: Lens' GetAlarmsResponse [Alarm]
garsAlarms = lens _garsAlarms (\s a -> s {_garsAlarms = a}) . _Default . _Coerce

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAlarmsResponse Int
garsResponseStatus = lens _garsResponseStatus (\s a -> s {_garsResponseStatus = a})

instance NFData GetAlarmsResponse

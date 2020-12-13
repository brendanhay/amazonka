{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.GetAlarms
  ( -- * Creating a request
    GetAlarms (..),
    mkGetAlarms,

    -- ** Request lenses
    gaAlarmName,
    gaMonitoredResourceName,
    gaPageToken,

    -- * Destructuring the response
    GetAlarmsResponse (..),
    mkGetAlarmsResponse,

    -- ** Response lenses
    garsNextPageToken,
    garsAlarms,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAlarms' smart constructor.
data GetAlarms = GetAlarms'
  { -- | The name of the alarm.
    --
    -- Specify an alarm name to return information about a specific alarm.
    alarmName :: Lude.Maybe Lude.Text,
    -- | The name of the Lightsail resource being monitored by the alarm.
    --
    -- Specify a monitored resource name to return information about all alarms for a specific resource.
    monitoredResourceName :: Lude.Maybe Lude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAlarms' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
-- * 'monitoredResourceName' - The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms for a specific resource.
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetAlarms ::
  GetAlarms
mkGetAlarms =
  GetAlarms'
    { alarmName = Lude.Nothing,
      monitoredResourceName = Lude.Nothing,
      pageToken = Lude.Nothing
    }

-- | The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAlarmName :: Lens.Lens' GetAlarms (Lude.Maybe Lude.Text)
gaAlarmName = Lens.lens (alarmName :: GetAlarms -> Lude.Maybe Lude.Text) (\s a -> s {alarmName = a} :: GetAlarms)
{-# DEPRECATED gaAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms for a specific resource.
--
-- /Note:/ Consider using 'monitoredResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMonitoredResourceName :: Lens.Lens' GetAlarms (Lude.Maybe Lude.Text)
gaMonitoredResourceName = Lens.lens (monitoredResourceName :: GetAlarms -> Lude.Maybe Lude.Text) (\s a -> s {monitoredResourceName = a} :: GetAlarms)
{-# DEPRECATED gaMonitoredResourceName "Use generic-lens or generic-optics with 'monitoredResourceName' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPageToken :: Lens.Lens' GetAlarms (Lude.Maybe Lude.Text)
gaPageToken = Lens.lens (pageToken :: GetAlarms -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetAlarms)
{-# DEPRECATED gaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Lude.AWSRequest GetAlarms where
  type Rs GetAlarms = GetAlarmsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAlarmsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "alarms" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAlarms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetAlarms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAlarms where
  toJSON GetAlarms' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("alarmName" Lude..=) Lude.<$> alarmName,
            ("monitoredResourceName" Lude..=) Lude.<$> monitoredResourceName,
            ("pageToken" Lude..=) Lude.<$> pageToken
          ]
      )

instance Lude.ToPath GetAlarms where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAlarms where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAlarmsResponse' smart constructor.
data GetAlarmsResponse = GetAlarmsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe the alarms.
    alarms :: Lude.Maybe [Alarm],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAlarmsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
-- * 'alarms' - An array of objects that describe the alarms.
-- * 'responseStatus' - The response status code.
mkGetAlarmsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAlarmsResponse
mkGetAlarmsResponse pResponseStatus_ =
  GetAlarmsResponse'
    { nextPageToken = Lude.Nothing,
      alarms = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsNextPageToken :: Lens.Lens' GetAlarmsResponse (Lude.Maybe Lude.Text)
garsNextPageToken = Lens.lens (nextPageToken :: GetAlarmsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAlarmsResponse)
{-# DEPRECATED garsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects that describe the alarms.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAlarms :: Lens.Lens' GetAlarmsResponse (Lude.Maybe [Alarm])
garsAlarms = Lens.lens (alarms :: GetAlarmsResponse -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: GetAlarmsResponse)
{-# DEPRECATED garsAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAlarmsResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAlarmsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAlarmsResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

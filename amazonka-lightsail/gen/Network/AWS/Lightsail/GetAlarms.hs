{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetAlarms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured alarms. Specify an alarm name
-- in your request to return information about a specific alarm, or specify
-- a monitored resource name to return information about all alarms for a
-- specific resource.
--
-- An alarm is used to monitor a single metric for one of your resources.
-- When a metric condition is met, the alarm can notify you by email, SMS
-- text message, and a banner displayed on the Amazon Lightsail console.
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
module Network.AWS.Lightsail.GetAlarms
  ( -- * Creating a Request
    GetAlarms (..),
    newGetAlarms,

    -- * Request Lenses
    getAlarms_pageToken,
    getAlarms_alarmName,
    getAlarms_monitoredResourceName,

    -- * Destructuring the Response
    GetAlarmsResponse (..),
    newGetAlarmsResponse,

    -- * Response Lenses
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_alarms,
    getAlarmsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAlarms' smart constructor.
data GetAlarms = GetAlarms'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetAlarms@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text,
    -- | The name of the alarm.
    --
    -- Specify an alarm name to return information about a specific alarm.
    alarmName :: Core.Maybe Core.Text,
    -- | The name of the Lightsail resource being monitored by the alarm.
    --
    -- Specify a monitored resource name to return information about all alarms
    -- for a specific resource.
    monitoredResourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAlarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getAlarms_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
--
-- 'alarmName', 'getAlarms_alarmName' - The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
--
-- 'monitoredResourceName', 'getAlarms_monitoredResourceName' - The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms
-- for a specific resource.
newGetAlarms ::
  GetAlarms
newGetAlarms =
  GetAlarms'
    { pageToken = Core.Nothing,
      alarmName = Core.Nothing,
      monitoredResourceName = Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getAlarms_pageToken :: Lens.Lens' GetAlarms (Core.Maybe Core.Text)
getAlarms_pageToken = Lens.lens (\GetAlarms' {pageToken} -> pageToken) (\s@GetAlarms' {} a -> s {pageToken = a} :: GetAlarms)

-- | The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
getAlarms_alarmName :: Lens.Lens' GetAlarms (Core.Maybe Core.Text)
getAlarms_alarmName = Lens.lens (\GetAlarms' {alarmName} -> alarmName) (\s@GetAlarms' {} a -> s {alarmName = a} :: GetAlarms)

-- | The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms
-- for a specific resource.
getAlarms_monitoredResourceName :: Lens.Lens' GetAlarms (Core.Maybe Core.Text)
getAlarms_monitoredResourceName = Lens.lens (\GetAlarms' {monitoredResourceName} -> monitoredResourceName) (\s@GetAlarms' {} a -> s {monitoredResourceName = a} :: GetAlarms)

instance Core.AWSRequest GetAlarms where
  type AWSResponse GetAlarms = GetAlarmsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAlarmsResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "alarms" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAlarms

instance Core.NFData GetAlarms

instance Core.ToHeaders GetAlarms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.GetAlarms" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAlarms where
  toJSON GetAlarms' {..} =
    Core.object
      ( Core.catMaybes
          [ ("pageToken" Core..=) Core.<$> pageToken,
            ("alarmName" Core..=) Core.<$> alarmName,
            ("monitoredResourceName" Core..=)
              Core.<$> monitoredResourceName
          ]
      )

instance Core.ToPath GetAlarms where
  toPath = Core.const "/"

instance Core.ToQuery GetAlarms where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAlarmsResponse' smart constructor.
data GetAlarmsResponse = GetAlarmsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetAlarms@ request and
    -- specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of objects that describe the alarms.
    alarms :: Core.Maybe [Alarm],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAlarmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAlarmsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetAlarms@ request and
-- specify the next page token using the @pageToken@ parameter.
--
-- 'alarms', 'getAlarmsResponse_alarms' - An array of objects that describe the alarms.
--
-- 'httpStatus', 'getAlarmsResponse_httpStatus' - The response's http status code.
newGetAlarmsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAlarmsResponse
newGetAlarmsResponse pHttpStatus_ =
  GetAlarmsResponse'
    { nextPageToken = Core.Nothing,
      alarms = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetAlarms@ request and
-- specify the next page token using the @pageToken@ parameter.
getAlarmsResponse_nextPageToken :: Lens.Lens' GetAlarmsResponse (Core.Maybe Core.Text)
getAlarmsResponse_nextPageToken = Lens.lens (\GetAlarmsResponse' {nextPageToken} -> nextPageToken) (\s@GetAlarmsResponse' {} a -> s {nextPageToken = a} :: GetAlarmsResponse)

-- | An array of objects that describe the alarms.
getAlarmsResponse_alarms :: Lens.Lens' GetAlarmsResponse (Core.Maybe [Alarm])
getAlarmsResponse_alarms = Lens.lens (\GetAlarmsResponse' {alarms} -> alarms) (\s@GetAlarmsResponse' {} a -> s {alarms = a} :: GetAlarmsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAlarmsResponse_httpStatus :: Lens.Lens' GetAlarmsResponse Core.Int
getAlarmsResponse_httpStatus = Lens.lens (\GetAlarmsResponse' {httpStatus} -> httpStatus) (\s@GetAlarmsResponse' {} a -> s {httpStatus = a} :: GetAlarmsResponse)

instance Core.NFData GetAlarmsResponse

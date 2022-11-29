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
-- Module      : Amazonka.Lightsail.GetAlarms
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lightsail.GetAlarms
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
    getAlarmsResponse_alarms,
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAlarms' smart constructor.
data GetAlarms = GetAlarms'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetAlarms@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm.
    --
    -- Specify an alarm name to return information about a specific alarm.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lightsail resource being monitored by the alarm.
    --
    -- Specify a monitored resource name to return information about all alarms
    -- for a specific resource.
    monitoredResourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { pageToken = Prelude.Nothing,
      alarmName = Prelude.Nothing,
      monitoredResourceName = Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getAlarms_pageToken :: Lens.Lens' GetAlarms (Prelude.Maybe Prelude.Text)
getAlarms_pageToken = Lens.lens (\GetAlarms' {pageToken} -> pageToken) (\s@GetAlarms' {} a -> s {pageToken = a} :: GetAlarms)

-- | The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
getAlarms_alarmName :: Lens.Lens' GetAlarms (Prelude.Maybe Prelude.Text)
getAlarms_alarmName = Lens.lens (\GetAlarms' {alarmName} -> alarmName) (\s@GetAlarms' {} a -> s {alarmName = a} :: GetAlarms)

-- | The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms
-- for a specific resource.
getAlarms_monitoredResourceName :: Lens.Lens' GetAlarms (Prelude.Maybe Prelude.Text)
getAlarms_monitoredResourceName = Lens.lens (\GetAlarms' {monitoredResourceName} -> monitoredResourceName) (\s@GetAlarms' {} a -> s {monitoredResourceName = a} :: GetAlarms)

instance Core.AWSRequest GetAlarms where
  type AWSResponse GetAlarms = GetAlarmsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAlarmsResponse'
            Prelude.<$> (x Core..?> "alarms" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAlarms where
  hashWithSalt _salt GetAlarms' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` monitoredResourceName

instance Prelude.NFData GetAlarms where
  rnf GetAlarms' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf monitoredResourceName

instance Core.ToHeaders GetAlarms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetAlarms" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAlarms where
  toJSON GetAlarms' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pageToken" Core..=) Prelude.<$> pageToken,
            ("alarmName" Core..=) Prelude.<$> alarmName,
            ("monitoredResourceName" Core..=)
              Prelude.<$> monitoredResourceName
          ]
      )

instance Core.ToPath GetAlarms where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAlarms where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAlarmsResponse' smart constructor.
data GetAlarmsResponse = GetAlarmsResponse'
  { -- | An array of objects that describe the alarms.
    alarms :: Prelude.Maybe [Alarm],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetAlarms@ request and
    -- specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAlarmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarms', 'getAlarmsResponse_alarms' - An array of objects that describe the alarms.
--
-- 'nextPageToken', 'getAlarmsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetAlarms@ request and
-- specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getAlarmsResponse_httpStatus' - The response's http status code.
newGetAlarmsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAlarmsResponse
newGetAlarmsResponse pHttpStatus_ =
  GetAlarmsResponse'
    { alarms = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the alarms.
getAlarmsResponse_alarms :: Lens.Lens' GetAlarmsResponse (Prelude.Maybe [Alarm])
getAlarmsResponse_alarms = Lens.lens (\GetAlarmsResponse' {alarms} -> alarms) (\s@GetAlarmsResponse' {} a -> s {alarms = a} :: GetAlarmsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetAlarms@ request and
-- specify the next page token using the @pageToken@ parameter.
getAlarmsResponse_nextPageToken :: Lens.Lens' GetAlarmsResponse (Prelude.Maybe Prelude.Text)
getAlarmsResponse_nextPageToken = Lens.lens (\GetAlarmsResponse' {nextPageToken} -> nextPageToken) (\s@GetAlarmsResponse' {} a -> s {nextPageToken = a} :: GetAlarmsResponse)

-- | The response's http status code.
getAlarmsResponse_httpStatus :: Lens.Lens' GetAlarmsResponse Prelude.Int
getAlarmsResponse_httpStatus = Lens.lens (\GetAlarmsResponse' {httpStatus} -> httpStatus) (\s@GetAlarmsResponse' {} a -> s {httpStatus = a} :: GetAlarmsResponse)

instance Prelude.NFData GetAlarmsResponse where
  rnf GetAlarmsResponse' {..} =
    Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus

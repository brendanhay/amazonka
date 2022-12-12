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
-- Module      : Amazonka.IoT1ClickDevices.ListDeviceEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using a device ID, returns a DeviceEventsResponse object containing an
-- array of events for the device.
--
-- This operation returns paginated results.
module Amazonka.IoT1ClickDevices.ListDeviceEvents
  ( -- * Creating a Request
    ListDeviceEvents (..),
    newListDeviceEvents,

    -- * Request Lenses
    listDeviceEvents_maxResults,
    listDeviceEvents_nextToken,
    listDeviceEvents_deviceId,
    listDeviceEvents_fromTimeStamp,
    listDeviceEvents_toTimeStamp,

    -- * Destructuring the Response
    ListDeviceEventsResponse (..),
    newListDeviceEventsResponse,

    -- * Response Lenses
    listDeviceEventsResponse_events,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickDevices.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { -- | The maximum number of results to return per request. If not set, a
    -- default value of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the device.
    deviceId :: Prelude.Text,
    -- | The start date for the device event query, in ISO8061 format. For
    -- example, 2018-03-28T15:45:12.880Z
    fromTimeStamp :: Data.POSIX,
    -- | The end date for the device event query, in ISO8061 format. For example,
    -- 2018-03-28T15:45:12.880Z
    toTimeStamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeviceEvents_maxResults' - The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
--
-- 'nextToken', 'listDeviceEvents_nextToken' - The token to retrieve the next set of results.
--
-- 'deviceId', 'listDeviceEvents_deviceId' - The unique identifier of the device.
--
-- 'fromTimeStamp', 'listDeviceEvents_fromTimeStamp' - The start date for the device event query, in ISO8061 format. For
-- example, 2018-03-28T15:45:12.880Z
--
-- 'toTimeStamp', 'listDeviceEvents_toTimeStamp' - The end date for the device event query, in ISO8061 format. For example,
-- 2018-03-28T15:45:12.880Z
newListDeviceEvents ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'fromTimeStamp'
  Prelude.UTCTime ->
  -- | 'toTimeStamp'
  Prelude.UTCTime ->
  ListDeviceEvents
newListDeviceEvents
  pDeviceId_
  pFromTimeStamp_
  pToTimeStamp_ =
    ListDeviceEvents'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        deviceId = pDeviceId_,
        fromTimeStamp = Data._Time Lens.# pFromTimeStamp_,
        toTimeStamp = Data._Time Lens.# pToTimeStamp_
      }

-- | The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
listDeviceEvents_maxResults :: Lens.Lens' ListDeviceEvents (Prelude.Maybe Prelude.Natural)
listDeviceEvents_maxResults = Lens.lens (\ListDeviceEvents' {maxResults} -> maxResults) (\s@ListDeviceEvents' {} a -> s {maxResults = a} :: ListDeviceEvents)

-- | The token to retrieve the next set of results.
listDeviceEvents_nextToken :: Lens.Lens' ListDeviceEvents (Prelude.Maybe Prelude.Text)
listDeviceEvents_nextToken = Lens.lens (\ListDeviceEvents' {nextToken} -> nextToken) (\s@ListDeviceEvents' {} a -> s {nextToken = a} :: ListDeviceEvents)

-- | The unique identifier of the device.
listDeviceEvents_deviceId :: Lens.Lens' ListDeviceEvents Prelude.Text
listDeviceEvents_deviceId = Lens.lens (\ListDeviceEvents' {deviceId} -> deviceId) (\s@ListDeviceEvents' {} a -> s {deviceId = a} :: ListDeviceEvents)

-- | The start date for the device event query, in ISO8061 format. For
-- example, 2018-03-28T15:45:12.880Z
listDeviceEvents_fromTimeStamp :: Lens.Lens' ListDeviceEvents Prelude.UTCTime
listDeviceEvents_fromTimeStamp = Lens.lens (\ListDeviceEvents' {fromTimeStamp} -> fromTimeStamp) (\s@ListDeviceEvents' {} a -> s {fromTimeStamp = a} :: ListDeviceEvents) Prelude.. Data._Time

-- | The end date for the device event query, in ISO8061 format. For example,
-- 2018-03-28T15:45:12.880Z
listDeviceEvents_toTimeStamp :: Lens.Lens' ListDeviceEvents Prelude.UTCTime
listDeviceEvents_toTimeStamp = Lens.lens (\ListDeviceEvents' {toTimeStamp} -> toTimeStamp) (\s@ListDeviceEvents' {} a -> s {toTimeStamp = a} :: ListDeviceEvents) Prelude.. Data._Time

instance Core.AWSPager ListDeviceEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceEventsResponse_events Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeviceEvents_nextToken
          Lens..~ rs
          Lens.^? listDeviceEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceEvents where
  type
    AWSResponse ListDeviceEvents =
      ListDeviceEventsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceEventsResponse'
            Prelude.<$> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceEvents where
  hashWithSalt _salt ListDeviceEvents' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` fromTimeStamp
      `Prelude.hashWithSalt` toTimeStamp

instance Prelude.NFData ListDeviceEvents where
  rnf ListDeviceEvents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf fromTimeStamp
      `Prelude.seq` Prelude.rnf toTimeStamp

instance Data.ToHeaders ListDeviceEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeviceEvents where
  toPath ListDeviceEvents' {..} =
    Prelude.mconcat
      ["/devices/", Data.toBS deviceId, "/events"]

instance Data.ToQuery ListDeviceEvents where
  toQuery ListDeviceEvents' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "fromTimeStamp" Data.=: fromTimeStamp,
        "toTimeStamp" Data.=: toTimeStamp
      ]

-- | /See:/ 'newListDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { -- | An array of zero or more elements describing the event(s) associated
    -- with the device.
    events :: Prelude.Maybe [DeviceEvent],
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'listDeviceEventsResponse_events' - An array of zero or more elements describing the event(s) associated
-- with the device.
--
-- 'nextToken', 'listDeviceEventsResponse_nextToken' - The token to retrieve the next set of results.
--
-- 'httpStatus', 'listDeviceEventsResponse_httpStatus' - The response's http status code.
newListDeviceEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceEventsResponse
newListDeviceEventsResponse pHttpStatus_ =
  ListDeviceEventsResponse'
    { events = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of zero or more elements describing the event(s) associated
-- with the device.
listDeviceEventsResponse_events :: Lens.Lens' ListDeviceEventsResponse (Prelude.Maybe [DeviceEvent])
listDeviceEventsResponse_events = Lens.lens (\ListDeviceEventsResponse' {events} -> events) (\s@ListDeviceEventsResponse' {} a -> s {events = a} :: ListDeviceEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results.
listDeviceEventsResponse_nextToken :: Lens.Lens' ListDeviceEventsResponse (Prelude.Maybe Prelude.Text)
listDeviceEventsResponse_nextToken = Lens.lens (\ListDeviceEventsResponse' {nextToken} -> nextToken) (\s@ListDeviceEventsResponse' {} a -> s {nextToken = a} :: ListDeviceEventsResponse)

-- | The response's http status code.
listDeviceEventsResponse_httpStatus :: Lens.Lens' ListDeviceEventsResponse Prelude.Int
listDeviceEventsResponse_httpStatus = Lens.lens (\ListDeviceEventsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceEventsResponse' {} a -> s {httpStatus = a} :: ListDeviceEventsResponse)

instance Prelude.NFData ListDeviceEventsResponse where
  rnf ListDeviceEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

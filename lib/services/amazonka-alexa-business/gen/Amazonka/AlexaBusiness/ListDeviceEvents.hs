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
-- Module      : Amazonka.AlexaBusiness.ListDeviceEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the device event history, including device connection status, for
-- up to 30 days.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListDeviceEvents
  ( -- * Creating a Request
    ListDeviceEvents (..),
    newListDeviceEvents,

    -- * Request Lenses
    listDeviceEvents_eventType,
    listDeviceEvents_maxResults,
    listDeviceEvents_nextToken,
    listDeviceEvents_deviceArn,

    -- * Destructuring the Response
    ListDeviceEventsResponse (..),
    newListDeviceEventsResponse,

    -- * Response Lenses
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { -- | The event type to filter device events. If EventType isn\'t specified,
    -- this returns a list of all device events in reverse chronological order.
    -- If EventType is specified, this returns a list of device events for that
    -- EventType in reverse chronological order.
    eventType :: Prelude.Maybe DeviceEventType,
    -- | The maximum number of results to include in the response. The default
    -- value is 50. If more results exist than the specified MaxResults value,
    -- a token is included in the response so that the remaining results can be
    -- retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults. When the end of results is reached, the
    -- response has a value of null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a device.
    deviceArn :: Prelude.Text
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
-- 'eventType', 'listDeviceEvents_eventType' - The event type to filter device events. If EventType isn\'t specified,
-- this returns a list of all device events in reverse chronological order.
-- If EventType is specified, this returns a list of device events for that
-- EventType in reverse chronological order.
--
-- 'maxResults', 'listDeviceEvents_maxResults' - The maximum number of results to include in the response. The default
-- value is 50. If more results exist than the specified MaxResults value,
-- a token is included in the response so that the remaining results can be
-- retrieved.
--
-- 'nextToken', 'listDeviceEvents_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults. When the end of results is reached, the
-- response has a value of null.
--
-- 'deviceArn', 'listDeviceEvents_deviceArn' - The ARN of a device.
newListDeviceEvents ::
  -- | 'deviceArn'
  Prelude.Text ->
  ListDeviceEvents
newListDeviceEvents pDeviceArn_ =
  ListDeviceEvents'
    { eventType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      deviceArn = pDeviceArn_
    }

-- | The event type to filter device events. If EventType isn\'t specified,
-- this returns a list of all device events in reverse chronological order.
-- If EventType is specified, this returns a list of device events for that
-- EventType in reverse chronological order.
listDeviceEvents_eventType :: Lens.Lens' ListDeviceEvents (Prelude.Maybe DeviceEventType)
listDeviceEvents_eventType = Lens.lens (\ListDeviceEvents' {eventType} -> eventType) (\s@ListDeviceEvents' {} a -> s {eventType = a} :: ListDeviceEvents)

-- | The maximum number of results to include in the response. The default
-- value is 50. If more results exist than the specified MaxResults value,
-- a token is included in the response so that the remaining results can be
-- retrieved.
listDeviceEvents_maxResults :: Lens.Lens' ListDeviceEvents (Prelude.Maybe Prelude.Natural)
listDeviceEvents_maxResults = Lens.lens (\ListDeviceEvents' {maxResults} -> maxResults) (\s@ListDeviceEvents' {} a -> s {maxResults = a} :: ListDeviceEvents)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults. When the end of results is reached, the
-- response has a value of null.
listDeviceEvents_nextToken :: Lens.Lens' ListDeviceEvents (Prelude.Maybe Prelude.Text)
listDeviceEvents_nextToken = Lens.lens (\ListDeviceEvents' {nextToken} -> nextToken) (\s@ListDeviceEvents' {} a -> s {nextToken = a} :: ListDeviceEvents)

-- | The ARN of a device.
listDeviceEvents_deviceArn :: Lens.Lens' ListDeviceEvents Prelude.Text
listDeviceEvents_deviceArn = Lens.lens (\ListDeviceEvents' {deviceArn} -> deviceArn) (\s@ListDeviceEvents' {} a -> s {deviceArn = a} :: ListDeviceEvents)

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
            Lens.^? listDeviceEventsResponse_deviceEvents
              Prelude.. Lens._Just
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
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceEventsResponse'
            Prelude.<$> (x Data..?> "DeviceEvents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceEvents where
  hashWithSalt _salt ListDeviceEvents' {..} =
    _salt `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData ListDeviceEvents where
  rnf ListDeviceEvents' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deviceArn

instance Data.ToHeaders ListDeviceEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ListDeviceEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeviceEvents where
  toJSON ListDeviceEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventType" Data..=) Prelude.<$> eventType,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DeviceArn" Data..= deviceArn)
          ]
      )

instance Data.ToPath ListDeviceEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeviceEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { -- | The device events requested for the device ARN.
    deviceEvents :: Prelude.Maybe [DeviceEvent],
    -- | The token returned to indicate that there is more data available.
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
-- 'deviceEvents', 'listDeviceEventsResponse_deviceEvents' - The device events requested for the device ARN.
--
-- 'nextToken', 'listDeviceEventsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'httpStatus', 'listDeviceEventsResponse_httpStatus' - The response's http status code.
newListDeviceEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceEventsResponse
newListDeviceEventsResponse pHttpStatus_ =
  ListDeviceEventsResponse'
    { deviceEvents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device events requested for the device ARN.
listDeviceEventsResponse_deviceEvents :: Lens.Lens' ListDeviceEventsResponse (Prelude.Maybe [DeviceEvent])
listDeviceEventsResponse_deviceEvents = Lens.lens (\ListDeviceEventsResponse' {deviceEvents} -> deviceEvents) (\s@ListDeviceEventsResponse' {} a -> s {deviceEvents = a} :: ListDeviceEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token returned to indicate that there is more data available.
listDeviceEventsResponse_nextToken :: Lens.Lens' ListDeviceEventsResponse (Prelude.Maybe Prelude.Text)
listDeviceEventsResponse_nextToken = Lens.lens (\ListDeviceEventsResponse' {nextToken} -> nextToken) (\s@ListDeviceEventsResponse' {} a -> s {nextToken = a} :: ListDeviceEventsResponse)

-- | The response's http status code.
listDeviceEventsResponse_httpStatus :: Lens.Lens' ListDeviceEventsResponse Prelude.Int
listDeviceEventsResponse_httpStatus = Lens.lens (\ListDeviceEventsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceEventsResponse' {} a -> s {httpStatus = a} :: ListDeviceEventsResponse)

instance Prelude.NFData ListDeviceEventsResponse where
  rnf ListDeviceEventsResponse' {..} =
    Prelude.rnf deviceEvents
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

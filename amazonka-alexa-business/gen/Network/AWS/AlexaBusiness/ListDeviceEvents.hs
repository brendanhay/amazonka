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
-- Module      : Network.AWS.AlexaBusiness.ListDeviceEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the device event history, including device connection status, for
-- up to 30 days.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListDeviceEvents
  ( -- * Creating a Request
    ListDeviceEvents (..),
    newListDeviceEvents,

    -- * Request Lenses
    listDeviceEvents_nextToken,
    listDeviceEvents_eventType,
    listDeviceEvents_maxResults,
    listDeviceEvents_deviceArn,

    -- * Destructuring the Response
    ListDeviceEventsResponse (..),
    newListDeviceEventsResponse,

    -- * Response Lenses
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults. When the end of results is reached, the
    -- response has a value of null.
    nextToken :: Core.Maybe Core.Text,
    -- | The event type to filter device events. If EventType isn\'t specified,
    -- this returns a list of all device events in reverse chronological order.
    -- If EventType is specified, this returns a list of device events for that
    -- EventType in reverse chronological order.
    eventType :: Core.Maybe DeviceEventType,
    -- | The maximum number of results to include in the response. The default
    -- value is 50. If more results exist than the specified MaxResults value,
    -- a token is included in the response so that the remaining results can be
    -- retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ARN of a device.
    deviceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceEvents_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults. When the end of results is reached, the
-- response has a value of null.
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
-- 'deviceArn', 'listDeviceEvents_deviceArn' - The ARN of a device.
newListDeviceEvents ::
  -- | 'deviceArn'
  Core.Text ->
  ListDeviceEvents
newListDeviceEvents pDeviceArn_ =
  ListDeviceEvents'
    { nextToken = Core.Nothing,
      eventType = Core.Nothing,
      maxResults = Core.Nothing,
      deviceArn = pDeviceArn_
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults. When the end of results is reached, the
-- response has a value of null.
listDeviceEvents_nextToken :: Lens.Lens' ListDeviceEvents (Core.Maybe Core.Text)
listDeviceEvents_nextToken = Lens.lens (\ListDeviceEvents' {nextToken} -> nextToken) (\s@ListDeviceEvents' {} a -> s {nextToken = a} :: ListDeviceEvents)

-- | The event type to filter device events. If EventType isn\'t specified,
-- this returns a list of all device events in reverse chronological order.
-- If EventType is specified, this returns a list of device events for that
-- EventType in reverse chronological order.
listDeviceEvents_eventType :: Lens.Lens' ListDeviceEvents (Core.Maybe DeviceEventType)
listDeviceEvents_eventType = Lens.lens (\ListDeviceEvents' {eventType} -> eventType) (\s@ListDeviceEvents' {} a -> s {eventType = a} :: ListDeviceEvents)

-- | The maximum number of results to include in the response. The default
-- value is 50. If more results exist than the specified MaxResults value,
-- a token is included in the response so that the remaining results can be
-- retrieved.
listDeviceEvents_maxResults :: Lens.Lens' ListDeviceEvents (Core.Maybe Core.Natural)
listDeviceEvents_maxResults = Lens.lens (\ListDeviceEvents' {maxResults} -> maxResults) (\s@ListDeviceEvents' {} a -> s {maxResults = a} :: ListDeviceEvents)

-- | The ARN of a device.
listDeviceEvents_deviceArn :: Lens.Lens' ListDeviceEvents Core.Text
listDeviceEvents_deviceArn = Lens.lens (\ListDeviceEvents' {deviceArn} -> deviceArn) (\s@ListDeviceEvents' {} a -> s {deviceArn = a} :: ListDeviceEvents)

instance Core.AWSPager ListDeviceEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceEventsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceEventsResponse_deviceEvents
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeviceEvents_nextToken
          Lens..~ rs
          Lens.^? listDeviceEventsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDeviceEvents where
  type
    AWSResponse ListDeviceEvents =
      ListDeviceEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "DeviceEvents" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeviceEvents

instance Core.NFData ListDeviceEvents

instance Core.ToHeaders ListDeviceEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListDeviceEvents" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeviceEvents where
  toJSON ListDeviceEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("EventType" Core..=) Core.<$> eventType,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("DeviceArn" Core..= deviceArn)
          ]
      )

instance Core.ToPath ListDeviceEvents where
  toPath = Core.const "/"

instance Core.ToQuery ListDeviceEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The device events requested for the device ARN.
    deviceEvents :: Core.Maybe [DeviceEvent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceEventsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'deviceEvents', 'listDeviceEventsResponse_deviceEvents' - The device events requested for the device ARN.
--
-- 'httpStatus', 'listDeviceEventsResponse_httpStatus' - The response's http status code.
newListDeviceEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeviceEventsResponse
newListDeviceEventsResponse pHttpStatus_ =
  ListDeviceEventsResponse'
    { nextToken = Core.Nothing,
      deviceEvents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
listDeviceEventsResponse_nextToken :: Lens.Lens' ListDeviceEventsResponse (Core.Maybe Core.Text)
listDeviceEventsResponse_nextToken = Lens.lens (\ListDeviceEventsResponse' {nextToken} -> nextToken) (\s@ListDeviceEventsResponse' {} a -> s {nextToken = a} :: ListDeviceEventsResponse)

-- | The device events requested for the device ARN.
listDeviceEventsResponse_deviceEvents :: Lens.Lens' ListDeviceEventsResponse (Core.Maybe [DeviceEvent])
listDeviceEventsResponse_deviceEvents = Lens.lens (\ListDeviceEventsResponse' {deviceEvents} -> deviceEvents) (\s@ListDeviceEventsResponse' {} a -> s {deviceEvents = a} :: ListDeviceEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeviceEventsResponse_httpStatus :: Lens.Lens' ListDeviceEventsResponse Core.Int
listDeviceEventsResponse_httpStatus = Lens.lens (\ListDeviceEventsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceEventsResponse' {} a -> s {httpStatus = a} :: ListDeviceEventsResponse)

instance Core.NFData ListDeviceEventsResponse

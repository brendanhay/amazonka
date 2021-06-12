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
-- Module      : Network.AWS.Pinpoint.GetEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the event stream settings for an
-- application.
module Network.AWS.Pinpoint.GetEventStream
  ( -- * Creating a Request
    GetEventStream (..),
    newGetEventStream,

    -- * Request Lenses
    getEventStream_applicationId,

    -- * Destructuring the Response
    GetEventStreamResponse (..),
    newGetEventStreamResponse,

    -- * Response Lenses
    getEventStreamResponse_httpStatus,
    getEventStreamResponse_eventStream,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEventStream' smart constructor.
data GetEventStream = GetEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getEventStream_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetEventStream ::
  -- | 'applicationId'
  Core.Text ->
  GetEventStream
newGetEventStream pApplicationId_ =
  GetEventStream' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getEventStream_applicationId :: Lens.Lens' GetEventStream Core.Text
getEventStream_applicationId = Lens.lens (\GetEventStream' {applicationId} -> applicationId) (\s@GetEventStream' {} a -> s {applicationId = a} :: GetEventStream)

instance Core.AWSRequest GetEventStream where
  type
    AWSResponse GetEventStream =
      GetEventStreamResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetEventStream

instance Core.NFData GetEventStream

instance Core.ToHeaders GetEventStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetEventStream where
  toPath GetEventStream' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/eventstream"
      ]

instance Core.ToQuery GetEventStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetEventStreamResponse' smart constructor.
data GetEventStreamResponse = GetEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    eventStream :: EventStream
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStream', 'getEventStreamResponse_eventStream' - Undocumented member.
newGetEventStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'eventStream'
  EventStream ->
  GetEventStreamResponse
newGetEventStreamResponse pHttpStatus_ pEventStream_ =
  GetEventStreamResponse'
    { httpStatus = pHttpStatus_,
      eventStream = pEventStream_
    }

-- | The response's http status code.
getEventStreamResponse_httpStatus :: Lens.Lens' GetEventStreamResponse Core.Int
getEventStreamResponse_httpStatus = Lens.lens (\GetEventStreamResponse' {httpStatus} -> httpStatus) (\s@GetEventStreamResponse' {} a -> s {httpStatus = a} :: GetEventStreamResponse)

-- | Undocumented member.
getEventStreamResponse_eventStream :: Lens.Lens' GetEventStreamResponse EventStream
getEventStreamResponse_eventStream = Lens.lens (\GetEventStreamResponse' {eventStream} -> eventStream) (\s@GetEventStreamResponse' {} a -> s {eventStream = a} :: GetEventStreamResponse)

instance Core.NFData GetEventStreamResponse

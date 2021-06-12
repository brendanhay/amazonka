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
-- Module      : Network.AWS.Pinpoint.PutEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event stream for an application or updates the settings of
-- an existing event stream for an application.
module Network.AWS.Pinpoint.PutEventStream
  ( -- * Creating a Request
    PutEventStream (..),
    newPutEventStream,

    -- * Request Lenses
    putEventStream_applicationId,
    putEventStream_writeEventStream,

    -- * Destructuring the Response
    PutEventStreamResponse (..),
    newPutEventStreamResponse,

    -- * Response Lenses
    putEventStreamResponse_httpStatus,
    putEventStreamResponse_eventStream,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutEventStream' smart constructor.
data PutEventStream = PutEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeEventStream :: WriteEventStream
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'putEventStream_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeEventStream', 'putEventStream_writeEventStream' - Undocumented member.
newPutEventStream ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'writeEventStream'
  WriteEventStream ->
  PutEventStream
newPutEventStream pApplicationId_ pWriteEventStream_ =
  PutEventStream'
    { applicationId = pApplicationId_,
      writeEventStream = pWriteEventStream_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
putEventStream_applicationId :: Lens.Lens' PutEventStream Core.Text
putEventStream_applicationId = Lens.lens (\PutEventStream' {applicationId} -> applicationId) (\s@PutEventStream' {} a -> s {applicationId = a} :: PutEventStream)

-- | Undocumented member.
putEventStream_writeEventStream :: Lens.Lens' PutEventStream WriteEventStream
putEventStream_writeEventStream = Lens.lens (\PutEventStream' {writeEventStream} -> writeEventStream) (\s@PutEventStream' {} a -> s {writeEventStream = a} :: PutEventStream)

instance Core.AWSRequest PutEventStream where
  type
    AWSResponse PutEventStream =
      PutEventStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable PutEventStream

instance Core.NFData PutEventStream

instance Core.ToHeaders PutEventStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutEventStream where
  toJSON PutEventStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("WriteEventStream" Core..= writeEventStream)
          ]
      )

instance Core.ToPath PutEventStream where
  toPath PutEventStream' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/eventstream"
      ]

instance Core.ToQuery PutEventStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutEventStreamResponse' smart constructor.
data PutEventStreamResponse = PutEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    eventStream :: EventStream
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStream', 'putEventStreamResponse_eventStream' - Undocumented member.
newPutEventStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'eventStream'
  EventStream ->
  PutEventStreamResponse
newPutEventStreamResponse pHttpStatus_ pEventStream_ =
  PutEventStreamResponse'
    { httpStatus = pHttpStatus_,
      eventStream = pEventStream_
    }

-- | The response's http status code.
putEventStreamResponse_httpStatus :: Lens.Lens' PutEventStreamResponse Core.Int
putEventStreamResponse_httpStatus = Lens.lens (\PutEventStreamResponse' {httpStatus} -> httpStatus) (\s@PutEventStreamResponse' {} a -> s {httpStatus = a} :: PutEventStreamResponse)

-- | Undocumented member.
putEventStreamResponse_eventStream :: Lens.Lens' PutEventStreamResponse EventStream
putEventStreamResponse_eventStream = Lens.lens (\PutEventStreamResponse' {eventStream} -> eventStream) (\s@PutEventStreamResponse' {} a -> s {eventStream = a} :: PutEventStreamResponse)

instance Core.NFData PutEventStreamResponse

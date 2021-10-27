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
-- Module      : Network.AWS.Nimble.GetStreamingSessionStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a StreamingSessionStream for a streaming session.
--
-- Invoke this operation to poll the resource after invoking
-- CreateStreamingSessionStream.
--
-- After the StreamingSessionStream changes to the state READY, the url
-- property will contain a stream to be used with the DCV streaming client.
module Network.AWS.Nimble.GetStreamingSessionStream
  ( -- * Creating a Request
    GetStreamingSessionStream (..),
    newGetStreamingSessionStream,

    -- * Request Lenses
    getStreamingSessionStream_studioId,
    getStreamingSessionStream_streamId,
    getStreamingSessionStream_sessionId,

    -- * Destructuring the Response
    GetStreamingSessionStreamResponse (..),
    newGetStreamingSessionStreamResponse,

    -- * Response Lenses
    getStreamingSessionStreamResponse_stream,
    getStreamingSessionStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStreamingSessionStream' smart constructor.
data GetStreamingSessionStream = GetStreamingSessionStream'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSessionStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'getStreamingSessionStream_studioId' - The studio ID.
--
-- 'streamId', 'getStreamingSessionStream_streamId' - The stream ID.
--
-- 'sessionId', 'getStreamingSessionStream_sessionId' - The session ID.
newGetStreamingSessionStream ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'streamId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  GetStreamingSessionStream
newGetStreamingSessionStream
  pStudioId_
  pStreamId_
  pSessionId_ =
    GetStreamingSessionStream'
      { studioId = pStudioId_,
        streamId = pStreamId_,
        sessionId = pSessionId_
      }

-- | The studio ID.
getStreamingSessionStream_studioId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_studioId = Lens.lens (\GetStreamingSessionStream' {studioId} -> studioId) (\s@GetStreamingSessionStream' {} a -> s {studioId = a} :: GetStreamingSessionStream)

-- | The stream ID.
getStreamingSessionStream_streamId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_streamId = Lens.lens (\GetStreamingSessionStream' {streamId} -> streamId) (\s@GetStreamingSessionStream' {} a -> s {streamId = a} :: GetStreamingSessionStream)

-- | The session ID.
getStreamingSessionStream_sessionId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_sessionId = Lens.lens (\GetStreamingSessionStream' {sessionId} -> sessionId) (\s@GetStreamingSessionStream' {} a -> s {sessionId = a} :: GetStreamingSessionStream)

instance Core.AWSRequest GetStreamingSessionStream where
  type
    AWSResponse GetStreamingSessionStream =
      GetStreamingSessionStreamResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamingSessionStreamResponse'
            Prelude.<$> (x Core..?> "stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingSessionStream

instance Prelude.NFData GetStreamingSessionStream

instance Core.ToHeaders GetStreamingSessionStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetStreamingSessionStream where
  toPath GetStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-sessions/",
        Core.toBS sessionId,
        "/streams/",
        Core.toBS streamId
      ]

instance Core.ToQuery GetStreamingSessionStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamingSessionStreamResponse' smart constructor.
data GetStreamingSessionStreamResponse = GetStreamingSessionStreamResponse'
  { -- | The stream.
    stream :: Prelude.Maybe StreamingSessionStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSessionStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stream', 'getStreamingSessionStreamResponse_stream' - The stream.
--
-- 'httpStatus', 'getStreamingSessionStreamResponse_httpStatus' - The response's http status code.
newGetStreamingSessionStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingSessionStreamResponse
newGetStreamingSessionStreamResponse pHttpStatus_ =
  GetStreamingSessionStreamResponse'
    { stream =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stream.
getStreamingSessionStreamResponse_stream :: Lens.Lens' GetStreamingSessionStreamResponse (Prelude.Maybe StreamingSessionStream)
getStreamingSessionStreamResponse_stream = Lens.lens (\GetStreamingSessionStreamResponse' {stream} -> stream) (\s@GetStreamingSessionStreamResponse' {} a -> s {stream = a} :: GetStreamingSessionStreamResponse)

-- | The response's http status code.
getStreamingSessionStreamResponse_httpStatus :: Lens.Lens' GetStreamingSessionStreamResponse Prelude.Int
getStreamingSessionStreamResponse_httpStatus = Lens.lens (\GetStreamingSessionStreamResponse' {httpStatus} -> httpStatus) (\s@GetStreamingSessionStreamResponse' {} a -> s {httpStatus = a} :: GetStreamingSessionStreamResponse)

instance
  Prelude.NFData
    GetStreamingSessionStreamResponse

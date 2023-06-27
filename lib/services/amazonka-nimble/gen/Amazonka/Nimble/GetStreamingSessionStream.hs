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
-- Module      : Amazonka.Nimble.GetStreamingSessionStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a StreamingSessionStream for a streaming session.
--
-- Invoke this operation to poll the resource after invoking
-- @CreateStreamingSessionStream@.
--
-- After the @StreamingSessionStream@ changes to the @READY@ state, the url
-- property will contain a stream to be used with the DCV streaming client.
module Amazonka.Nimble.GetStreamingSessionStream
  ( -- * Creating a Request
    GetStreamingSessionStream (..),
    newGetStreamingSessionStream,

    -- * Request Lenses
    getStreamingSessionStream_sessionId,
    getStreamingSessionStream_streamId,
    getStreamingSessionStream_studioId,

    -- * Destructuring the Response
    GetStreamingSessionStreamResponse (..),
    newGetStreamingSessionStreamResponse,

    -- * Response Lenses
    getStreamingSessionStreamResponse_stream,
    getStreamingSessionStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamingSessionStream' smart constructor.
data GetStreamingSessionStream = GetStreamingSessionStream'
  { -- | The streaming session ID.
    sessionId :: Prelude.Text,
    -- | The streaming session stream ID.
    streamId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
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
-- 'sessionId', 'getStreamingSessionStream_sessionId' - The streaming session ID.
--
-- 'streamId', 'getStreamingSessionStream_streamId' - The streaming session stream ID.
--
-- 'studioId', 'getStreamingSessionStream_studioId' - The studio ID.
newGetStreamingSessionStream ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'streamId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStreamingSessionStream
newGetStreamingSessionStream
  pSessionId_
  pStreamId_
  pStudioId_ =
    GetStreamingSessionStream'
      { sessionId = pSessionId_,
        streamId = pStreamId_,
        studioId = pStudioId_
      }

-- | The streaming session ID.
getStreamingSessionStream_sessionId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_sessionId = Lens.lens (\GetStreamingSessionStream' {sessionId} -> sessionId) (\s@GetStreamingSessionStream' {} a -> s {sessionId = a} :: GetStreamingSessionStream)

-- | The streaming session stream ID.
getStreamingSessionStream_streamId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_streamId = Lens.lens (\GetStreamingSessionStream' {streamId} -> streamId) (\s@GetStreamingSessionStream' {} a -> s {streamId = a} :: GetStreamingSessionStream)

-- | The studio ID.
getStreamingSessionStream_studioId :: Lens.Lens' GetStreamingSessionStream Prelude.Text
getStreamingSessionStream_studioId = Lens.lens (\GetStreamingSessionStream' {studioId} -> studioId) (\s@GetStreamingSessionStream' {} a -> s {studioId = a} :: GetStreamingSessionStream)

instance Core.AWSRequest GetStreamingSessionStream where
  type
    AWSResponse GetStreamingSessionStream =
      GetStreamingSessionStreamResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamingSessionStreamResponse'
            Prelude.<$> (x Data..?> "stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingSessionStream where
  hashWithSalt _salt GetStreamingSessionStream' {..} =
    _salt
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStreamingSessionStream where
  rnf GetStreamingSessionStream' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetStreamingSessionStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStreamingSessionStream where
  toPath GetStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions/",
        Data.toBS sessionId,
        "/streams/",
        Data.toBS streamId
      ]

instance Data.ToQuery GetStreamingSessionStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamingSessionStreamResponse' smart constructor.
data GetStreamingSessionStreamResponse = GetStreamingSessionStreamResponse'
  { -- | The stream.
    stream :: Prelude.Maybe StreamingSessionStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetStreamingSessionStreamResponse' {..} =
    Prelude.rnf stream
      `Prelude.seq` Prelude.rnf httpStatus

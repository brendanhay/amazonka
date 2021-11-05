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
-- Module      : Amazonka.Nimble.CreateStreamingSessionStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming session stream for a streaming session.
--
-- After invoking this API, invoke GetStreamingSessionStream with the
-- returned streamId to poll the resource until it is in state READY.
module Amazonka.Nimble.CreateStreamingSessionStream
  ( -- * Creating a Request
    CreateStreamingSessionStream (..),
    newCreateStreamingSessionStream,

    -- * Request Lenses
    createStreamingSessionStream_expirationInSeconds,
    createStreamingSessionStream_clientToken,
    createStreamingSessionStream_studioId,
    createStreamingSessionStream_sessionId,

    -- * Destructuring the Response
    CreateStreamingSessionStreamResponse (..),
    newCreateStreamingSessionStreamResponse,

    -- * Response Lenses
    createStreamingSessionStreamResponse_stream,
    createStreamingSessionStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A list collection streams.
--
-- /See:/ 'newCreateStreamingSessionStream' smart constructor.
data CreateStreamingSessionStream = CreateStreamingSessionStream'
  { -- | The expiration time in seconds.
    expirationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingSessionStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationInSeconds', 'createStreamingSessionStream_expirationInSeconds' - The expiration time in seconds.
--
-- 'clientToken', 'createStreamingSessionStream_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'createStreamingSessionStream_studioId' - The studio ID.
--
-- 'sessionId', 'createStreamingSessionStream_sessionId' - The session ID.
newCreateStreamingSessionStream ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  CreateStreamingSessionStream
newCreateStreamingSessionStream
  pStudioId_
  pSessionId_ =
    CreateStreamingSessionStream'
      { expirationInSeconds =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        studioId = pStudioId_,
        sessionId = pSessionId_
      }

-- | The expiration time in seconds.
createStreamingSessionStream_expirationInSeconds :: Lens.Lens' CreateStreamingSessionStream (Prelude.Maybe Prelude.Natural)
createStreamingSessionStream_expirationInSeconds = Lens.lens (\CreateStreamingSessionStream' {expirationInSeconds} -> expirationInSeconds) (\s@CreateStreamingSessionStream' {} a -> s {expirationInSeconds = a} :: CreateStreamingSessionStream)

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
createStreamingSessionStream_clientToken :: Lens.Lens' CreateStreamingSessionStream (Prelude.Maybe Prelude.Text)
createStreamingSessionStream_clientToken = Lens.lens (\CreateStreamingSessionStream' {clientToken} -> clientToken) (\s@CreateStreamingSessionStream' {} a -> s {clientToken = a} :: CreateStreamingSessionStream)

-- | The studio ID.
createStreamingSessionStream_studioId :: Lens.Lens' CreateStreamingSessionStream Prelude.Text
createStreamingSessionStream_studioId = Lens.lens (\CreateStreamingSessionStream' {studioId} -> studioId) (\s@CreateStreamingSessionStream' {} a -> s {studioId = a} :: CreateStreamingSessionStream)

-- | The session ID.
createStreamingSessionStream_sessionId :: Lens.Lens' CreateStreamingSessionStream Prelude.Text
createStreamingSessionStream_sessionId = Lens.lens (\CreateStreamingSessionStream' {sessionId} -> sessionId) (\s@CreateStreamingSessionStream' {} a -> s {sessionId = a} :: CreateStreamingSessionStream)

instance Core.AWSRequest CreateStreamingSessionStream where
  type
    AWSResponse CreateStreamingSessionStream =
      CreateStreamingSessionStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingSessionStreamResponse'
            Prelude.<$> (x Core..?> "stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateStreamingSessionStream

instance Prelude.NFData CreateStreamingSessionStream

instance Core.ToHeaders CreateStreamingSessionStream where
  toHeaders CreateStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateStreamingSessionStream where
  toJSON CreateStreamingSessionStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expirationInSeconds" Core..=)
              Prelude.<$> expirationInSeconds
          ]
      )

instance Core.ToPath CreateStreamingSessionStream where
  toPath CreateStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-sessions/",
        Core.toBS sessionId,
        "/streams"
      ]

instance Core.ToQuery CreateStreamingSessionStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingSessionStreamResponse' smart constructor.
data CreateStreamingSessionStreamResponse = CreateStreamingSessionStreamResponse'
  { -- | The stream.
    stream :: Prelude.Maybe StreamingSessionStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingSessionStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stream', 'createStreamingSessionStreamResponse_stream' - The stream.
--
-- 'httpStatus', 'createStreamingSessionStreamResponse_httpStatus' - The response's http status code.
newCreateStreamingSessionStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamingSessionStreamResponse
newCreateStreamingSessionStreamResponse pHttpStatus_ =
  CreateStreamingSessionStreamResponse'
    { stream =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stream.
createStreamingSessionStreamResponse_stream :: Lens.Lens' CreateStreamingSessionStreamResponse (Prelude.Maybe StreamingSessionStream)
createStreamingSessionStreamResponse_stream = Lens.lens (\CreateStreamingSessionStreamResponse' {stream} -> stream) (\s@CreateStreamingSessionStreamResponse' {} a -> s {stream = a} :: CreateStreamingSessionStreamResponse)

-- | The response's http status code.
createStreamingSessionStreamResponse_httpStatus :: Lens.Lens' CreateStreamingSessionStreamResponse Prelude.Int
createStreamingSessionStreamResponse_httpStatus = Lens.lens (\CreateStreamingSessionStreamResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingSessionStreamResponse' {} a -> s {httpStatus = a} :: CreateStreamingSessionStreamResponse)

instance
  Prelude.NFData
    CreateStreamingSessionStreamResponse

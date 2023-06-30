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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming session stream for a streaming session.
--
-- After invoking this API, invoke GetStreamingSessionStream with the
-- returned streamId to poll the resource until it is in the @READY@ state.
module Amazonka.Nimble.CreateStreamingSessionStream
  ( -- * Creating a Request
    CreateStreamingSessionStream (..),
    newCreateStreamingSessionStream,

    -- * Request Lenses
    createStreamingSessionStream_clientToken,
    createStreamingSessionStream_expirationInSeconds,
    createStreamingSessionStream_sessionId,
    createStreamingSessionStream_studioId,

    -- * Destructuring the Response
    CreateStreamingSessionStreamResponse (..),
    newCreateStreamingSessionStreamResponse,

    -- * Response Lenses
    createStreamingSessionStreamResponse_stream,
    createStreamingSessionStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStreamingSessionStream' smart constructor.
data CreateStreamingSessionStream = CreateStreamingSessionStream'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The expiration time in seconds.
    expirationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The streaming session ID.
    sessionId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
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
-- 'clientToken', 'createStreamingSessionStream_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'expirationInSeconds', 'createStreamingSessionStream_expirationInSeconds' - The expiration time in seconds.
--
-- 'sessionId', 'createStreamingSessionStream_sessionId' - The streaming session ID.
--
-- 'studioId', 'createStreamingSessionStream_studioId' - The studio ID.
newCreateStreamingSessionStream ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  CreateStreamingSessionStream
newCreateStreamingSessionStream
  pSessionId_
  pStudioId_ =
    CreateStreamingSessionStream'
      { clientToken =
          Prelude.Nothing,
        expirationInSeconds = Prelude.Nothing,
        sessionId = pSessionId_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
createStreamingSessionStream_clientToken :: Lens.Lens' CreateStreamingSessionStream (Prelude.Maybe Prelude.Text)
createStreamingSessionStream_clientToken = Lens.lens (\CreateStreamingSessionStream' {clientToken} -> clientToken) (\s@CreateStreamingSessionStream' {} a -> s {clientToken = a} :: CreateStreamingSessionStream)

-- | The expiration time in seconds.
createStreamingSessionStream_expirationInSeconds :: Lens.Lens' CreateStreamingSessionStream (Prelude.Maybe Prelude.Natural)
createStreamingSessionStream_expirationInSeconds = Lens.lens (\CreateStreamingSessionStream' {expirationInSeconds} -> expirationInSeconds) (\s@CreateStreamingSessionStream' {} a -> s {expirationInSeconds = a} :: CreateStreamingSessionStream)

-- | The streaming session ID.
createStreamingSessionStream_sessionId :: Lens.Lens' CreateStreamingSessionStream Prelude.Text
createStreamingSessionStream_sessionId = Lens.lens (\CreateStreamingSessionStream' {sessionId} -> sessionId) (\s@CreateStreamingSessionStream' {} a -> s {sessionId = a} :: CreateStreamingSessionStream)

-- | The studio ID.
createStreamingSessionStream_studioId :: Lens.Lens' CreateStreamingSessionStream Prelude.Text
createStreamingSessionStream_studioId = Lens.lens (\CreateStreamingSessionStream' {studioId} -> studioId) (\s@CreateStreamingSessionStream' {} a -> s {studioId = a} :: CreateStreamingSessionStream)

instance Core.AWSRequest CreateStreamingSessionStream where
  type
    AWSResponse CreateStreamingSessionStream =
      CreateStreamingSessionStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingSessionStreamResponse'
            Prelude.<$> (x Data..?> "stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateStreamingSessionStream
  where
  hashWithSalt _salt CreateStreamingSessionStream' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` expirationInSeconds
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData CreateStreamingSessionStream where
  rnf CreateStreamingSessionStream' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf expirationInSeconds
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders CreateStreamingSessionStream where
  toHeaders CreateStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateStreamingSessionStream where
  toJSON CreateStreamingSessionStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expirationInSeconds" Data..=)
              Prelude.<$> expirationInSeconds
          ]
      )

instance Data.ToPath CreateStreamingSessionStream where
  toPath CreateStreamingSessionStream' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions/",
        Data.toBS sessionId,
        "/streams"
      ]

instance Data.ToQuery CreateStreamingSessionStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingSessionStreamResponse' smart constructor.
data CreateStreamingSessionStreamResponse = CreateStreamingSessionStreamResponse'
  { -- | The stream.
    stream :: Prelude.Maybe StreamingSessionStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateStreamingSessionStreamResponse' {..} =
    Prelude.rnf stream
      `Prelude.seq` Prelude.rnf httpStatus

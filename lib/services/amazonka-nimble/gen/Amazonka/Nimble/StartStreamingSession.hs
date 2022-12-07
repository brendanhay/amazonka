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
-- Module      : Amazonka.Nimble.StartStreamingSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transitions sessions from the STOPPED state into the READY state. The
-- START_IN_PROGRESS state is the intermediate state between the STOPPED
-- and READY states.
module Amazonka.Nimble.StartStreamingSession
  ( -- * Creating a Request
    StartStreamingSession (..),
    newStartStreamingSession,

    -- * Request Lenses
    startStreamingSession_clientToken,
    startStreamingSession_sessionId,
    startStreamingSession_studioId,

    -- * Destructuring the Response
    StartStreamingSessionResponse (..),
    newStartStreamingSessionResponse,

    -- * Response Lenses
    startStreamingSessionResponse_session,
    startStreamingSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartStreamingSession' smart constructor.
data StartStreamingSession = StartStreamingSession'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The streaming session ID for the StartStreamingSessionRequest.
    sessionId :: Prelude.Text,
    -- | The studio ID for the StartStreamingSessionRequest.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startStreamingSession_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'sessionId', 'startStreamingSession_sessionId' - The streaming session ID for the StartStreamingSessionRequest.
--
-- 'studioId', 'startStreamingSession_studioId' - The studio ID for the StartStreamingSessionRequest.
newStartStreamingSession ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  StartStreamingSession
newStartStreamingSession pSessionId_ pStudioId_ =
  StartStreamingSession'
    { clientToken =
        Prelude.Nothing,
      sessionId = pSessionId_,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
startStreamingSession_clientToken :: Lens.Lens' StartStreamingSession (Prelude.Maybe Prelude.Text)
startStreamingSession_clientToken = Lens.lens (\StartStreamingSession' {clientToken} -> clientToken) (\s@StartStreamingSession' {} a -> s {clientToken = a} :: StartStreamingSession)

-- | The streaming session ID for the StartStreamingSessionRequest.
startStreamingSession_sessionId :: Lens.Lens' StartStreamingSession Prelude.Text
startStreamingSession_sessionId = Lens.lens (\StartStreamingSession' {sessionId} -> sessionId) (\s@StartStreamingSession' {} a -> s {sessionId = a} :: StartStreamingSession)

-- | The studio ID for the StartStreamingSessionRequest.
startStreamingSession_studioId :: Lens.Lens' StartStreamingSession Prelude.Text
startStreamingSession_studioId = Lens.lens (\StartStreamingSession' {studioId} -> studioId) (\s@StartStreamingSession' {} a -> s {studioId = a} :: StartStreamingSession)

instance Core.AWSRequest StartStreamingSession where
  type
    AWSResponse StartStreamingSession =
      StartStreamingSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartStreamingSessionResponse'
            Prelude.<$> (x Data..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartStreamingSession where
  hashWithSalt _salt StartStreamingSession' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData StartStreamingSession where
  rnf StartStreamingSession' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders StartStreamingSession where
  toHeaders StartStreamingSession' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON StartStreamingSession where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartStreamingSession where
  toPath StartStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions/",
        Data.toBS sessionId,
        "/start"
      ]

instance Data.ToQuery StartStreamingSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStreamingSessionResponse' smart constructor.
data StartStreamingSessionResponse = StartStreamingSessionResponse'
  { session :: Prelude.Maybe StreamingSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStreamingSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'startStreamingSessionResponse_session' - Undocumented member.
--
-- 'httpStatus', 'startStreamingSessionResponse_httpStatus' - The response's http status code.
newStartStreamingSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartStreamingSessionResponse
newStartStreamingSessionResponse pHttpStatus_ =
  StartStreamingSessionResponse'
    { session =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startStreamingSessionResponse_session :: Lens.Lens' StartStreamingSessionResponse (Prelude.Maybe StreamingSession)
startStreamingSessionResponse_session = Lens.lens (\StartStreamingSessionResponse' {session} -> session) (\s@StartStreamingSessionResponse' {} a -> s {session = a} :: StartStreamingSessionResponse)

-- | The response's http status code.
startStreamingSessionResponse_httpStatus :: Lens.Lens' StartStreamingSessionResponse Prelude.Int
startStreamingSessionResponse_httpStatus = Lens.lens (\StartStreamingSessionResponse' {httpStatus} -> httpStatus) (\s@StartStreamingSessionResponse' {} a -> s {httpStatus = a} :: StartStreamingSessionResponse)

instance Prelude.NFData StartStreamingSessionResponse where
  rnf StartStreamingSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus

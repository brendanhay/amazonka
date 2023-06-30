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
-- Module      : Amazonka.Nimble.GetStreamingSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets StreamingSession resource.
--
-- Invoke this operation to poll for a streaming session state while
-- creating or deleting a session.
module Amazonka.Nimble.GetStreamingSession
  ( -- * Creating a Request
    GetStreamingSession (..),
    newGetStreamingSession,

    -- * Request Lenses
    getStreamingSession_sessionId,
    getStreamingSession_studioId,

    -- * Destructuring the Response
    GetStreamingSessionResponse (..),
    newGetStreamingSessionResponse,

    -- * Response Lenses
    getStreamingSessionResponse_session,
    getStreamingSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamingSession' smart constructor.
data GetStreamingSession = GetStreamingSession'
  { -- | The streaming session ID.
    sessionId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getStreamingSession_sessionId' - The streaming session ID.
--
-- 'studioId', 'getStreamingSession_studioId' - The studio ID.
newGetStreamingSession ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStreamingSession
newGetStreamingSession pSessionId_ pStudioId_ =
  GetStreamingSession'
    { sessionId = pSessionId_,
      studioId = pStudioId_
    }

-- | The streaming session ID.
getStreamingSession_sessionId :: Lens.Lens' GetStreamingSession Prelude.Text
getStreamingSession_sessionId = Lens.lens (\GetStreamingSession' {sessionId} -> sessionId) (\s@GetStreamingSession' {} a -> s {sessionId = a} :: GetStreamingSession)

-- | The studio ID.
getStreamingSession_studioId :: Lens.Lens' GetStreamingSession Prelude.Text
getStreamingSession_studioId = Lens.lens (\GetStreamingSession' {studioId} -> studioId) (\s@GetStreamingSession' {} a -> s {studioId = a} :: GetStreamingSession)

instance Core.AWSRequest GetStreamingSession where
  type
    AWSResponse GetStreamingSession =
      GetStreamingSessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamingSessionResponse'
            Prelude.<$> (x Data..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingSession where
  hashWithSalt _salt GetStreamingSession' {..} =
    _salt
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStreamingSession where
  rnf GetStreamingSession' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetStreamingSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStreamingSession where
  toPath GetStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions/",
        Data.toBS sessionId
      ]

instance Data.ToQuery GetStreamingSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamingSessionResponse' smart constructor.
data GetStreamingSessionResponse = GetStreamingSessionResponse'
  { -- | The session.
    session :: Prelude.Maybe StreamingSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'getStreamingSessionResponse_session' - The session.
--
-- 'httpStatus', 'getStreamingSessionResponse_httpStatus' - The response's http status code.
newGetStreamingSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingSessionResponse
newGetStreamingSessionResponse pHttpStatus_ =
  GetStreamingSessionResponse'
    { session =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session.
getStreamingSessionResponse_session :: Lens.Lens' GetStreamingSessionResponse (Prelude.Maybe StreamingSession)
getStreamingSessionResponse_session = Lens.lens (\GetStreamingSessionResponse' {session} -> session) (\s@GetStreamingSessionResponse' {} a -> s {session = a} :: GetStreamingSessionResponse)

-- | The response's http status code.
getStreamingSessionResponse_httpStatus :: Lens.Lens' GetStreamingSessionResponse Prelude.Int
getStreamingSessionResponse_httpStatus = Lens.lens (\GetStreamingSessionResponse' {httpStatus} -> httpStatus) (\s@GetStreamingSessionResponse' {} a -> s {httpStatus = a} :: GetStreamingSessionResponse)

instance Prelude.NFData GetStreamingSessionResponse where
  rnf GetStreamingSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Wisdom.GetSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information for a specified session.
module Amazonka.Wisdom.GetSession
  ( -- * Creating a Request
    GetSession (..),
    newGetSession,

    -- * Request Lenses
    getSession_assistantId,
    getSession_sessionId,

    -- * Destructuring the Response
    GetSessionResponse (..),
    newGetSessionResponse,

    -- * Response Lenses
    getSessionResponse_session,
    getSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The identifier of the session. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantId', 'getSession_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'sessionId', 'getSession_sessionId' - The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
newGetSession ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  GetSession
newGetSession pAssistantId_ pSessionId_ =
  GetSession'
    { assistantId = pAssistantId_,
      sessionId = pSessionId_
    }

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getSession_assistantId :: Lens.Lens' GetSession Prelude.Text
getSession_assistantId = Lens.lens (\GetSession' {assistantId} -> assistantId) (\s@GetSession' {} a -> s {assistantId = a} :: GetSession)

-- | The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
getSession_sessionId :: Lens.Lens' GetSession Prelude.Text
getSession_sessionId = Lens.lens (\GetSession' {sessionId} -> sessionId) (\s@GetSession' {} a -> s {sessionId = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Prelude.<$> (x Core..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSession where
  hashWithSalt _salt GetSession' {..} =
    _salt `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData GetSession where
  rnf GetSession' {..} =
    Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf sessionId

instance Core.ToHeaders GetSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSession where
  toPath GetSession' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Core.toBS assistantId,
        "/sessions/",
        Core.toBS sessionId
      ]

instance Core.ToQuery GetSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | The session.
    session :: Prelude.Maybe SessionData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'getSessionResponse_session' - The session.
--
-- 'httpStatus', 'getSessionResponse_httpStatus' - The response's http status code.
newGetSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionResponse
newGetSessionResponse pHttpStatus_ =
  GetSessionResponse'
    { session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session.
getSessionResponse_session :: Lens.Lens' GetSessionResponse (Prelude.Maybe SessionData)
getSessionResponse_session = Lens.lens (\GetSessionResponse' {session} -> session) (\s@GetSessionResponse' {} a -> s {session = a} :: GetSessionResponse)

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Prelude.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Prelude.NFData GetSessionResponse where
  rnf GetSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus

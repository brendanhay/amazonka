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
-- Module      : Amazonka.Glue.GetSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the session.
module Amazonka.Glue.GetSession
  ( -- * Creating a Request
    GetSession (..),
    newGetSession,

    -- * Request Lenses
    getSession_requestOrigin,
    getSession_id,

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
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session.
    id :: Prelude.Text
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
-- 'requestOrigin', 'getSession_requestOrigin' - The origin of the request.
--
-- 'id', 'getSession_id' - The ID of the session.
newGetSession ::
  -- | 'id'
  Prelude.Text ->
  GetSession
newGetSession pId_ =
  GetSession'
    { requestOrigin = Prelude.Nothing,
      id = pId_
    }

-- | The origin of the request.
getSession_requestOrigin :: Lens.Lens' GetSession (Prelude.Maybe Prelude.Text)
getSession_requestOrigin = Lens.lens (\GetSession' {requestOrigin} -> requestOrigin) (\s@GetSession' {} a -> s {requestOrigin = a} :: GetSession)

-- | The ID of the session.
getSession_id :: Lens.Lens' GetSession Prelude.Text
getSession_id = Lens.lens (\GetSession' {id} -> id) (\s@GetSession' {} a -> s {id = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Prelude.<$> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSession where
  hashWithSalt _salt GetSession' {..} =
    _salt `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetSession where
  rnf GetSession' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSession where
  toJSON GetSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetSession where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | The session object is returned in the response.
    session :: Prelude.Maybe Session,
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
-- 'session', 'getSessionResponse_session' - The session object is returned in the response.
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

-- | The session object is returned in the response.
getSessionResponse_session :: Lens.Lens' GetSessionResponse (Prelude.Maybe Session)
getSessionResponse_session = Lens.lens (\GetSessionResponse' {session} -> session) (\s@GetSessionResponse' {} a -> s {session = a} :: GetSessionResponse)

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Prelude.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Prelude.NFData GetSessionResponse where
  rnf GetSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Athena.GetSessionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current status of a session.
module Amazonka.Athena.GetSessionStatus
  ( -- * Creating a Request
    GetSessionStatus (..),
    newGetSessionStatus,

    -- * Request Lenses
    getSessionStatus_sessionId,

    -- * Destructuring the Response
    GetSessionStatusResponse (..),
    newGetSessionStatusResponse,

    -- * Response Lenses
    getSessionStatusResponse_sessionId,
    getSessionStatusResponse_status,
    getSessionStatusResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSessionStatus' smart constructor.
data GetSessionStatus = GetSessionStatus'
  { -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getSessionStatus_sessionId' - The session ID.
newGetSessionStatus ::
  -- | 'sessionId'
  Prelude.Text ->
  GetSessionStatus
newGetSessionStatus pSessionId_ =
  GetSessionStatus' {sessionId = pSessionId_}

-- | The session ID.
getSessionStatus_sessionId :: Lens.Lens' GetSessionStatus Prelude.Text
getSessionStatus_sessionId = Lens.lens (\GetSessionStatus' {sessionId} -> sessionId) (\s@GetSessionStatus' {} a -> s {sessionId = a} :: GetSessionStatus)

instance Core.AWSRequest GetSessionStatus where
  type
    AWSResponse GetSessionStatus =
      GetSessionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionStatusResponse'
            Prelude.<$> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSessionStatus where
  hashWithSalt _salt GetSessionStatus' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData GetSessionStatus where
  rnf GetSessionStatus' {..} = Prelude.rnf sessionId

instance Data.ToHeaders GetSessionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetSessionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSessionStatus where
  toJSON GetSessionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath GetSessionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSessionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSessionStatusResponse' smart constructor.
data GetSessionStatusResponse = GetSessionStatusResponse'
  { -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the status of the session.
    status :: Prelude.Maybe SessionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getSessionStatusResponse_sessionId' - The session ID.
--
-- 'status', 'getSessionStatusResponse_status' - Contains information about the status of the session.
--
-- 'httpStatus', 'getSessionStatusResponse_httpStatus' - The response's http status code.
newGetSessionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionStatusResponse
newGetSessionStatusResponse pHttpStatus_ =
  GetSessionStatusResponse'
    { sessionId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session ID.
getSessionStatusResponse_sessionId :: Lens.Lens' GetSessionStatusResponse (Prelude.Maybe Prelude.Text)
getSessionStatusResponse_sessionId = Lens.lens (\GetSessionStatusResponse' {sessionId} -> sessionId) (\s@GetSessionStatusResponse' {} a -> s {sessionId = a} :: GetSessionStatusResponse)

-- | Contains information about the status of the session.
getSessionStatusResponse_status :: Lens.Lens' GetSessionStatusResponse (Prelude.Maybe SessionStatus)
getSessionStatusResponse_status = Lens.lens (\GetSessionStatusResponse' {status} -> status) (\s@GetSessionStatusResponse' {} a -> s {status = a} :: GetSessionStatusResponse)

-- | The response's http status code.
getSessionStatusResponse_httpStatus :: Lens.Lens' GetSessionStatusResponse Prelude.Int
getSessionStatusResponse_httpStatus = Lens.lens (\GetSessionStatusResponse' {httpStatus} -> httpStatus) (\s@GetSessionStatusResponse' {} a -> s {httpStatus = a} :: GetSessionStatusResponse)

instance Prelude.NFData GetSessionStatusResponse where
  rnf GetSessionStatusResponse' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

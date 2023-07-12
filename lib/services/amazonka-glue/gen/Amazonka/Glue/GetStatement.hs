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
-- Module      : Amazonka.Glue.GetStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the statement.
module Amazonka.Glue.GetStatement
  ( -- * Creating a Request
    GetStatement (..),
    newGetStatement,

    -- * Request Lenses
    getStatement_requestOrigin,
    getStatement_sessionId,
    getStatement_id,

    -- * Destructuring the Response
    GetStatementResponse (..),
    newGetStatementResponse,

    -- * Response Lenses
    getStatementResponse_statement,
    getStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStatement' smart constructor.
data GetStatement = GetStatement'
  { -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The Session ID of the statement.
    sessionId :: Prelude.Text,
    -- | The Id of the statement.
    id :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestOrigin', 'getStatement_requestOrigin' - The origin of the request.
--
-- 'sessionId', 'getStatement_sessionId' - The Session ID of the statement.
--
-- 'id', 'getStatement_id' - The Id of the statement.
newGetStatement ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Int ->
  GetStatement
newGetStatement pSessionId_ pId_ =
  GetStatement'
    { requestOrigin = Prelude.Nothing,
      sessionId = pSessionId_,
      id = pId_
    }

-- | The origin of the request.
getStatement_requestOrigin :: Lens.Lens' GetStatement (Prelude.Maybe Prelude.Text)
getStatement_requestOrigin = Lens.lens (\GetStatement' {requestOrigin} -> requestOrigin) (\s@GetStatement' {} a -> s {requestOrigin = a} :: GetStatement)

-- | The Session ID of the statement.
getStatement_sessionId :: Lens.Lens' GetStatement Prelude.Text
getStatement_sessionId = Lens.lens (\GetStatement' {sessionId} -> sessionId) (\s@GetStatement' {} a -> s {sessionId = a} :: GetStatement)

-- | The Id of the statement.
getStatement_id :: Lens.Lens' GetStatement Prelude.Int
getStatement_id = Lens.lens (\GetStatement' {id} -> id) (\s@GetStatement' {} a -> s {id = a} :: GetStatement)

instance Core.AWSRequest GetStatement where
  type AWSResponse GetStatement = GetStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStatementResponse'
            Prelude.<$> (x Data..?> "Statement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStatement where
  hashWithSalt _salt GetStatement' {..} =
    _salt
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetStatement where
  rnf GetStatement' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetStatement" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStatement where
  toJSON GetStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("SessionId" Data..= sessionId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery GetStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStatementResponse' smart constructor.
data GetStatementResponse = GetStatementResponse'
  { -- | Returns the statement.
    statement :: Prelude.Maybe Statement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statement', 'getStatementResponse_statement' - Returns the statement.
--
-- 'httpStatus', 'getStatementResponse_httpStatus' - The response's http status code.
newGetStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStatementResponse
newGetStatementResponse pHttpStatus_ =
  GetStatementResponse'
    { statement = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the statement.
getStatementResponse_statement :: Lens.Lens' GetStatementResponse (Prelude.Maybe Statement)
getStatementResponse_statement = Lens.lens (\GetStatementResponse' {statement} -> statement) (\s@GetStatementResponse' {} a -> s {statement = a} :: GetStatementResponse)

-- | The response's http status code.
getStatementResponse_httpStatus :: Lens.Lens' GetStatementResponse Prelude.Int
getStatementResponse_httpStatus = Lens.lens (\GetStatementResponse' {httpStatus} -> httpStatus) (\s@GetStatementResponse' {} a -> s {httpStatus = a} :: GetStatementResponse)

instance Prelude.NFData GetStatementResponse where
  rnf GetStatementResponse' {..} =
    Prelude.rnf statement
      `Prelude.seq` Prelude.rnf httpStatus

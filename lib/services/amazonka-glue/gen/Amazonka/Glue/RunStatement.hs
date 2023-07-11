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
-- Module      : Amazonka.Glue.RunStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes the statement.
module Amazonka.Glue.RunStatement
  ( -- * Creating a Request
    RunStatement (..),
    newRunStatement,

    -- * Request Lenses
    runStatement_requestOrigin,
    runStatement_sessionId,
    runStatement_code,

    -- * Destructuring the Response
    RunStatementResponse (..),
    newRunStatementResponse,

    -- * Response Lenses
    runStatementResponse_id,
    runStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRunStatement' smart constructor.
data RunStatement = RunStatement'
  { -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The Session Id of the statement to be run.
    sessionId :: Prelude.Text,
    -- | The statement code to be run.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestOrigin', 'runStatement_requestOrigin' - The origin of the request.
--
-- 'sessionId', 'runStatement_sessionId' - The Session Id of the statement to be run.
--
-- 'code', 'runStatement_code' - The statement code to be run.
newRunStatement ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  RunStatement
newRunStatement pSessionId_ pCode_ =
  RunStatement'
    { requestOrigin = Prelude.Nothing,
      sessionId = pSessionId_,
      code = pCode_
    }

-- | The origin of the request.
runStatement_requestOrigin :: Lens.Lens' RunStatement (Prelude.Maybe Prelude.Text)
runStatement_requestOrigin = Lens.lens (\RunStatement' {requestOrigin} -> requestOrigin) (\s@RunStatement' {} a -> s {requestOrigin = a} :: RunStatement)

-- | The Session Id of the statement to be run.
runStatement_sessionId :: Lens.Lens' RunStatement Prelude.Text
runStatement_sessionId = Lens.lens (\RunStatement' {sessionId} -> sessionId) (\s@RunStatement' {} a -> s {sessionId = a} :: RunStatement)

-- | The statement code to be run.
runStatement_code :: Lens.Lens' RunStatement Prelude.Text
runStatement_code = Lens.lens (\RunStatement' {code} -> code) (\s@RunStatement' {} a -> s {code = a} :: RunStatement)

instance Core.AWSRequest RunStatement where
  type AWSResponse RunStatement = RunStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RunStatementResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunStatement where
  hashWithSalt _salt RunStatement' {..} =
    _salt
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` code

instance Prelude.NFData RunStatement where
  rnf RunStatement' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf code

instance Data.ToHeaders RunStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.RunStatement" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RunStatement where
  toJSON RunStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("SessionId" Data..= sessionId),
            Prelude.Just ("Code" Data..= code)
          ]
      )

instance Data.ToPath RunStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery RunStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRunStatementResponse' smart constructor.
data RunStatementResponse = RunStatementResponse'
  { -- | Returns the Id of the statement that was run.
    id :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'runStatementResponse_id' - Returns the Id of the statement that was run.
--
-- 'httpStatus', 'runStatementResponse_httpStatus' - The response's http status code.
newRunStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RunStatementResponse
newRunStatementResponse pHttpStatus_ =
  RunStatementResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the Id of the statement that was run.
runStatementResponse_id :: Lens.Lens' RunStatementResponse (Prelude.Maybe Prelude.Int)
runStatementResponse_id = Lens.lens (\RunStatementResponse' {id} -> id) (\s@RunStatementResponse' {} a -> s {id = a} :: RunStatementResponse)

-- | The response's http status code.
runStatementResponse_httpStatus :: Lens.Lens' RunStatementResponse Prelude.Int
runStatementResponse_httpStatus = Lens.lens (\RunStatementResponse' {httpStatus} -> httpStatus) (\s@RunStatementResponse' {} a -> s {httpStatus = a} :: RunStatementResponse)

instance Prelude.NFData RunStatementResponse where
  rnf RunStatementResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus

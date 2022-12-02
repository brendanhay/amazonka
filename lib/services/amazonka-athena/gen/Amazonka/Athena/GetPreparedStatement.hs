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
-- Module      : Amazonka.Athena.GetPreparedStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the prepared statement with the specified name from the
-- specified workgroup.
module Amazonka.Athena.GetPreparedStatement
  ( -- * Creating a Request
    GetPreparedStatement (..),
    newGetPreparedStatement,

    -- * Request Lenses
    getPreparedStatement_statementName,
    getPreparedStatement_workGroup,

    -- * Destructuring the Response
    GetPreparedStatementResponse (..),
    newGetPreparedStatementResponse,

    -- * Response Lenses
    getPreparedStatementResponse_preparedStatement,
    getPreparedStatementResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPreparedStatement' smart constructor.
data GetPreparedStatement = GetPreparedStatement'
  { -- | The name of the prepared statement to retrieve.
    statementName :: Prelude.Text,
    -- | The workgroup to which the statement to be retrieved belongs.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statementName', 'getPreparedStatement_statementName' - The name of the prepared statement to retrieve.
--
-- 'workGroup', 'getPreparedStatement_workGroup' - The workgroup to which the statement to be retrieved belongs.
newGetPreparedStatement ::
  -- | 'statementName'
  Prelude.Text ->
  -- | 'workGroup'
  Prelude.Text ->
  GetPreparedStatement
newGetPreparedStatement pStatementName_ pWorkGroup_ =
  GetPreparedStatement'
    { statementName =
        pStatementName_,
      workGroup = pWorkGroup_
    }

-- | The name of the prepared statement to retrieve.
getPreparedStatement_statementName :: Lens.Lens' GetPreparedStatement Prelude.Text
getPreparedStatement_statementName = Lens.lens (\GetPreparedStatement' {statementName} -> statementName) (\s@GetPreparedStatement' {} a -> s {statementName = a} :: GetPreparedStatement)

-- | The workgroup to which the statement to be retrieved belongs.
getPreparedStatement_workGroup :: Lens.Lens' GetPreparedStatement Prelude.Text
getPreparedStatement_workGroup = Lens.lens (\GetPreparedStatement' {workGroup} -> workGroup) (\s@GetPreparedStatement' {} a -> s {workGroup = a} :: GetPreparedStatement)

instance Core.AWSRequest GetPreparedStatement where
  type
    AWSResponse GetPreparedStatement =
      GetPreparedStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPreparedStatementResponse'
            Prelude.<$> (x Data..?> "PreparedStatement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPreparedStatement where
  hashWithSalt _salt GetPreparedStatement' {..} =
    _salt `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData GetPreparedStatement where
  rnf GetPreparedStatement' {..} =
    Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf workGroup

instance Data.ToHeaders GetPreparedStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetPreparedStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPreparedStatement where
  toJSON GetPreparedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StatementName" Data..= statementName),
            Prelude.Just ("WorkGroup" Data..= workGroup)
          ]
      )

instance Data.ToPath GetPreparedStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPreparedStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPreparedStatementResponse' smart constructor.
data GetPreparedStatementResponse = GetPreparedStatementResponse'
  { -- | The name of the prepared statement that was retrieved.
    preparedStatement :: Prelude.Maybe PreparedStatement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPreparedStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preparedStatement', 'getPreparedStatementResponse_preparedStatement' - The name of the prepared statement that was retrieved.
--
-- 'httpStatus', 'getPreparedStatementResponse_httpStatus' - The response's http status code.
newGetPreparedStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPreparedStatementResponse
newGetPreparedStatementResponse pHttpStatus_ =
  GetPreparedStatementResponse'
    { preparedStatement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the prepared statement that was retrieved.
getPreparedStatementResponse_preparedStatement :: Lens.Lens' GetPreparedStatementResponse (Prelude.Maybe PreparedStatement)
getPreparedStatementResponse_preparedStatement = Lens.lens (\GetPreparedStatementResponse' {preparedStatement} -> preparedStatement) (\s@GetPreparedStatementResponse' {} a -> s {preparedStatement = a} :: GetPreparedStatementResponse)

-- | The response's http status code.
getPreparedStatementResponse_httpStatus :: Lens.Lens' GetPreparedStatementResponse Prelude.Int
getPreparedStatementResponse_httpStatus = Lens.lens (\GetPreparedStatementResponse' {httpStatus} -> httpStatus) (\s@GetPreparedStatementResponse' {} a -> s {httpStatus = a} :: GetPreparedStatementResponse)

instance Prelude.NFData GetPreparedStatementResponse where
  rnf GetPreparedStatementResponse' {..} =
    Prelude.rnf preparedStatement
      `Prelude.seq` Prelude.rnf httpStatus

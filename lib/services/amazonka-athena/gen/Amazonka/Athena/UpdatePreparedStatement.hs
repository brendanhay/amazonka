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
-- Module      : Amazonka.Athena.UpdatePreparedStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a prepared statement.
module Amazonka.Athena.UpdatePreparedStatement
  ( -- * Creating a Request
    UpdatePreparedStatement (..),
    newUpdatePreparedStatement,

    -- * Request Lenses
    updatePreparedStatement_description,
    updatePreparedStatement_statementName,
    updatePreparedStatement_workGroup,
    updatePreparedStatement_queryStatement,

    -- * Destructuring the Response
    UpdatePreparedStatementResponse (..),
    newUpdatePreparedStatementResponse,

    -- * Response Lenses
    updatePreparedStatementResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePreparedStatement' smart constructor.
data UpdatePreparedStatement = UpdatePreparedStatement'
  { -- | The description of the prepared statement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the prepared statement.
    statementName :: Prelude.Text,
    -- | The workgroup for the prepared statement.
    workGroup :: Prelude.Text,
    -- | The query string for the prepared statement.
    queryStatement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePreparedStatement_description' - The description of the prepared statement.
--
-- 'statementName', 'updatePreparedStatement_statementName' - The name of the prepared statement.
--
-- 'workGroup', 'updatePreparedStatement_workGroup' - The workgroup for the prepared statement.
--
-- 'queryStatement', 'updatePreparedStatement_queryStatement' - The query string for the prepared statement.
newUpdatePreparedStatement ::
  -- | 'statementName'
  Prelude.Text ->
  -- | 'workGroup'
  Prelude.Text ->
  -- | 'queryStatement'
  Prelude.Text ->
  UpdatePreparedStatement
newUpdatePreparedStatement
  pStatementName_
  pWorkGroup_
  pQueryStatement_ =
    UpdatePreparedStatement'
      { description =
          Prelude.Nothing,
        statementName = pStatementName_,
        workGroup = pWorkGroup_,
        queryStatement = pQueryStatement_
      }

-- | The description of the prepared statement.
updatePreparedStatement_description :: Lens.Lens' UpdatePreparedStatement (Prelude.Maybe Prelude.Text)
updatePreparedStatement_description = Lens.lens (\UpdatePreparedStatement' {description} -> description) (\s@UpdatePreparedStatement' {} a -> s {description = a} :: UpdatePreparedStatement)

-- | The name of the prepared statement.
updatePreparedStatement_statementName :: Lens.Lens' UpdatePreparedStatement Prelude.Text
updatePreparedStatement_statementName = Lens.lens (\UpdatePreparedStatement' {statementName} -> statementName) (\s@UpdatePreparedStatement' {} a -> s {statementName = a} :: UpdatePreparedStatement)

-- | The workgroup for the prepared statement.
updatePreparedStatement_workGroup :: Lens.Lens' UpdatePreparedStatement Prelude.Text
updatePreparedStatement_workGroup = Lens.lens (\UpdatePreparedStatement' {workGroup} -> workGroup) (\s@UpdatePreparedStatement' {} a -> s {workGroup = a} :: UpdatePreparedStatement)

-- | The query string for the prepared statement.
updatePreparedStatement_queryStatement :: Lens.Lens' UpdatePreparedStatement Prelude.Text
updatePreparedStatement_queryStatement = Lens.lens (\UpdatePreparedStatement' {queryStatement} -> queryStatement) (\s@UpdatePreparedStatement' {} a -> s {queryStatement = a} :: UpdatePreparedStatement)

instance Core.AWSRequest UpdatePreparedStatement where
  type
    AWSResponse UpdatePreparedStatement =
      UpdatePreparedStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePreparedStatementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePreparedStatement where
  hashWithSalt _salt UpdatePreparedStatement' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` queryStatement

instance Prelude.NFData UpdatePreparedStatement where
  rnf UpdatePreparedStatement' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf queryStatement

instance Data.ToHeaders UpdatePreparedStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdatePreparedStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePreparedStatement where
  toJSON UpdatePreparedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("StatementName" Data..= statementName),
            Prelude.Just ("WorkGroup" Data..= workGroup),
            Prelude.Just
              ("QueryStatement" Data..= queryStatement)
          ]
      )

instance Data.ToPath UpdatePreparedStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePreparedStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePreparedStatementResponse' smart constructor.
data UpdatePreparedStatementResponse = UpdatePreparedStatementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePreparedStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePreparedStatementResponse_httpStatus' - The response's http status code.
newUpdatePreparedStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePreparedStatementResponse
newUpdatePreparedStatementResponse pHttpStatus_ =
  UpdatePreparedStatementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePreparedStatementResponse_httpStatus :: Lens.Lens' UpdatePreparedStatementResponse Prelude.Int
updatePreparedStatementResponse_httpStatus = Lens.lens (\UpdatePreparedStatementResponse' {httpStatus} -> httpStatus) (\s@UpdatePreparedStatementResponse' {} a -> s {httpStatus = a} :: UpdatePreparedStatementResponse)

instance
  Prelude.NFData
    UpdatePreparedStatementResponse
  where
  rnf UpdatePreparedStatementResponse' {..} =
    Prelude.rnf httpStatus

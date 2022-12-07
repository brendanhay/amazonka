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
-- Module      : Amazonka.Athena.DeletePreparedStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the prepared statement with the specified name from the
-- specified workgroup.
module Amazonka.Athena.DeletePreparedStatement
  ( -- * Creating a Request
    DeletePreparedStatement (..),
    newDeletePreparedStatement,

    -- * Request Lenses
    deletePreparedStatement_statementName,
    deletePreparedStatement_workGroup,

    -- * Destructuring the Response
    DeletePreparedStatementResponse (..),
    newDeletePreparedStatementResponse,

    -- * Response Lenses
    deletePreparedStatementResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePreparedStatement' smart constructor.
data DeletePreparedStatement = DeletePreparedStatement'
  { -- | The name of the prepared statement to delete.
    statementName :: Prelude.Text,
    -- | The workgroup to which the statement to be deleted belongs.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statementName', 'deletePreparedStatement_statementName' - The name of the prepared statement to delete.
--
-- 'workGroup', 'deletePreparedStatement_workGroup' - The workgroup to which the statement to be deleted belongs.
newDeletePreparedStatement ::
  -- | 'statementName'
  Prelude.Text ->
  -- | 'workGroup'
  Prelude.Text ->
  DeletePreparedStatement
newDeletePreparedStatement
  pStatementName_
  pWorkGroup_ =
    DeletePreparedStatement'
      { statementName =
          pStatementName_,
        workGroup = pWorkGroup_
      }

-- | The name of the prepared statement to delete.
deletePreparedStatement_statementName :: Lens.Lens' DeletePreparedStatement Prelude.Text
deletePreparedStatement_statementName = Lens.lens (\DeletePreparedStatement' {statementName} -> statementName) (\s@DeletePreparedStatement' {} a -> s {statementName = a} :: DeletePreparedStatement)

-- | The workgroup to which the statement to be deleted belongs.
deletePreparedStatement_workGroup :: Lens.Lens' DeletePreparedStatement Prelude.Text
deletePreparedStatement_workGroup = Lens.lens (\DeletePreparedStatement' {workGroup} -> workGroup) (\s@DeletePreparedStatement' {} a -> s {workGroup = a} :: DeletePreparedStatement)

instance Core.AWSRequest DeletePreparedStatement where
  type
    AWSResponse DeletePreparedStatement =
      DeletePreparedStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePreparedStatementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePreparedStatement where
  hashWithSalt _salt DeletePreparedStatement' {..} =
    _salt `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData DeletePreparedStatement where
  rnf DeletePreparedStatement' {..} =
    Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf workGroup

instance Data.ToHeaders DeletePreparedStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.DeletePreparedStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePreparedStatement where
  toJSON DeletePreparedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StatementName" Data..= statementName),
            Prelude.Just ("WorkGroup" Data..= workGroup)
          ]
      )

instance Data.ToPath DeletePreparedStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePreparedStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePreparedStatementResponse' smart constructor.
data DeletePreparedStatementResponse = DeletePreparedStatementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePreparedStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePreparedStatementResponse_httpStatus' - The response's http status code.
newDeletePreparedStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePreparedStatementResponse
newDeletePreparedStatementResponse pHttpStatus_ =
  DeletePreparedStatementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePreparedStatementResponse_httpStatus :: Lens.Lens' DeletePreparedStatementResponse Prelude.Int
deletePreparedStatementResponse_httpStatus = Lens.lens (\DeletePreparedStatementResponse' {httpStatus} -> httpStatus) (\s@DeletePreparedStatementResponse' {} a -> s {httpStatus = a} :: DeletePreparedStatementResponse)

instance
  Prelude.NFData
    DeletePreparedStatementResponse
  where
  rnf DeletePreparedStatementResponse' {..} =
    Prelude.rnf httpStatus

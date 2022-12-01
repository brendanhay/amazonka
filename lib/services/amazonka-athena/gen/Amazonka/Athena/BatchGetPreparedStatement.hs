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
-- Module      : Amazonka.Athena.BatchGetPreparedStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single prepared statement or a list of up to
-- 256 prepared statements for the array of prepared statement names that
-- you provide. Requires you to have access to the workgroup to which the
-- prepared statements belong. If a prepared statement cannot be retrieved
-- for the name specified, the statement is listed in
-- @UnprocessedPreparedStatementNames@.
module Amazonka.Athena.BatchGetPreparedStatement
  ( -- * Creating a Request
    BatchGetPreparedStatement (..),
    newBatchGetPreparedStatement,

    -- * Request Lenses
    batchGetPreparedStatement_preparedStatementNames,
    batchGetPreparedStatement_workGroup,

    -- * Destructuring the Response
    BatchGetPreparedStatementResponse (..),
    newBatchGetPreparedStatementResponse,

    -- * Response Lenses
    batchGetPreparedStatementResponse_preparedStatements,
    batchGetPreparedStatementResponse_unprocessedPreparedStatementNames,
    batchGetPreparedStatementResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetPreparedStatement' smart constructor.
data BatchGetPreparedStatement = BatchGetPreparedStatement'
  { -- | A list of prepared statement names to return.
    preparedStatementNames :: [Prelude.Text],
    -- | The name of the workgroup to which the prepared statements belong.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetPreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preparedStatementNames', 'batchGetPreparedStatement_preparedStatementNames' - A list of prepared statement names to return.
--
-- 'workGroup', 'batchGetPreparedStatement_workGroup' - The name of the workgroup to which the prepared statements belong.
newBatchGetPreparedStatement ::
  -- | 'workGroup'
  Prelude.Text ->
  BatchGetPreparedStatement
newBatchGetPreparedStatement pWorkGroup_ =
  BatchGetPreparedStatement'
    { preparedStatementNames =
        Prelude.mempty,
      workGroup = pWorkGroup_
    }

-- | A list of prepared statement names to return.
batchGetPreparedStatement_preparedStatementNames :: Lens.Lens' BatchGetPreparedStatement [Prelude.Text]
batchGetPreparedStatement_preparedStatementNames = Lens.lens (\BatchGetPreparedStatement' {preparedStatementNames} -> preparedStatementNames) (\s@BatchGetPreparedStatement' {} a -> s {preparedStatementNames = a} :: BatchGetPreparedStatement) Prelude.. Lens.coerced

-- | The name of the workgroup to which the prepared statements belong.
batchGetPreparedStatement_workGroup :: Lens.Lens' BatchGetPreparedStatement Prelude.Text
batchGetPreparedStatement_workGroup = Lens.lens (\BatchGetPreparedStatement' {workGroup} -> workGroup) (\s@BatchGetPreparedStatement' {} a -> s {workGroup = a} :: BatchGetPreparedStatement)

instance Core.AWSRequest BatchGetPreparedStatement where
  type
    AWSResponse BatchGetPreparedStatement =
      BatchGetPreparedStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetPreparedStatementResponse'
            Prelude.<$> ( x Core..?> "PreparedStatements"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "UnprocessedPreparedStatementNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetPreparedStatement where
  hashWithSalt _salt BatchGetPreparedStatement' {..} =
    _salt `Prelude.hashWithSalt` preparedStatementNames
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData BatchGetPreparedStatement where
  rnf BatchGetPreparedStatement' {..} =
    Prelude.rnf preparedStatementNames
      `Prelude.seq` Prelude.rnf workGroup

instance Core.ToHeaders BatchGetPreparedStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.BatchGetPreparedStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetPreparedStatement where
  toJSON BatchGetPreparedStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PreparedStatementNames"
                  Core..= preparedStatementNames
              ),
            Prelude.Just ("WorkGroup" Core..= workGroup)
          ]
      )

instance Core.ToPath BatchGetPreparedStatement where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetPreparedStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetPreparedStatementResponse' smart constructor.
data BatchGetPreparedStatementResponse = BatchGetPreparedStatementResponse'
  { -- | The list of prepared statements returned.
    preparedStatements :: Prelude.Maybe [PreparedStatement],
    -- | A list of one or more prepared statements that were requested but could
    -- not be returned.
    unprocessedPreparedStatementNames :: Prelude.Maybe [UnprocessedPreparedStatementName],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetPreparedStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preparedStatements', 'batchGetPreparedStatementResponse_preparedStatements' - The list of prepared statements returned.
--
-- 'unprocessedPreparedStatementNames', 'batchGetPreparedStatementResponse_unprocessedPreparedStatementNames' - A list of one or more prepared statements that were requested but could
-- not be returned.
--
-- 'httpStatus', 'batchGetPreparedStatementResponse_httpStatus' - The response's http status code.
newBatchGetPreparedStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetPreparedStatementResponse
newBatchGetPreparedStatementResponse pHttpStatus_ =
  BatchGetPreparedStatementResponse'
    { preparedStatements =
        Prelude.Nothing,
      unprocessedPreparedStatementNames =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of prepared statements returned.
batchGetPreparedStatementResponse_preparedStatements :: Lens.Lens' BatchGetPreparedStatementResponse (Prelude.Maybe [PreparedStatement])
batchGetPreparedStatementResponse_preparedStatements = Lens.lens (\BatchGetPreparedStatementResponse' {preparedStatements} -> preparedStatements) (\s@BatchGetPreparedStatementResponse' {} a -> s {preparedStatements = a} :: BatchGetPreparedStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of one or more prepared statements that were requested but could
-- not be returned.
batchGetPreparedStatementResponse_unprocessedPreparedStatementNames :: Lens.Lens' BatchGetPreparedStatementResponse (Prelude.Maybe [UnprocessedPreparedStatementName])
batchGetPreparedStatementResponse_unprocessedPreparedStatementNames = Lens.lens (\BatchGetPreparedStatementResponse' {unprocessedPreparedStatementNames} -> unprocessedPreparedStatementNames) (\s@BatchGetPreparedStatementResponse' {} a -> s {unprocessedPreparedStatementNames = a} :: BatchGetPreparedStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetPreparedStatementResponse_httpStatus :: Lens.Lens' BatchGetPreparedStatementResponse Prelude.Int
batchGetPreparedStatementResponse_httpStatus = Lens.lens (\BatchGetPreparedStatementResponse' {httpStatus} -> httpStatus) (\s@BatchGetPreparedStatementResponse' {} a -> s {httpStatus = a} :: BatchGetPreparedStatementResponse)

instance
  Prelude.NFData
    BatchGetPreparedStatementResponse
  where
  rnf BatchGetPreparedStatementResponse' {..} =
    Prelude.rnf preparedStatements
      `Prelude.seq` Prelude.rnf unprocessedPreparedStatementNames
      `Prelude.seq` Prelude.rnf httpStatus

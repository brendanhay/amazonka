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
-- Module      : Amazonka.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on
-- the table.
--
-- If you issue a @DescribeTable@ request immediately after a @CreateTable@
-- request, DynamoDB might return a @ResourceNotFoundException@. This is
-- because @DescribeTable@ uses an eventually consistent query, and the
-- metadata for your table might not be available at that moment. Wait for
-- a few seconds, and then try the @DescribeTable@ request again.
module Amazonka.DynamoDB.DescribeTable
  ( -- * Creating a Request
    DescribeTable (..),
    newDescribeTable,

    -- * Request Lenses
    describeTable_tableName,

    -- * Destructuring the Response
    DescribeTableResponse (..),
    newDescribeTableResponse,

    -- * Response Lenses
    describeTableResponse_table,
    describeTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeTable@ operation.
--
-- /See:/ 'newDescribeTable' smart constructor.
data DescribeTable = DescribeTable'
  { -- | The name of the table to describe.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeTable_tableName' - The name of the table to describe.
newDescribeTable ::
  -- | 'tableName'
  Prelude.Text ->
  DescribeTable
newDescribeTable pTableName_ =
  DescribeTable' {tableName = pTableName_}

-- | The name of the table to describe.
describeTable_tableName :: Lens.Lens' DescribeTable Prelude.Text
describeTable_tableName = Lens.lens (\DescribeTable' {tableName} -> tableName) (\s@DescribeTable' {} a -> s {tableName = a} :: DescribeTable)

instance Core.AWSRequest DescribeTable where
  type
    AWSResponse DescribeTable =
      DescribeTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableResponse'
            Prelude.<$> (x Data..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTable where
  hashWithSalt _salt DescribeTable' {..} =
    _salt `Prelude.hashWithSalt` tableName

instance Prelude.NFData DescribeTable where
  rnf DescribeTable' {..} = Prelude.rnf tableName

instance Data.ToHeaders DescribeTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTable where
  toJSON DescribeTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Data..= tableName)]
      )

instance Data.ToPath DescribeTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTable where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DescribeTable@ operation.
--
-- /See:/ 'newDescribeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { -- | The properties of the table.
    table :: Prelude.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'table', 'describeTableResponse_table' - The properties of the table.
--
-- 'httpStatus', 'describeTableResponse_httpStatus' - The response's http status code.
newDescribeTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableResponse
newDescribeTableResponse pHttpStatus_ =
  DescribeTableResponse'
    { table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the table.
describeTableResponse_table :: Lens.Lens' DescribeTableResponse (Prelude.Maybe TableDescription)
describeTableResponse_table = Lens.lens (\DescribeTableResponse' {table} -> table) (\s@DescribeTableResponse' {} a -> s {table = a} :: DescribeTableResponse)

-- | The response's http status code.
describeTableResponse_httpStatus :: Lens.Lens' DescribeTableResponse Prelude.Int
describeTableResponse_httpStatus = Lens.lens (\DescribeTableResponse' {httpStatus} -> httpStatus) (\s@DescribeTableResponse' {} a -> s {httpStatus = a} :: DescribeTableResponse)

instance Prelude.NFData DescribeTableResponse where
  rnf DescribeTableResponse' {..} =
    Prelude.rnf table `Prelude.seq`
      Prelude.rnf httpStatus

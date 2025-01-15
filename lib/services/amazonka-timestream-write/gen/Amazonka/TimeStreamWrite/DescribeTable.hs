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
-- Module      : Amazonka.TimeStreamWrite.DescribeTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the table name, database
-- name, retention duration of the memory store and the magnetic store.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.describe-table.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.DescribeTable
  ( -- * Creating a Request
    DescribeTable (..),
    newDescribeTable,

    -- * Request Lenses
    describeTable_databaseName,
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDescribeTable' smart constructor.
data DescribeTable = DescribeTable'
  { -- | The name of the Timestream database.
    databaseName :: Prelude.Text,
    -- | The name of the Timestream table.
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
-- 'databaseName', 'describeTable_databaseName' - The name of the Timestream database.
--
-- 'tableName', 'describeTable_tableName' - The name of the Timestream table.
newDescribeTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DescribeTable
newDescribeTable pDatabaseName_ pTableName_ =
  DescribeTable'
    { databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The name of the Timestream database.
describeTable_databaseName :: Lens.Lens' DescribeTable Prelude.Text
describeTable_databaseName = Lens.lens (\DescribeTable' {databaseName} -> databaseName) (\s@DescribeTable' {} a -> s {databaseName = a} :: DescribeTable)

-- | The name of the Timestream table.
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
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DescribeTable where
  rnf DescribeTable' {..} =
    Prelude.rnf databaseName `Prelude.seq`
      Prelude.rnf tableName

instance Data.ToHeaders DescribeTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DescribeTable" ::
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
          [ Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath DescribeTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { -- | The Timestream table.
    table :: Prelude.Maybe Table,
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
-- 'table', 'describeTableResponse_table' - The Timestream table.
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

-- | The Timestream table.
describeTableResponse_table :: Lens.Lens' DescribeTableResponse (Prelude.Maybe Table)
describeTableResponse_table = Lens.lens (\DescribeTableResponse' {table} -> table) (\s@DescribeTableResponse' {} a -> s {table = a} :: DescribeTableResponse)

-- | The response's http status code.
describeTableResponse_httpStatus :: Lens.Lens' DescribeTableResponse Prelude.Int
describeTableResponse_httpStatus = Lens.lens (\DescribeTableResponse' {httpStatus} -> httpStatus) (\s@DescribeTableResponse' {} a -> s {httpStatus = a} :: DescribeTableResponse)

instance Prelude.NFData DescribeTableResponse where
  rnf DescribeTableResponse' {..} =
    Prelude.rnf table `Prelude.seq`
      Prelude.rnf httpStatus

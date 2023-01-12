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
-- Module      : Amazonka.TimeStreamWrite.DescribeDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the database, including the database name,
-- time that the database was created, and the total number of tables found
-- within the database.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.describe-db.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.DescribeDatabase
  ( -- * Creating a Request
    DescribeDatabase (..),
    newDescribeDatabase,

    -- * Request Lenses
    describeDatabase_databaseName,

    -- * Destructuring the Response
    DescribeDatabaseResponse (..),
    newDescribeDatabaseResponse,

    -- * Response Lenses
    describeDatabaseResponse_database,
    describeDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDescribeDatabase' smart constructor.
data DescribeDatabase = DescribeDatabase'
  { -- | The name of the Timestream database.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'describeDatabase_databaseName' - The name of the Timestream database.
newDescribeDatabase ::
  -- | 'databaseName'
  Prelude.Text ->
  DescribeDatabase
newDescribeDatabase pDatabaseName_ =
  DescribeDatabase' {databaseName = pDatabaseName_}

-- | The name of the Timestream database.
describeDatabase_databaseName :: Lens.Lens' DescribeDatabase Prelude.Text
describeDatabase_databaseName = Lens.lens (\DescribeDatabase' {databaseName} -> databaseName) (\s@DescribeDatabase' {} a -> s {databaseName = a} :: DescribeDatabase)

instance Core.AWSRequest DescribeDatabase where
  type
    AWSResponse DescribeDatabase =
      DescribeDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatabaseResponse'
            Prelude.<$> (x Data..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatabase where
  hashWithSalt _salt DescribeDatabase' {..} =
    _salt `Prelude.hashWithSalt` databaseName

instance Prelude.NFData DescribeDatabase where
  rnf DescribeDatabase' {..} = Prelude.rnf databaseName

instance Data.ToHeaders DescribeDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DescribeDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDatabase where
  toJSON DescribeDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatabaseName" Data..= databaseName)]
      )

instance Data.ToPath DescribeDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatabaseResponse' smart constructor.
data DescribeDatabaseResponse = DescribeDatabaseResponse'
  { -- | The name of the Timestream table.
    database :: Prelude.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'describeDatabaseResponse_database' - The name of the Timestream table.
--
-- 'httpStatus', 'describeDatabaseResponse_httpStatus' - The response's http status code.
newDescribeDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatabaseResponse
newDescribeDatabaseResponse pHttpStatus_ =
  DescribeDatabaseResponse'
    { database =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Timestream table.
describeDatabaseResponse_database :: Lens.Lens' DescribeDatabaseResponse (Prelude.Maybe Database)
describeDatabaseResponse_database = Lens.lens (\DescribeDatabaseResponse' {database} -> database) (\s@DescribeDatabaseResponse' {} a -> s {database = a} :: DescribeDatabaseResponse)

-- | The response's http status code.
describeDatabaseResponse_httpStatus :: Lens.Lens' DescribeDatabaseResponse Prelude.Int
describeDatabaseResponse_httpStatus = Lens.lens (\DescribeDatabaseResponse' {httpStatus} -> httpStatus) (\s@DescribeDatabaseResponse' {} a -> s {httpStatus = a} :: DescribeDatabaseResponse)

instance Prelude.NFData DescribeDatabaseResponse where
  rnf DescribeDatabaseResponse' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.LakeFormation.GetTableObjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the set of Amazon S3 objects that make up the specified governed
-- table. A transaction ID or timestamp can be specified for time-travel
-- queries.
module Amazonka.LakeFormation.GetTableObjects
  ( -- * Creating a Request
    GetTableObjects (..),
    newGetTableObjects,

    -- * Request Lenses
    getTableObjects_catalogId,
    getTableObjects_maxResults,
    getTableObjects_nextToken,
    getTableObjects_partitionPredicate,
    getTableObjects_queryAsOfTime,
    getTableObjects_transactionId,
    getTableObjects_databaseName,
    getTableObjects_tableName,

    -- * Destructuring the Response
    GetTableObjectsResponse (..),
    newGetTableObjectsResponse,

    -- * Response Lenses
    getTableObjectsResponse_nextToken,
    getTableObjectsResponse_objects,
    getTableObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTableObjects' smart constructor.
data GetTableObjects = GetTableObjects'
  { -- | The catalog containing the governed table. Defaults to the caller’s
    -- account.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Specifies how many values to return in a page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token if this is not the first call to retrieve these
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A predicate to filter the objects returned based on the partition keys
    -- defined in the governed table.
    --
    -- -   The comparison operators supported are: =, >, \<, >=, \<=
    --
    -- -   The logical operators supported are: AND
    --
    -- -   The data types supported are integer, long, date(yyyy-MM-dd),
    --     timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string
    --     and decimal.
    partitionPredicate :: Prelude.Maybe Prelude.Text,
    -- | The time as of when to read the governed table contents. If not set, the
    -- most recent transaction commit time is used. Cannot be specified along
    -- with @TransactionId@.
    queryAsOfTime :: Prelude.Maybe Data.POSIX,
    -- | The transaction ID at which to read the governed table contents. If this
    -- transaction has aborted, an error is returned. If not set, defaults to
    -- the most recent committed transaction. Cannot be specified along with
    -- @QueryAsOfTime@.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The database containing the governed table.
    databaseName :: Prelude.Text,
    -- | The governed table for which to retrieve objects.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getTableObjects_catalogId' - The catalog containing the governed table. Defaults to the caller’s
-- account.
--
-- 'maxResults', 'getTableObjects_maxResults' - Specifies how many values to return in a page.
--
-- 'nextToken', 'getTableObjects_nextToken' - A continuation token if this is not the first call to retrieve these
-- objects.
--
-- 'partitionPredicate', 'getTableObjects_partitionPredicate' - A predicate to filter the objects returned based on the partition keys
-- defined in the governed table.
--
-- -   The comparison operators supported are: =, >, \<, >=, \<=
--
-- -   The logical operators supported are: AND
--
-- -   The data types supported are integer, long, date(yyyy-MM-dd),
--     timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string
--     and decimal.
--
-- 'queryAsOfTime', 'getTableObjects_queryAsOfTime' - The time as of when to read the governed table contents. If not set, the
-- most recent transaction commit time is used. Cannot be specified along
-- with @TransactionId@.
--
-- 'transactionId', 'getTableObjects_transactionId' - The transaction ID at which to read the governed table contents. If this
-- transaction has aborted, an error is returned. If not set, defaults to
-- the most recent committed transaction. Cannot be specified along with
-- @QueryAsOfTime@.
--
-- 'databaseName', 'getTableObjects_databaseName' - The database containing the governed table.
--
-- 'tableName', 'getTableObjects_tableName' - The governed table for which to retrieve objects.
newGetTableObjects ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTableObjects
newGetTableObjects pDatabaseName_ pTableName_ =
  GetTableObjects'
    { catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      partitionPredicate = Prelude.Nothing,
      queryAsOfTime = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The catalog containing the governed table. Defaults to the caller’s
-- account.
getTableObjects_catalogId :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.Text)
getTableObjects_catalogId = Lens.lens (\GetTableObjects' {catalogId} -> catalogId) (\s@GetTableObjects' {} a -> s {catalogId = a} :: GetTableObjects)

-- | Specifies how many values to return in a page.
getTableObjects_maxResults :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.Natural)
getTableObjects_maxResults = Lens.lens (\GetTableObjects' {maxResults} -> maxResults) (\s@GetTableObjects' {} a -> s {maxResults = a} :: GetTableObjects)

-- | A continuation token if this is not the first call to retrieve these
-- objects.
getTableObjects_nextToken :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.Text)
getTableObjects_nextToken = Lens.lens (\GetTableObjects' {nextToken} -> nextToken) (\s@GetTableObjects' {} a -> s {nextToken = a} :: GetTableObjects)

-- | A predicate to filter the objects returned based on the partition keys
-- defined in the governed table.
--
-- -   The comparison operators supported are: =, >, \<, >=, \<=
--
-- -   The logical operators supported are: AND
--
-- -   The data types supported are integer, long, date(yyyy-MM-dd),
--     timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string
--     and decimal.
getTableObjects_partitionPredicate :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.Text)
getTableObjects_partitionPredicate = Lens.lens (\GetTableObjects' {partitionPredicate} -> partitionPredicate) (\s@GetTableObjects' {} a -> s {partitionPredicate = a} :: GetTableObjects)

-- | The time as of when to read the governed table contents. If not set, the
-- most recent transaction commit time is used. Cannot be specified along
-- with @TransactionId@.
getTableObjects_queryAsOfTime :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.UTCTime)
getTableObjects_queryAsOfTime = Lens.lens (\GetTableObjects' {queryAsOfTime} -> queryAsOfTime) (\s@GetTableObjects' {} a -> s {queryAsOfTime = a} :: GetTableObjects) Prelude.. Lens.mapping Data._Time

-- | The transaction ID at which to read the governed table contents. If this
-- transaction has aborted, an error is returned. If not set, defaults to
-- the most recent committed transaction. Cannot be specified along with
-- @QueryAsOfTime@.
getTableObjects_transactionId :: Lens.Lens' GetTableObjects (Prelude.Maybe Prelude.Text)
getTableObjects_transactionId = Lens.lens (\GetTableObjects' {transactionId} -> transactionId) (\s@GetTableObjects' {} a -> s {transactionId = a} :: GetTableObjects)

-- | The database containing the governed table.
getTableObjects_databaseName :: Lens.Lens' GetTableObjects Prelude.Text
getTableObjects_databaseName = Lens.lens (\GetTableObjects' {databaseName} -> databaseName) (\s@GetTableObjects' {} a -> s {databaseName = a} :: GetTableObjects)

-- | The governed table for which to retrieve objects.
getTableObjects_tableName :: Lens.Lens' GetTableObjects Prelude.Text
getTableObjects_tableName = Lens.lens (\GetTableObjects' {tableName} -> tableName) (\s@GetTableObjects' {} a -> s {tableName = a} :: GetTableObjects)

instance Core.AWSRequest GetTableObjects where
  type
    AWSResponse GetTableObjects =
      GetTableObjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableObjectsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Objects" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableObjects where
  hashWithSalt _salt GetTableObjects' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` partitionPredicate
      `Prelude.hashWithSalt` queryAsOfTime
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData GetTableObjects where
  rnf GetTableObjects' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf partitionPredicate `Prelude.seq`
            Prelude.rnf queryAsOfTime `Prelude.seq`
              Prelude.rnf transactionId `Prelude.seq`
                Prelude.rnf databaseName `Prelude.seq`
                  Prelude.rnf tableName

instance Data.ToHeaders GetTableObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTableObjects where
  toJSON GetTableObjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PartitionPredicate" Data..=)
              Prelude.<$> partitionPredicate,
            ("QueryAsOfTime" Data..=) Prelude.<$> queryAsOfTime,
            ("TransactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath GetTableObjects where
  toPath = Prelude.const "/GetTableObjects"

instance Data.ToQuery GetTableObjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableObjectsResponse' smart constructor.
data GetTableObjectsResponse = GetTableObjectsResponse'
  { -- | A continuation token indicating whether additional data is available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects organized by partition keys.
    objects :: Prelude.Maybe [PartitionObjects],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTableObjectsResponse_nextToken' - A continuation token indicating whether additional data is available.
--
-- 'objects', 'getTableObjectsResponse_objects' - A list of objects organized by partition keys.
--
-- 'httpStatus', 'getTableObjectsResponse_httpStatus' - The response's http status code.
newGetTableObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTableObjectsResponse
newGetTableObjectsResponse pHttpStatus_ =
  GetTableObjectsResponse'
    { nextToken =
        Prelude.Nothing,
      objects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token indicating whether additional data is available.
getTableObjectsResponse_nextToken :: Lens.Lens' GetTableObjectsResponse (Prelude.Maybe Prelude.Text)
getTableObjectsResponse_nextToken = Lens.lens (\GetTableObjectsResponse' {nextToken} -> nextToken) (\s@GetTableObjectsResponse' {} a -> s {nextToken = a} :: GetTableObjectsResponse)

-- | A list of objects organized by partition keys.
getTableObjectsResponse_objects :: Lens.Lens' GetTableObjectsResponse (Prelude.Maybe [PartitionObjects])
getTableObjectsResponse_objects = Lens.lens (\GetTableObjectsResponse' {objects} -> objects) (\s@GetTableObjectsResponse' {} a -> s {objects = a} :: GetTableObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTableObjectsResponse_httpStatus :: Lens.Lens' GetTableObjectsResponse Prelude.Int
getTableObjectsResponse_httpStatus = Lens.lens (\GetTableObjectsResponse' {httpStatus} -> httpStatus) (\s@GetTableObjectsResponse' {} a -> s {httpStatus = a} :: GetTableObjectsResponse)

instance Prelude.NFData GetTableObjectsResponse where
  rnf GetTableObjectsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf objects `Prelude.seq`
        Prelude.rnf httpStatus

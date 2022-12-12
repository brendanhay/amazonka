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
-- Module      : Amazonka.LakeFormation.ListTableStorageOptimizers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration of all storage optimizers associated with a
-- specified table.
module Amazonka.LakeFormation.ListTableStorageOptimizers
  ( -- * Creating a Request
    ListTableStorageOptimizers (..),
    newListTableStorageOptimizers,

    -- * Request Lenses
    listTableStorageOptimizers_catalogId,
    listTableStorageOptimizers_maxResults,
    listTableStorageOptimizers_nextToken,
    listTableStorageOptimizers_storageOptimizerType,
    listTableStorageOptimizers_databaseName,
    listTableStorageOptimizers_tableName,

    -- * Destructuring the Response
    ListTableStorageOptimizersResponse (..),
    newListTableStorageOptimizersResponse,

    -- * Response Lenses
    listTableStorageOptimizersResponse_nextToken,
    listTableStorageOptimizersResponse_storageOptimizerList,
    listTableStorageOptimizersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTableStorageOptimizers' smart constructor.
data ListTableStorageOptimizers = ListTableStorageOptimizers'
  { -- | The Catalog ID of the table.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The number of storage optimizers to return on each call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The specific type of storage optimizers to list. The supported value is
    -- @compaction@.
    storageOptimizerType :: Prelude.Maybe OptimizerType,
    -- | Name of the database where the table is present.
    databaseName :: Prelude.Text,
    -- | Name of the table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableStorageOptimizers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'listTableStorageOptimizers_catalogId' - The Catalog ID of the table.
--
-- 'maxResults', 'listTableStorageOptimizers_maxResults' - The number of storage optimizers to return on each call.
--
-- 'nextToken', 'listTableStorageOptimizers_nextToken' - A continuation token, if this is a continuation call.
--
-- 'storageOptimizerType', 'listTableStorageOptimizers_storageOptimizerType' - The specific type of storage optimizers to list. The supported value is
-- @compaction@.
--
-- 'databaseName', 'listTableStorageOptimizers_databaseName' - Name of the database where the table is present.
--
-- 'tableName', 'listTableStorageOptimizers_tableName' - Name of the table.
newListTableStorageOptimizers ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  ListTableStorageOptimizers
newListTableStorageOptimizers
  pDatabaseName_
  pTableName_ =
    ListTableStorageOptimizers'
      { catalogId =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        storageOptimizerType = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_
      }

-- | The Catalog ID of the table.
listTableStorageOptimizers_catalogId :: Lens.Lens' ListTableStorageOptimizers (Prelude.Maybe Prelude.Text)
listTableStorageOptimizers_catalogId = Lens.lens (\ListTableStorageOptimizers' {catalogId} -> catalogId) (\s@ListTableStorageOptimizers' {} a -> s {catalogId = a} :: ListTableStorageOptimizers)

-- | The number of storage optimizers to return on each call.
listTableStorageOptimizers_maxResults :: Lens.Lens' ListTableStorageOptimizers (Prelude.Maybe Prelude.Natural)
listTableStorageOptimizers_maxResults = Lens.lens (\ListTableStorageOptimizers' {maxResults} -> maxResults) (\s@ListTableStorageOptimizers' {} a -> s {maxResults = a} :: ListTableStorageOptimizers)

-- | A continuation token, if this is a continuation call.
listTableStorageOptimizers_nextToken :: Lens.Lens' ListTableStorageOptimizers (Prelude.Maybe Prelude.Text)
listTableStorageOptimizers_nextToken = Lens.lens (\ListTableStorageOptimizers' {nextToken} -> nextToken) (\s@ListTableStorageOptimizers' {} a -> s {nextToken = a} :: ListTableStorageOptimizers)

-- | The specific type of storage optimizers to list. The supported value is
-- @compaction@.
listTableStorageOptimizers_storageOptimizerType :: Lens.Lens' ListTableStorageOptimizers (Prelude.Maybe OptimizerType)
listTableStorageOptimizers_storageOptimizerType = Lens.lens (\ListTableStorageOptimizers' {storageOptimizerType} -> storageOptimizerType) (\s@ListTableStorageOptimizers' {} a -> s {storageOptimizerType = a} :: ListTableStorageOptimizers)

-- | Name of the database where the table is present.
listTableStorageOptimizers_databaseName :: Lens.Lens' ListTableStorageOptimizers Prelude.Text
listTableStorageOptimizers_databaseName = Lens.lens (\ListTableStorageOptimizers' {databaseName} -> databaseName) (\s@ListTableStorageOptimizers' {} a -> s {databaseName = a} :: ListTableStorageOptimizers)

-- | Name of the table.
listTableStorageOptimizers_tableName :: Lens.Lens' ListTableStorageOptimizers Prelude.Text
listTableStorageOptimizers_tableName = Lens.lens (\ListTableStorageOptimizers' {tableName} -> tableName) (\s@ListTableStorageOptimizers' {} a -> s {tableName = a} :: ListTableStorageOptimizers)

instance Core.AWSRequest ListTableStorageOptimizers where
  type
    AWSResponse ListTableStorageOptimizers =
      ListTableStorageOptimizersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableStorageOptimizersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "StorageOptimizerList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTableStorageOptimizers where
  hashWithSalt _salt ListTableStorageOptimizers' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` storageOptimizerType
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData ListTableStorageOptimizers where
  rnf ListTableStorageOptimizers' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storageOptimizerType
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders ListTableStorageOptimizers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTableStorageOptimizers where
  toJSON ListTableStorageOptimizers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StorageOptimizerType" Data..=)
              Prelude.<$> storageOptimizerType,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath ListTableStorageOptimizers where
  toPath = Prelude.const "/ListTableStorageOptimizers"

instance Data.ToQuery ListTableStorageOptimizers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTableStorageOptimizersResponse' smart constructor.
data ListTableStorageOptimizersResponse = ListTableStorageOptimizersResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the storage optimizers associated with a table.
    storageOptimizerList :: Prelude.Maybe [StorageOptimizer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableStorageOptimizersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableStorageOptimizersResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'storageOptimizerList', 'listTableStorageOptimizersResponse_storageOptimizerList' - A list of the storage optimizers associated with a table.
--
-- 'httpStatus', 'listTableStorageOptimizersResponse_httpStatus' - The response's http status code.
newListTableStorageOptimizersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTableStorageOptimizersResponse
newListTableStorageOptimizersResponse pHttpStatus_ =
  ListTableStorageOptimizersResponse'
    { nextToken =
        Prelude.Nothing,
      storageOptimizerList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listTableStorageOptimizersResponse_nextToken :: Lens.Lens' ListTableStorageOptimizersResponse (Prelude.Maybe Prelude.Text)
listTableStorageOptimizersResponse_nextToken = Lens.lens (\ListTableStorageOptimizersResponse' {nextToken} -> nextToken) (\s@ListTableStorageOptimizersResponse' {} a -> s {nextToken = a} :: ListTableStorageOptimizersResponse)

-- | A list of the storage optimizers associated with a table.
listTableStorageOptimizersResponse_storageOptimizerList :: Lens.Lens' ListTableStorageOptimizersResponse (Prelude.Maybe [StorageOptimizer])
listTableStorageOptimizersResponse_storageOptimizerList = Lens.lens (\ListTableStorageOptimizersResponse' {storageOptimizerList} -> storageOptimizerList) (\s@ListTableStorageOptimizersResponse' {} a -> s {storageOptimizerList = a} :: ListTableStorageOptimizersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTableStorageOptimizersResponse_httpStatus :: Lens.Lens' ListTableStorageOptimizersResponse Prelude.Int
listTableStorageOptimizersResponse_httpStatus = Lens.lens (\ListTableStorageOptimizersResponse' {httpStatus} -> httpStatus) (\s@ListTableStorageOptimizersResponse' {} a -> s {httpStatus = a} :: ListTableStorageOptimizersResponse)

instance
  Prelude.NFData
    ListTableStorageOptimizersResponse
  where
  rnf ListTableStorageOptimizersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storageOptimizerList
      `Prelude.seq` Prelude.rnf httpStatus

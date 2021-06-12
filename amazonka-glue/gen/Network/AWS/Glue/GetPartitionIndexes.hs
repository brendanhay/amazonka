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
-- Module      : Network.AWS.Glue.GetPartitionIndexes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the partition indexes associated with a table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitionIndexes
  ( -- * Creating a Request
    GetPartitionIndexes (..),
    newGetPartitionIndexes,

    -- * Request Lenses
    getPartitionIndexes_nextToken,
    getPartitionIndexes_catalogId,
    getPartitionIndexes_databaseName,
    getPartitionIndexes_tableName,

    -- * Destructuring the Response
    GetPartitionIndexesResponse (..),
    newGetPartitionIndexesResponse,

    -- * Response Lenses
    getPartitionIndexesResponse_nextToken,
    getPartitionIndexesResponse_partitionIndexDescriptorList,
    getPartitionIndexesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPartitionIndexes' smart constructor.
data GetPartitionIndexes = GetPartitionIndexes'
  { -- | A continuation token, included if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The catalog ID where the table resides.
    catalogId :: Core.Maybe Core.Text,
    -- | Specifies the name of a database from which you want to retrieve
    -- partition indexes.
    databaseName :: Core.Text,
    -- | Specifies the name of a table for which you want to retrieve the
    -- partition indexes.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPartitionIndexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPartitionIndexes_nextToken' - A continuation token, included if this is a continuation call.
--
-- 'catalogId', 'getPartitionIndexes_catalogId' - The catalog ID where the table resides.
--
-- 'databaseName', 'getPartitionIndexes_databaseName' - Specifies the name of a database from which you want to retrieve
-- partition indexes.
--
-- 'tableName', 'getPartitionIndexes_tableName' - Specifies the name of a table for which you want to retrieve the
-- partition indexes.
newGetPartitionIndexes ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetPartitionIndexes
newGetPartitionIndexes pDatabaseName_ pTableName_ =
  GetPartitionIndexes'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A continuation token, included if this is a continuation call.
getPartitionIndexes_nextToken :: Lens.Lens' GetPartitionIndexes (Core.Maybe Core.Text)
getPartitionIndexes_nextToken = Lens.lens (\GetPartitionIndexes' {nextToken} -> nextToken) (\s@GetPartitionIndexes' {} a -> s {nextToken = a} :: GetPartitionIndexes)

-- | The catalog ID where the table resides.
getPartitionIndexes_catalogId :: Lens.Lens' GetPartitionIndexes (Core.Maybe Core.Text)
getPartitionIndexes_catalogId = Lens.lens (\GetPartitionIndexes' {catalogId} -> catalogId) (\s@GetPartitionIndexes' {} a -> s {catalogId = a} :: GetPartitionIndexes)

-- | Specifies the name of a database from which you want to retrieve
-- partition indexes.
getPartitionIndexes_databaseName :: Lens.Lens' GetPartitionIndexes Core.Text
getPartitionIndexes_databaseName = Lens.lens (\GetPartitionIndexes' {databaseName} -> databaseName) (\s@GetPartitionIndexes' {} a -> s {databaseName = a} :: GetPartitionIndexes)

-- | Specifies the name of a table for which you want to retrieve the
-- partition indexes.
getPartitionIndexes_tableName :: Lens.Lens' GetPartitionIndexes Core.Text
getPartitionIndexes_tableName = Lens.lens (\GetPartitionIndexes' {tableName} -> tableName) (\s@GetPartitionIndexes' {} a -> s {tableName = a} :: GetPartitionIndexes)

instance Core.AWSPager GetPartitionIndexes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getPartitionIndexesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getPartitionIndexesResponse_partitionIndexDescriptorList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getPartitionIndexes_nextToken
          Lens..~ rs
          Lens.^? getPartitionIndexesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetPartitionIndexes where
  type
    AWSResponse GetPartitionIndexes =
      GetPartitionIndexesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPartitionIndexesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PartitionIndexDescriptorList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPartitionIndexes

instance Core.NFData GetPartitionIndexes

instance Core.ToHeaders GetPartitionIndexes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetPartitionIndexes" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPartitionIndexes where
  toJSON GetPartitionIndexes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetPartitionIndexes where
  toPath = Core.const "/"

instance Core.ToQuery GetPartitionIndexes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPartitionIndexesResponse' smart constructor.
data GetPartitionIndexesResponse = GetPartitionIndexesResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of index descriptors.
    partitionIndexDescriptorList :: Core.Maybe [PartitionIndexDescriptor],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPartitionIndexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPartitionIndexesResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'partitionIndexDescriptorList', 'getPartitionIndexesResponse_partitionIndexDescriptorList' - A list of index descriptors.
--
-- 'httpStatus', 'getPartitionIndexesResponse_httpStatus' - The response's http status code.
newGetPartitionIndexesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPartitionIndexesResponse
newGetPartitionIndexesResponse pHttpStatus_ =
  GetPartitionIndexesResponse'
    { nextToken =
        Core.Nothing,
      partitionIndexDescriptorList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
getPartitionIndexesResponse_nextToken :: Lens.Lens' GetPartitionIndexesResponse (Core.Maybe Core.Text)
getPartitionIndexesResponse_nextToken = Lens.lens (\GetPartitionIndexesResponse' {nextToken} -> nextToken) (\s@GetPartitionIndexesResponse' {} a -> s {nextToken = a} :: GetPartitionIndexesResponse)

-- | A list of index descriptors.
getPartitionIndexesResponse_partitionIndexDescriptorList :: Lens.Lens' GetPartitionIndexesResponse (Core.Maybe [PartitionIndexDescriptor])
getPartitionIndexesResponse_partitionIndexDescriptorList = Lens.lens (\GetPartitionIndexesResponse' {partitionIndexDescriptorList} -> partitionIndexDescriptorList) (\s@GetPartitionIndexesResponse' {} a -> s {partitionIndexDescriptorList = a} :: GetPartitionIndexesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPartitionIndexesResponse_httpStatus :: Lens.Lens' GetPartitionIndexesResponse Core.Int
getPartitionIndexesResponse_httpStatus = Lens.lens (\GetPartitionIndexesResponse' {httpStatus} -> httpStatus) (\s@GetPartitionIndexesResponse' {} a -> s {httpStatus = a} :: GetPartitionIndexesResponse)

instance Core.NFData GetPartitionIndexesResponse

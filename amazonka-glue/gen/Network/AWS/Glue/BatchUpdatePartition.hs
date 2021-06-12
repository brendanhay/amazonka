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
-- Module      : Network.AWS.Glue.BatchUpdatePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchUpdatePartition
  ( -- * Creating a Request
    BatchUpdatePartition (..),
    newBatchUpdatePartition,

    -- * Request Lenses
    batchUpdatePartition_catalogId,
    batchUpdatePartition_databaseName,
    batchUpdatePartition_tableName,
    batchUpdatePartition_entries,

    -- * Destructuring the Response
    BatchUpdatePartitionResponse (..),
    newBatchUpdatePartitionResponse,

    -- * Response Lenses
    batchUpdatePartitionResponse_errors,
    batchUpdatePartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchUpdatePartition' smart constructor.
data BatchUpdatePartition = BatchUpdatePartition'
  { -- | The ID of the catalog in which the partition is to be updated.
    -- Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the metadata database in which the partition is to be
    -- updated.
    databaseName :: Core.Text,
    -- | The name of the metadata table in which the partition is to be updated.
    tableName :: Core.Text,
    -- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to
    -- update.
    entries :: Core.NonEmpty BatchUpdatePartitionRequestEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchUpdatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchUpdatePartition_catalogId' - The ID of the catalog in which the partition is to be updated.
-- Currently, this should be the AWS account ID.
--
-- 'databaseName', 'batchUpdatePartition_databaseName' - The name of the metadata database in which the partition is to be
-- updated.
--
-- 'tableName', 'batchUpdatePartition_tableName' - The name of the metadata table in which the partition is to be updated.
--
-- 'entries', 'batchUpdatePartition_entries' - A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to
-- update.
newBatchUpdatePartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'entries'
  Core.NonEmpty BatchUpdatePartitionRequestEntry ->
  BatchUpdatePartition
newBatchUpdatePartition
  pDatabaseName_
  pTableName_
  pEntries_ =
    BatchUpdatePartition'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        entries = Lens._Coerce Lens.# pEntries_
      }

-- | The ID of the catalog in which the partition is to be updated.
-- Currently, this should be the AWS account ID.
batchUpdatePartition_catalogId :: Lens.Lens' BatchUpdatePartition (Core.Maybe Core.Text)
batchUpdatePartition_catalogId = Lens.lens (\BatchUpdatePartition' {catalogId} -> catalogId) (\s@BatchUpdatePartition' {} a -> s {catalogId = a} :: BatchUpdatePartition)

-- | The name of the metadata database in which the partition is to be
-- updated.
batchUpdatePartition_databaseName :: Lens.Lens' BatchUpdatePartition Core.Text
batchUpdatePartition_databaseName = Lens.lens (\BatchUpdatePartition' {databaseName} -> databaseName) (\s@BatchUpdatePartition' {} a -> s {databaseName = a} :: BatchUpdatePartition)

-- | The name of the metadata table in which the partition is to be updated.
batchUpdatePartition_tableName :: Lens.Lens' BatchUpdatePartition Core.Text
batchUpdatePartition_tableName = Lens.lens (\BatchUpdatePartition' {tableName} -> tableName) (\s@BatchUpdatePartition' {} a -> s {tableName = a} :: BatchUpdatePartition)

-- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to
-- update.
batchUpdatePartition_entries :: Lens.Lens' BatchUpdatePartition (Core.NonEmpty BatchUpdatePartitionRequestEntry)
batchUpdatePartition_entries = Lens.lens (\BatchUpdatePartition' {entries} -> entries) (\s@BatchUpdatePartition' {} a -> s {entries = a} :: BatchUpdatePartition) Core.. Lens._Coerce

instance Core.AWSRequest BatchUpdatePartition where
  type
    AWSResponse BatchUpdatePartition =
      BatchUpdatePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdatePartitionResponse'
            Core.<$> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchUpdatePartition

instance Core.NFData BatchUpdatePartition

instance Core.ToHeaders BatchUpdatePartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchUpdatePartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchUpdatePartition where
  toJSON BatchUpdatePartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("Entries" Core..= entries)
          ]
      )

instance Core.ToPath BatchUpdatePartition where
  toPath = Core.const "/"

instance Core.ToQuery BatchUpdatePartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchUpdatePartitionResponse' smart constructor.
data BatchUpdatePartitionResponse = BatchUpdatePartitionResponse'
  { -- | The errors encountered when trying to update the requested partitions. A
    -- list of @BatchUpdatePartitionFailureEntry@ objects.
    errors :: Core.Maybe [BatchUpdatePartitionFailureEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchUpdatePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchUpdatePartitionResponse_errors' - The errors encountered when trying to update the requested partitions. A
-- list of @BatchUpdatePartitionFailureEntry@ objects.
--
-- 'httpStatus', 'batchUpdatePartitionResponse_httpStatus' - The response's http status code.
newBatchUpdatePartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchUpdatePartitionResponse
newBatchUpdatePartitionResponse pHttpStatus_ =
  BatchUpdatePartitionResponse'
    { errors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to update the requested partitions. A
-- list of @BatchUpdatePartitionFailureEntry@ objects.
batchUpdatePartitionResponse_errors :: Lens.Lens' BatchUpdatePartitionResponse (Core.Maybe [BatchUpdatePartitionFailureEntry])
batchUpdatePartitionResponse_errors = Lens.lens (\BatchUpdatePartitionResponse' {errors} -> errors) (\s@BatchUpdatePartitionResponse' {} a -> s {errors = a} :: BatchUpdatePartitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchUpdatePartitionResponse_httpStatus :: Lens.Lens' BatchUpdatePartitionResponse Core.Int
batchUpdatePartitionResponse_httpStatus = Lens.lens (\BatchUpdatePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchUpdatePartitionResponse' {} a -> s {httpStatus = a} :: BatchUpdatePartitionResponse)

instance Core.NFData BatchUpdatePartitionResponse

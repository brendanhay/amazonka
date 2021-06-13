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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchUpdatePartition' smart constructor.
data BatchUpdatePartition = BatchUpdatePartition'
  { -- | The ID of the catalog in which the partition is to be updated.
    -- Currently, this should be the AWS account ID.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the metadata database in which the partition is to be
    -- updated.
    databaseName :: Prelude.Text,
    -- | The name of the metadata table in which the partition is to be updated.
    tableName :: Prelude.Text,
    -- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to
    -- update.
    entries :: Prelude.NonEmpty BatchUpdatePartitionRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'entries'
  Prelude.NonEmpty BatchUpdatePartitionRequestEntry ->
  BatchUpdatePartition
newBatchUpdatePartition
  pDatabaseName_
  pTableName_
  pEntries_ =
    BatchUpdatePartition'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        entries = Lens._Coerce Lens.# pEntries_
      }

-- | The ID of the catalog in which the partition is to be updated.
-- Currently, this should be the AWS account ID.
batchUpdatePartition_catalogId :: Lens.Lens' BatchUpdatePartition (Prelude.Maybe Prelude.Text)
batchUpdatePartition_catalogId = Lens.lens (\BatchUpdatePartition' {catalogId} -> catalogId) (\s@BatchUpdatePartition' {} a -> s {catalogId = a} :: BatchUpdatePartition)

-- | The name of the metadata database in which the partition is to be
-- updated.
batchUpdatePartition_databaseName :: Lens.Lens' BatchUpdatePartition Prelude.Text
batchUpdatePartition_databaseName = Lens.lens (\BatchUpdatePartition' {databaseName} -> databaseName) (\s@BatchUpdatePartition' {} a -> s {databaseName = a} :: BatchUpdatePartition)

-- | The name of the metadata table in which the partition is to be updated.
batchUpdatePartition_tableName :: Lens.Lens' BatchUpdatePartition Prelude.Text
batchUpdatePartition_tableName = Lens.lens (\BatchUpdatePartition' {tableName} -> tableName) (\s@BatchUpdatePartition' {} a -> s {tableName = a} :: BatchUpdatePartition)

-- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to
-- update.
batchUpdatePartition_entries :: Lens.Lens' BatchUpdatePartition (Prelude.NonEmpty BatchUpdatePartitionRequestEntry)
batchUpdatePartition_entries = Lens.lens (\BatchUpdatePartition' {entries} -> entries) (\s@BatchUpdatePartition' {} a -> s {entries = a} :: BatchUpdatePartition) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchUpdatePartition where
  type
    AWSResponse BatchUpdatePartition =
      BatchUpdatePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdatePartitionResponse'
            Prelude.<$> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdatePartition

instance Prelude.NFData BatchUpdatePartition

instance Core.ToHeaders BatchUpdatePartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.BatchUpdatePartition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchUpdatePartition where
  toJSON BatchUpdatePartition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("Entries" Core..= entries)
          ]
      )

instance Core.ToPath BatchUpdatePartition where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchUpdatePartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdatePartitionResponse' smart constructor.
data BatchUpdatePartitionResponse = BatchUpdatePartitionResponse'
  { -- | The errors encountered when trying to update the requested partitions. A
    -- list of @BatchUpdatePartitionFailureEntry@ objects.
    errors :: Prelude.Maybe [BatchUpdatePartitionFailureEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchUpdatePartitionResponse
newBatchUpdatePartitionResponse pHttpStatus_ =
  BatchUpdatePartitionResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to update the requested partitions. A
-- list of @BatchUpdatePartitionFailureEntry@ objects.
batchUpdatePartitionResponse_errors :: Lens.Lens' BatchUpdatePartitionResponse (Prelude.Maybe [BatchUpdatePartitionFailureEntry])
batchUpdatePartitionResponse_errors = Lens.lens (\BatchUpdatePartitionResponse' {errors} -> errors) (\s@BatchUpdatePartitionResponse' {} a -> s {errors = a} :: BatchUpdatePartitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchUpdatePartitionResponse_httpStatus :: Lens.Lens' BatchUpdatePartitionResponse Prelude.Int
batchUpdatePartitionResponse_httpStatus = Lens.lens (\BatchUpdatePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchUpdatePartitionResponse' {} a -> s {httpStatus = a} :: BatchUpdatePartitionResponse)

instance Prelude.NFData BatchUpdatePartitionResponse

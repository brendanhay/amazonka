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
-- Module      : Network.AWS.Glue.CreatePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
module Network.AWS.Glue.CreatePartition
  ( -- * Creating a Request
    CreatePartition (..),
    newCreatePartition,

    -- * Request Lenses
    createPartition_catalogId,
    createPartition_databaseName,
    createPartition_tableName,
    createPartition_partitionInput,

    -- * Destructuring the Response
    CreatePartitionResponse (..),
    newCreatePartitionResponse,

    -- * Response Lenses
    createPartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePartition' smart constructor.
data CreatePartition = CreatePartition'
  { -- | The AWS account ID of the catalog in which the partition is to be
    -- created.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the metadata database in which the partition is to be
    -- created.
    databaseName :: Core.Text,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Core.Text,
    -- | A @PartitionInput@ structure defining the partition to be created.
    partitionInput :: PartitionInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createPartition_catalogId' - The AWS account ID of the catalog in which the partition is to be
-- created.
--
-- 'databaseName', 'createPartition_databaseName' - The name of the metadata database in which the partition is to be
-- created.
--
-- 'tableName', 'createPartition_tableName' - The name of the metadata table in which the partition is to be created.
--
-- 'partitionInput', 'createPartition_partitionInput' - A @PartitionInput@ structure defining the partition to be created.
newCreatePartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'partitionInput'
  PartitionInput ->
  CreatePartition
newCreatePartition
  pDatabaseName_
  pTableName_
  pPartitionInput_ =
    CreatePartition'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionInput = pPartitionInput_
      }

-- | The AWS account ID of the catalog in which the partition is to be
-- created.
createPartition_catalogId :: Lens.Lens' CreatePartition (Core.Maybe Core.Text)
createPartition_catalogId = Lens.lens (\CreatePartition' {catalogId} -> catalogId) (\s@CreatePartition' {} a -> s {catalogId = a} :: CreatePartition)

-- | The name of the metadata database in which the partition is to be
-- created.
createPartition_databaseName :: Lens.Lens' CreatePartition Core.Text
createPartition_databaseName = Lens.lens (\CreatePartition' {databaseName} -> databaseName) (\s@CreatePartition' {} a -> s {databaseName = a} :: CreatePartition)

-- | The name of the metadata table in which the partition is to be created.
createPartition_tableName :: Lens.Lens' CreatePartition Core.Text
createPartition_tableName = Lens.lens (\CreatePartition' {tableName} -> tableName) (\s@CreatePartition' {} a -> s {tableName = a} :: CreatePartition)

-- | A @PartitionInput@ structure defining the partition to be created.
createPartition_partitionInput :: Lens.Lens' CreatePartition PartitionInput
createPartition_partitionInput = Lens.lens (\CreatePartition' {partitionInput} -> partitionInput) (\s@CreatePartition' {} a -> s {partitionInput = a} :: CreatePartition)

instance Core.AWSRequest CreatePartition where
  type
    AWSResponse CreatePartition =
      CreatePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePartitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePartition

instance Core.NFData CreatePartition

instance Core.ToHeaders CreatePartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreatePartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePartition where
  toJSON CreatePartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionInput" Core..= partitionInput)
          ]
      )

instance Core.ToPath CreatePartition where
  toPath = Core.const "/"

instance Core.ToQuery CreatePartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePartitionResponse' smart constructor.
data CreatePartitionResponse = CreatePartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPartitionResponse_httpStatus' - The response's http status code.
newCreatePartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePartitionResponse
newCreatePartitionResponse pHttpStatus_ =
  CreatePartitionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createPartitionResponse_httpStatus :: Lens.Lens' CreatePartitionResponse Core.Int
createPartitionResponse_httpStatus = Lens.lens (\CreatePartitionResponse' {httpStatus} -> httpStatus) (\s@CreatePartitionResponse' {} a -> s {httpStatus = a} :: CreatePartitionResponse)

instance Core.NFData CreatePartitionResponse

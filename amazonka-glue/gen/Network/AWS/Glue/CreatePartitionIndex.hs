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
-- Module      : Network.AWS.Glue.CreatePartitionIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a specified partition index in an existing table.
module Network.AWS.Glue.CreatePartitionIndex
  ( -- * Creating a Request
    CreatePartitionIndex (..),
    newCreatePartitionIndex,

    -- * Request Lenses
    createPartitionIndex_catalogId,
    createPartitionIndex_databaseName,
    createPartitionIndex_tableName,
    createPartitionIndex_partitionIndex,

    -- * Destructuring the Response
    CreatePartitionIndexResponse (..),
    newCreatePartitionIndexResponse,

    -- * Response Lenses
    createPartitionIndexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePartitionIndex' smart constructor.
data CreatePartitionIndex = CreatePartitionIndex'
  { -- | The catalog ID where the table resides.
    catalogId :: Core.Maybe Core.Text,
    -- | Specifies the name of a database in which you want to create a partition
    -- index.
    databaseName :: Core.Text,
    -- | Specifies the name of a table in which you want to create a partition
    -- index.
    tableName :: Core.Text,
    -- | Specifies a @PartitionIndex@ structure to create a partition index in an
    -- existing table.
    partitionIndex :: PartitionIndex
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartitionIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createPartitionIndex_catalogId' - The catalog ID where the table resides.
--
-- 'databaseName', 'createPartitionIndex_databaseName' - Specifies the name of a database in which you want to create a partition
-- index.
--
-- 'tableName', 'createPartitionIndex_tableName' - Specifies the name of a table in which you want to create a partition
-- index.
--
-- 'partitionIndex', 'createPartitionIndex_partitionIndex' - Specifies a @PartitionIndex@ structure to create a partition index in an
-- existing table.
newCreatePartitionIndex ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'partitionIndex'
  PartitionIndex ->
  CreatePartitionIndex
newCreatePartitionIndex
  pDatabaseName_
  pTableName_
  pPartitionIndex_ =
    CreatePartitionIndex'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionIndex = pPartitionIndex_
      }

-- | The catalog ID where the table resides.
createPartitionIndex_catalogId :: Lens.Lens' CreatePartitionIndex (Core.Maybe Core.Text)
createPartitionIndex_catalogId = Lens.lens (\CreatePartitionIndex' {catalogId} -> catalogId) (\s@CreatePartitionIndex' {} a -> s {catalogId = a} :: CreatePartitionIndex)

-- | Specifies the name of a database in which you want to create a partition
-- index.
createPartitionIndex_databaseName :: Lens.Lens' CreatePartitionIndex Core.Text
createPartitionIndex_databaseName = Lens.lens (\CreatePartitionIndex' {databaseName} -> databaseName) (\s@CreatePartitionIndex' {} a -> s {databaseName = a} :: CreatePartitionIndex)

-- | Specifies the name of a table in which you want to create a partition
-- index.
createPartitionIndex_tableName :: Lens.Lens' CreatePartitionIndex Core.Text
createPartitionIndex_tableName = Lens.lens (\CreatePartitionIndex' {tableName} -> tableName) (\s@CreatePartitionIndex' {} a -> s {tableName = a} :: CreatePartitionIndex)

-- | Specifies a @PartitionIndex@ structure to create a partition index in an
-- existing table.
createPartitionIndex_partitionIndex :: Lens.Lens' CreatePartitionIndex PartitionIndex
createPartitionIndex_partitionIndex = Lens.lens (\CreatePartitionIndex' {partitionIndex} -> partitionIndex) (\s@CreatePartitionIndex' {} a -> s {partitionIndex = a} :: CreatePartitionIndex)

instance Core.AWSRequest CreatePartitionIndex where
  type
    AWSResponse CreatePartitionIndex =
      CreatePartitionIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePartitionIndexResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePartitionIndex

instance Core.NFData CreatePartitionIndex

instance Core.ToHeaders CreatePartitionIndex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreatePartitionIndex" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePartitionIndex where
  toJSON CreatePartitionIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionIndex" Core..= partitionIndex)
          ]
      )

instance Core.ToPath CreatePartitionIndex where
  toPath = Core.const "/"

instance Core.ToQuery CreatePartitionIndex where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePartitionIndexResponse' smart constructor.
data CreatePartitionIndexResponse = CreatePartitionIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartitionIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPartitionIndexResponse_httpStatus' - The response's http status code.
newCreatePartitionIndexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePartitionIndexResponse
newCreatePartitionIndexResponse pHttpStatus_ =
  CreatePartitionIndexResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createPartitionIndexResponse_httpStatus :: Lens.Lens' CreatePartitionIndexResponse Core.Int
createPartitionIndexResponse_httpStatus = Lens.lens (\CreatePartitionIndexResponse' {httpStatus} -> httpStatus) (\s@CreatePartitionIndexResponse' {} a -> s {httpStatus = a} :: CreatePartitionIndexResponse)

instance Core.NFData CreatePartitionIndexResponse

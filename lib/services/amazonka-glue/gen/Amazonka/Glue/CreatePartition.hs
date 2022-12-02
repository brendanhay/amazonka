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
-- Module      : Amazonka.Glue.CreatePartition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
module Amazonka.Glue.CreatePartition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePartition' smart constructor.
data CreatePartition = CreatePartition'
  { -- | The Amazon Web Services account ID of the catalog in which the partition
    -- is to be created.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the metadata database in which the partition is to be
    -- created.
    databaseName :: Prelude.Text,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Prelude.Text,
    -- | A @PartitionInput@ structure defining the partition to be created.
    partitionInput :: PartitionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createPartition_catalogId' - The Amazon Web Services account ID of the catalog in which the partition
-- is to be created.
--
-- 'databaseName', 'createPartition_databaseName' - The name of the metadata database in which the partition is to be
-- created.
--
-- 'tableName', 'createPartition_tableName' - The name of the metadata table in which the partition is to be created.
--
-- 'partitionInput', 'createPartition_partitionInput' - A @PartitionInput@ structure defining the partition to be created.
newCreatePartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'partitionInput'
  PartitionInput ->
  CreatePartition
newCreatePartition
  pDatabaseName_
  pTableName_
  pPartitionInput_ =
    CreatePartition'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionInput = pPartitionInput_
      }

-- | The Amazon Web Services account ID of the catalog in which the partition
-- is to be created.
createPartition_catalogId :: Lens.Lens' CreatePartition (Prelude.Maybe Prelude.Text)
createPartition_catalogId = Lens.lens (\CreatePartition' {catalogId} -> catalogId) (\s@CreatePartition' {} a -> s {catalogId = a} :: CreatePartition)

-- | The name of the metadata database in which the partition is to be
-- created.
createPartition_databaseName :: Lens.Lens' CreatePartition Prelude.Text
createPartition_databaseName = Lens.lens (\CreatePartition' {databaseName} -> databaseName) (\s@CreatePartition' {} a -> s {databaseName = a} :: CreatePartition)

-- | The name of the metadata table in which the partition is to be created.
createPartition_tableName :: Lens.Lens' CreatePartition Prelude.Text
createPartition_tableName = Lens.lens (\CreatePartition' {tableName} -> tableName) (\s@CreatePartition' {} a -> s {tableName = a} :: CreatePartition)

-- | A @PartitionInput@ structure defining the partition to be created.
createPartition_partitionInput :: Lens.Lens' CreatePartition PartitionInput
createPartition_partitionInput = Lens.lens (\CreatePartition' {partitionInput} -> partitionInput) (\s@CreatePartition' {} a -> s {partitionInput = a} :: CreatePartition)

instance Core.AWSRequest CreatePartition where
  type
    AWSResponse CreatePartition =
      CreatePartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePartitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePartition where
  hashWithSalt _salt CreatePartition' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` partitionInput

instance Prelude.NFData CreatePartition where
  rnf CreatePartition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionInput

instance Data.ToHeaders CreatePartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreatePartition" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePartition where
  toJSON CreatePartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionInput" Data..= partitionInput)
          ]
      )

instance Data.ToPath CreatePartition where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePartitionResponse' smart constructor.
data CreatePartitionResponse = CreatePartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreatePartitionResponse
newCreatePartitionResponse pHttpStatus_ =
  CreatePartitionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createPartitionResponse_httpStatus :: Lens.Lens' CreatePartitionResponse Prelude.Int
createPartitionResponse_httpStatus = Lens.lens (\CreatePartitionResponse' {httpStatus} -> httpStatus) (\s@CreatePartitionResponse' {} a -> s {httpStatus = a} :: CreatePartitionResponse)

instance Prelude.NFData CreatePartitionResponse where
  rnf CreatePartitionResponse' {..} =
    Prelude.rnf httpStatus

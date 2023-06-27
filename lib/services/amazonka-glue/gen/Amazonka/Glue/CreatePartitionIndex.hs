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
-- Module      : Amazonka.Glue.CreatePartitionIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a specified partition index in an existing table.
module Amazonka.Glue.CreatePartitionIndex
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePartitionIndex' smart constructor.
data CreatePartitionIndex = CreatePartitionIndex'
  { -- | The catalog ID where the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of a database in which you want to create a partition
    -- index.
    databaseName :: Prelude.Text,
    -- | Specifies the name of a table in which you want to create a partition
    -- index.
    tableName :: Prelude.Text,
    -- | Specifies a @PartitionIndex@ structure to create a partition index in an
    -- existing table.
    partitionIndex :: PartitionIndex
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'partitionIndex'
  PartitionIndex ->
  CreatePartitionIndex
newCreatePartitionIndex
  pDatabaseName_
  pTableName_
  pPartitionIndex_ =
    CreatePartitionIndex'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionIndex = pPartitionIndex_
      }

-- | The catalog ID where the table resides.
createPartitionIndex_catalogId :: Lens.Lens' CreatePartitionIndex (Prelude.Maybe Prelude.Text)
createPartitionIndex_catalogId = Lens.lens (\CreatePartitionIndex' {catalogId} -> catalogId) (\s@CreatePartitionIndex' {} a -> s {catalogId = a} :: CreatePartitionIndex)

-- | Specifies the name of a database in which you want to create a partition
-- index.
createPartitionIndex_databaseName :: Lens.Lens' CreatePartitionIndex Prelude.Text
createPartitionIndex_databaseName = Lens.lens (\CreatePartitionIndex' {databaseName} -> databaseName) (\s@CreatePartitionIndex' {} a -> s {databaseName = a} :: CreatePartitionIndex)

-- | Specifies the name of a table in which you want to create a partition
-- index.
createPartitionIndex_tableName :: Lens.Lens' CreatePartitionIndex Prelude.Text
createPartitionIndex_tableName = Lens.lens (\CreatePartitionIndex' {tableName} -> tableName) (\s@CreatePartitionIndex' {} a -> s {tableName = a} :: CreatePartitionIndex)

-- | Specifies a @PartitionIndex@ structure to create a partition index in an
-- existing table.
createPartitionIndex_partitionIndex :: Lens.Lens' CreatePartitionIndex PartitionIndex
createPartitionIndex_partitionIndex = Lens.lens (\CreatePartitionIndex' {partitionIndex} -> partitionIndex) (\s@CreatePartitionIndex' {} a -> s {partitionIndex = a} :: CreatePartitionIndex)

instance Core.AWSRequest CreatePartitionIndex where
  type
    AWSResponse CreatePartitionIndex =
      CreatePartitionIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePartitionIndexResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePartitionIndex where
  hashWithSalt _salt CreatePartitionIndex' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` partitionIndex

instance Prelude.NFData CreatePartitionIndex where
  rnf CreatePartitionIndex' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionIndex

instance Data.ToHeaders CreatePartitionIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CreatePartitionIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePartitionIndex where
  toJSON CreatePartitionIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionIndex" Data..= partitionIndex)
          ]
      )

instance Data.ToPath CreatePartitionIndex where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePartitionIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePartitionIndexResponse' smart constructor.
data CreatePartitionIndexResponse = CreatePartitionIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreatePartitionIndexResponse
newCreatePartitionIndexResponse pHttpStatus_ =
  CreatePartitionIndexResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createPartitionIndexResponse_httpStatus :: Lens.Lens' CreatePartitionIndexResponse Prelude.Int
createPartitionIndexResponse_httpStatus = Lens.lens (\CreatePartitionIndexResponse' {httpStatus} -> httpStatus) (\s@CreatePartitionIndexResponse' {} a -> s {httpStatus = a} :: CreatePartitionIndexResponse)

instance Prelude.NFData CreatePartitionIndexResponse where
  rnf CreatePartitionIndexResponse' {..} =
    Prelude.rnf httpStatus

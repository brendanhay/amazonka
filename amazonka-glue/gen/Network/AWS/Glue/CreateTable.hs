{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.CreateTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table definition in the Data Catalog.
module Network.AWS.Glue.CreateTable
  ( -- * Creating a Request
    CreateTable (..),
    newCreateTable,

    -- * Request Lenses
    createTable_catalogId,
    createTable_partitionIndexes,
    createTable_databaseName,
    createTable_tableInput,

    -- * Destructuring the Response
    CreateTableResponse (..),
    newCreateTableResponse,

    -- * Response Lenses
    createTableResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | The ID of the Data Catalog in which to create the @Table@. If none is
    -- supplied, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of partition indexes, @PartitionIndex@ structures, to create in
    -- the table.
    partitionIndexes :: Prelude.Maybe [PartitionIndex],
    -- | The catalog database in which to create the new table. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The @TableInput@ object that defines the metadata table to create in the
    -- catalog.
    tableInput :: TableInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createTable_catalogId' - The ID of the Data Catalog in which to create the @Table@. If none is
-- supplied, the AWS account ID is used by default.
--
-- 'partitionIndexes', 'createTable_partitionIndexes' - A list of partition indexes, @PartitionIndex@ structures, to create in
-- the table.
--
-- 'databaseName', 'createTable_databaseName' - The catalog database in which to create the new table. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableInput', 'createTable_tableInput' - The @TableInput@ object that defines the metadata table to create in the
-- catalog.
newCreateTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableInput'
  TableInput ->
  CreateTable
newCreateTable pDatabaseName_ pTableInput_ =
  CreateTable'
    { catalogId = Prelude.Nothing,
      partitionIndexes = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableInput = pTableInput_
    }

-- | The ID of the Data Catalog in which to create the @Table@. If none is
-- supplied, the AWS account ID is used by default.
createTable_catalogId :: Lens.Lens' CreateTable (Prelude.Maybe Prelude.Text)
createTable_catalogId = Lens.lens (\CreateTable' {catalogId} -> catalogId) (\s@CreateTable' {} a -> s {catalogId = a} :: CreateTable)

-- | A list of partition indexes, @PartitionIndex@ structures, to create in
-- the table.
createTable_partitionIndexes :: Lens.Lens' CreateTable (Prelude.Maybe [PartitionIndex])
createTable_partitionIndexes = Lens.lens (\CreateTable' {partitionIndexes} -> partitionIndexes) (\s@CreateTable' {} a -> s {partitionIndexes = a} :: CreateTable) Prelude.. Lens.mapping Prelude._Coerce

-- | The catalog database in which to create the new table. For Hive
-- compatibility, this name is entirely lowercase.
createTable_databaseName :: Lens.Lens' CreateTable Prelude.Text
createTable_databaseName = Lens.lens (\CreateTable' {databaseName} -> databaseName) (\s@CreateTable' {} a -> s {databaseName = a} :: CreateTable)

-- | The @TableInput@ object that defines the metadata table to create in the
-- catalog.
createTable_tableInput :: Lens.Lens' CreateTable TableInput
createTable_tableInput = Lens.lens (\CreateTable' {tableInput} -> tableInput) (\s@CreateTable' {} a -> s {tableInput = a} :: CreateTable)

instance Prelude.AWSRequest CreateTable where
  type Rs CreateTable = CreateTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTable

instance Prelude.NFData CreateTable

instance Prelude.ToHeaders CreateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.CreateTable" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("PartitionIndexes" Prelude..=)
              Prelude.<$> partitionIndexes,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableInput" Prelude..= tableInput)
          ]
      )

instance Prelude.ToPath CreateTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTableResponse_httpStatus' - The response's http status code.
newCreateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTableResponse
newCreateTableResponse pHttpStatus_ =
  CreateTableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createTableResponse_httpStatus :: Lens.Lens' CreateTableResponse Prelude.Int
createTableResponse_httpStatus = Lens.lens (\CreateTableResponse' {httpStatus} -> httpStatus) (\s@CreateTableResponse' {} a -> s {httpStatus = a} :: CreateTableResponse)

instance Prelude.NFData CreateTableResponse

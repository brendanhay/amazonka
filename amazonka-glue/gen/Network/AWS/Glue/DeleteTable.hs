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
-- Module      : Network.AWS.Glue.DeleteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a table definition from the Data Catalog.
--
-- After completing this operation, you no longer have access to the table
-- versions and partitions that belong to the deleted table. AWS Glue
-- deletes these \"orphaned\" resources asynchronously in a timely manner,
-- at the discretion of the service.
--
-- To ensure the immediate deletion of all related resources, before
-- calling @DeleteTable@, use @DeleteTableVersion@ or
-- @BatchDeleteTableVersion@, and @DeletePartition@ or
-- @BatchDeletePartition@, to delete any resources that belong to the
-- table.
module Network.AWS.Glue.DeleteTable
  ( -- * Creating a Request
    DeleteTable (..),
    newDeleteTable,

    -- * Request Lenses
    deleteTable_catalogId,
    deleteTable_databaseName,
    deleteTable_name,

    -- * Destructuring the Response
    DeleteTableResponse (..),
    newDeleteTableResponse,

    -- * Response Lenses
    deleteTableResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table to be deleted. For Hive compatibility, this name
    -- is entirely lowercase.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
--
-- 'databaseName', 'deleteTable_databaseName' - The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'name', 'deleteTable_name' - The name of the table to be deleted. For Hive compatibility, this name
-- is entirely lowercase.
newDeleteTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteTable
newDeleteTable pDatabaseName_ pName_ =
  DeleteTable'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
deleteTable_catalogId :: Lens.Lens' DeleteTable (Prelude.Maybe Prelude.Text)
deleteTable_catalogId = Lens.lens (\DeleteTable' {catalogId} -> catalogId) (\s@DeleteTable' {} a -> s {catalogId = a} :: DeleteTable)

-- | The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
deleteTable_databaseName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_databaseName = Lens.lens (\DeleteTable' {databaseName} -> databaseName) (\s@DeleteTable' {} a -> s {databaseName = a} :: DeleteTable)

-- | The name of the table to be deleted. For Hive compatibility, this name
-- is entirely lowercase.
deleteTable_name :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_name = Lens.lens (\DeleteTable' {name} -> name) (\s@DeleteTable' {} a -> s {name = a} :: DeleteTable)

instance Prelude.AWSRequest DeleteTable where
  type Rs DeleteTable = DeleteTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTable

instance Prelude.NFData DeleteTable

instance Prelude.ToHeaders DeleteTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteTable" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath DeleteTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTableResponse_httpStatus' - The response's http status code.
newDeleteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTableResponse
newDeleteTableResponse pHttpStatus_ =
  DeleteTableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTableResponse_httpStatus :: Lens.Lens' DeleteTableResponse Prelude.Int
deleteTableResponse_httpStatus = Lens.lens (\DeleteTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTableResponse' {} a -> s {httpStatus = a} :: DeleteTableResponse)

instance Prelude.NFData DeleteTableResponse

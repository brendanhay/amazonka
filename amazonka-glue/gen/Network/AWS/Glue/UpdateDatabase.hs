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
-- Module      : Network.AWS.Glue.UpdateDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing database definition in a Data Catalog.
module Network.AWS.Glue.UpdateDatabase
  ( -- * Creating a Request
    UpdateDatabase (..),
    newUpdateDatabase,

    -- * Request Lenses
    updateDatabase_catalogId,
    updateDatabase_name,
    updateDatabase_databaseInput,

    -- * Destructuring the Response
    UpdateDatabaseResponse (..),
    newUpdateDatabaseResponse,

    -- * Response Lenses
    updateDatabaseResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { -- | The ID of the Data Catalog in which the metadata database resides. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to update in the catalog. For Hive
    -- compatibility, this is folded to lowercase.
    name :: Prelude.Text,
    -- | A @DatabaseInput@ object specifying the new definition of the metadata
    -- database in the catalog.
    databaseInput :: DatabaseInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateDatabase_catalogId' - The ID of the Data Catalog in which the metadata database resides. If
-- none is provided, the AWS account ID is used by default.
--
-- 'name', 'updateDatabase_name' - The name of the database to update in the catalog. For Hive
-- compatibility, this is folded to lowercase.
--
-- 'databaseInput', 'updateDatabase_databaseInput' - A @DatabaseInput@ object specifying the new definition of the metadata
-- database in the catalog.
newUpdateDatabase ::
  -- | 'name'
  Prelude.Text ->
  -- | 'databaseInput'
  DatabaseInput ->
  UpdateDatabase
newUpdateDatabase pName_ pDatabaseInput_ =
  UpdateDatabase'
    { catalogId = Prelude.Nothing,
      name = pName_,
      databaseInput = pDatabaseInput_
    }

-- | The ID of the Data Catalog in which the metadata database resides. If
-- none is provided, the AWS account ID is used by default.
updateDatabase_catalogId :: Lens.Lens' UpdateDatabase (Prelude.Maybe Prelude.Text)
updateDatabase_catalogId = Lens.lens (\UpdateDatabase' {catalogId} -> catalogId) (\s@UpdateDatabase' {} a -> s {catalogId = a} :: UpdateDatabase)

-- | The name of the database to update in the catalog. For Hive
-- compatibility, this is folded to lowercase.
updateDatabase_name :: Lens.Lens' UpdateDatabase Prelude.Text
updateDatabase_name = Lens.lens (\UpdateDatabase' {name} -> name) (\s@UpdateDatabase' {} a -> s {name = a} :: UpdateDatabase)

-- | A @DatabaseInput@ object specifying the new definition of the metadata
-- database in the catalog.
updateDatabase_databaseInput :: Lens.Lens' UpdateDatabase DatabaseInput
updateDatabase_databaseInput = Lens.lens (\UpdateDatabase' {databaseInput} -> databaseInput) (\s@UpdateDatabase' {} a -> s {databaseInput = a} :: UpdateDatabase)

instance Prelude.AWSRequest UpdateDatabase where
  type Rs UpdateDatabase = UpdateDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatabase

instance Prelude.NFData UpdateDatabase

instance Prelude.ToHeaders UpdateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateDatabase" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDatabase where
  toJSON UpdateDatabase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("DatabaseInput" Prelude..= databaseInput)
          ]
      )

instance Prelude.ToPath UpdateDatabase where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatabaseResponse' smart constructor.
data UpdateDatabaseResponse = UpdateDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatabaseResponse_httpStatus' - The response's http status code.
newUpdateDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatabaseResponse
newUpdateDatabaseResponse pHttpStatus_ =
  UpdateDatabaseResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDatabaseResponse_httpStatus :: Lens.Lens' UpdateDatabaseResponse Prelude.Int
updateDatabaseResponse_httpStatus = Lens.lens (\UpdateDatabaseResponse' {httpStatus} -> httpStatus) (\s@UpdateDatabaseResponse' {} a -> s {httpStatus = a} :: UpdateDatabaseResponse)

instance Prelude.NFData UpdateDatabaseResponse

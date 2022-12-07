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
-- Module      : Amazonka.Glue.UpdateDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing database definition in a Data Catalog.
module Amazonka.Glue.UpdateDatabase
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { -- | The ID of the Data Catalog in which the metadata database resides. If
    -- none is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to update in the catalog. For Hive
    -- compatibility, this is folded to lowercase.
    name :: Prelude.Text,
    -- | A @DatabaseInput@ object specifying the new definition of the metadata
    -- database in the catalog.
    databaseInput :: DatabaseInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateDatabase_catalogId' - The ID of the Data Catalog in which the metadata database resides. If
-- none is provided, the Amazon Web Services account ID is used by default.
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
-- none is provided, the Amazon Web Services account ID is used by default.
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

instance Core.AWSRequest UpdateDatabase where
  type
    AWSResponse UpdateDatabase =
      UpdateDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatabase where
  hashWithSalt _salt UpdateDatabase' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` databaseInput

instance Prelude.NFData UpdateDatabase where
  rnf UpdateDatabase' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf databaseInput

instance Data.ToHeaders UpdateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateDatabase" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDatabase where
  toJSON UpdateDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DatabaseInput" Data..= databaseInput)
          ]
      )

instance Data.ToPath UpdateDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatabaseResponse' smart constructor.
data UpdateDatabaseResponse = UpdateDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateDatabaseResponse where
  rnf UpdateDatabaseResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.Athena.GetDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specified database and data catalog.
module Amazonka.Athena.GetDatabase
  ( -- * Creating a Request
    GetDatabase (..),
    newGetDatabase,

    -- * Request Lenses
    getDatabase_catalogName,
    getDatabase_databaseName,

    -- * Destructuring the Response
    GetDatabaseResponse (..),
    newGetDatabaseResponse,

    -- * Response Lenses
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The name of the data catalog that contains the database to return.
    catalogName :: Prelude.Text,
    -- | The name of the database to return.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogName', 'getDatabase_catalogName' - The name of the data catalog that contains the database to return.
--
-- 'databaseName', 'getDatabase_databaseName' - The name of the database to return.
newGetDatabase ::
  -- | 'catalogName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  GetDatabase
newGetDatabase pCatalogName_ pDatabaseName_ =
  GetDatabase'
    { catalogName = pCatalogName_,
      databaseName = pDatabaseName_
    }

-- | The name of the data catalog that contains the database to return.
getDatabase_catalogName :: Lens.Lens' GetDatabase Prelude.Text
getDatabase_catalogName = Lens.lens (\GetDatabase' {catalogName} -> catalogName) (\s@GetDatabase' {} a -> s {catalogName = a} :: GetDatabase)

-- | The name of the database to return.
getDatabase_databaseName :: Lens.Lens' GetDatabase Prelude.Text
getDatabase_databaseName = Lens.lens (\GetDatabase' {databaseName} -> databaseName) (\s@GetDatabase' {} a -> s {databaseName = a} :: GetDatabase)

instance Core.AWSRequest GetDatabase where
  type AWSResponse GetDatabase = GetDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Prelude.<$> (x Data..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDatabase where
  hashWithSalt _salt GetDatabase' {..} =
    _salt `Prelude.hashWithSalt` catalogName
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData GetDatabase where
  rnf GetDatabase' {..} =
    Prelude.rnf catalogName
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToHeaders GetDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonAthena.GetDatabase" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CatalogName" Data..= catalogName),
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )

instance Data.ToPath GetDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { -- | The database returned.
    database :: Prelude.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'getDatabaseResponse_database' - The database returned.
--
-- 'httpStatus', 'getDatabaseResponse_httpStatus' - The response's http status code.
newGetDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatabaseResponse
newGetDatabaseResponse pHttpStatus_ =
  GetDatabaseResponse'
    { database = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The database returned.
getDatabaseResponse_database :: Lens.Lens' GetDatabaseResponse (Prelude.Maybe Database)
getDatabaseResponse_database = Lens.lens (\GetDatabaseResponse' {database} -> database) (\s@GetDatabaseResponse' {} a -> s {database = a} :: GetDatabaseResponse)

-- | The response's http status code.
getDatabaseResponse_httpStatus :: Lens.Lens' GetDatabaseResponse Prelude.Int
getDatabaseResponse_httpStatus = Lens.lens (\GetDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetDatabaseResponse' {} a -> s {httpStatus = a} :: GetDatabaseResponse)

instance Prelude.NFData GetDatabaseResponse where
  rnf GetDatabaseResponse' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf httpStatus

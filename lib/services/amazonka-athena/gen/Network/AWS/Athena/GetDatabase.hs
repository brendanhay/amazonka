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
-- Module      : Network.AWS.Athena.GetDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specified database and data catalog.
module Network.AWS.Athena.GetDatabase
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

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Prelude.<$> (x Core..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDatabase

instance Prelude.NFData GetDatabase

instance Core.ToHeaders GetDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.GetDatabase" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CatalogName" Core..= catalogName),
            Prelude.Just ("DatabaseName" Core..= databaseName)
          ]
      )

instance Core.ToPath GetDatabase where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDatabase where
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

instance Prelude.NFData GetDatabaseResponse

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
-- Module      : Network.AWS.Glue.GetDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a specified database.
module Network.AWS.Glue.GetDatabase
  ( -- * Creating a Request
    GetDatabase (..),
    newGetDatabase,

    -- * Request Lenses
    getDatabase_catalogId,
    getDatabase_name,

    -- * Destructuring the Response
    GetDatabaseResponse (..),
    newGetDatabaseResponse,

    -- * Response Lenses
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The ID of the Data Catalog in which the database resides. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the database to retrieve. For Hive compatibility, this
    -- should be all lowercase.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getDatabase_catalogId' - The ID of the Data Catalog in which the database resides. If none is
-- provided, the AWS account ID is used by default.
--
-- 'name', 'getDatabase_name' - The name of the database to retrieve. For Hive compatibility, this
-- should be all lowercase.
newGetDatabase ::
  -- | 'name'
  Core.Text ->
  GetDatabase
newGetDatabase pName_ =
  GetDatabase'
    { catalogId = Core.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the database resides. If none is
-- provided, the AWS account ID is used by default.
getDatabase_catalogId :: Lens.Lens' GetDatabase (Core.Maybe Core.Text)
getDatabase_catalogId = Lens.lens (\GetDatabase' {catalogId} -> catalogId) (\s@GetDatabase' {} a -> s {catalogId = a} :: GetDatabase)

-- | The name of the database to retrieve. For Hive compatibility, this
-- should be all lowercase.
getDatabase_name :: Lens.Lens' GetDatabase Core.Text
getDatabase_name = Lens.lens (\GetDatabase' {name} -> name) (\s@GetDatabase' {} a -> s {name = a} :: GetDatabase)

instance Core.AWSRequest GetDatabase where
  type AWSResponse GetDatabase = GetDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Core.<$> (x Core..?> "Database")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDatabase

instance Core.NFData GetDatabase

instance Core.ToHeaders GetDatabase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDatabase" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetDatabase where
  toPath = Core.const "/"

instance Core.ToQuery GetDatabase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { -- | The definition of the specified database in the Data Catalog.
    database :: Core.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'getDatabaseResponse_database' - The definition of the specified database in the Data Catalog.
--
-- 'httpStatus', 'getDatabaseResponse_httpStatus' - The response's http status code.
newGetDatabaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDatabaseResponse
newGetDatabaseResponse pHttpStatus_ =
  GetDatabaseResponse'
    { database = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The definition of the specified database in the Data Catalog.
getDatabaseResponse_database :: Lens.Lens' GetDatabaseResponse (Core.Maybe Database)
getDatabaseResponse_database = Lens.lens (\GetDatabaseResponse' {database} -> database) (\s@GetDatabaseResponse' {} a -> s {database = a} :: GetDatabaseResponse)

-- | The response's http status code.
getDatabaseResponse_httpStatus :: Lens.Lens' GetDatabaseResponse Core.Int
getDatabaseResponse_httpStatus = Lens.lens (\GetDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetDatabaseResponse' {} a -> s {httpStatus = a} :: GetDatabaseResponse)

instance Core.NFData GetDatabaseResponse

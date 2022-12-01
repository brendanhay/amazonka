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
-- Module      : Amazonka.SSMSAP.GetDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the SAP HANA database of an application registered with AWS Systems
-- Manager for SAP.
module Amazonka.SSMSAP.GetDatabase
  ( -- * Creating a Request
    GetDatabase (..),
    newGetDatabase,

    -- * Request Lenses
    getDatabase_databaseId,
    getDatabase_componentId,
    getDatabase_applicationId,
    getDatabase_databaseArn,

    -- * Destructuring the Response
    GetDatabaseResponse (..),
    newGetDatabaseResponse,

    -- * Response Lenses
    getDatabaseResponse_tags,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { databaseId :: Prelude.Maybe Prelude.Text,
    componentId :: Prelude.Maybe Prelude.Text,
    applicationId :: Prelude.Maybe Prelude.Text,
    databaseArn :: Prelude.Maybe Prelude.Text
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
-- 'databaseId', 'getDatabase_databaseId' -
--
-- 'componentId', 'getDatabase_componentId' -
--
-- 'applicationId', 'getDatabase_applicationId' -
--
-- 'databaseArn', 'getDatabase_databaseArn' -
newGetDatabase ::
  GetDatabase
newGetDatabase =
  GetDatabase'
    { databaseId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      databaseArn = Prelude.Nothing
    }

-- |
getDatabase_databaseId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_databaseId = Lens.lens (\GetDatabase' {databaseId} -> databaseId) (\s@GetDatabase' {} a -> s {databaseId = a} :: GetDatabase)

-- |
getDatabase_componentId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_componentId = Lens.lens (\GetDatabase' {componentId} -> componentId) (\s@GetDatabase' {} a -> s {componentId = a} :: GetDatabase)

-- |
getDatabase_applicationId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_applicationId = Lens.lens (\GetDatabase' {applicationId} -> applicationId) (\s@GetDatabase' {} a -> s {applicationId = a} :: GetDatabase)

-- |
getDatabase_databaseArn :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_databaseArn = Lens.lens (\GetDatabase' {databaseArn} -> databaseArn) (\s@GetDatabase' {} a -> s {databaseArn = a} :: GetDatabase)

instance Core.AWSRequest GetDatabase where
  type AWSResponse GetDatabase = GetDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDatabase where
  hashWithSalt _salt GetDatabase' {..} =
    _salt `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` databaseArn

instance Prelude.NFData GetDatabase where
  rnf GetDatabase' {..} =
    Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf databaseArn

instance Core.ToHeaders GetDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DatabaseId" Core..=) Prelude.<$> databaseId,
            ("ComponentId" Core..=) Prelude.<$> componentId,
            ("ApplicationId" Core..=) Prelude.<$> applicationId,
            ("DatabaseArn" Core..=) Prelude.<$> databaseArn
          ]
      )

instance Core.ToPath GetDatabase where
  toPath = Prelude.const "/get-database"

instance Core.ToQuery GetDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    database :: Prelude.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getDatabaseResponse_tags' -
--
-- 'database', 'getDatabaseResponse_database' -
--
-- 'httpStatus', 'getDatabaseResponse_httpStatus' - The response's http status code.
newGetDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatabaseResponse
newGetDatabaseResponse pHttpStatus_ =
  GetDatabaseResponse'
    { tags = Prelude.Nothing,
      database = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
getDatabaseResponse_tags :: Lens.Lens' GetDatabaseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDatabaseResponse_tags = Lens.lens (\GetDatabaseResponse' {tags} -> tags) (\s@GetDatabaseResponse' {} a -> s {tags = a} :: GetDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- |
getDatabaseResponse_database :: Lens.Lens' GetDatabaseResponse (Prelude.Maybe Database)
getDatabaseResponse_database = Lens.lens (\GetDatabaseResponse' {database} -> database) (\s@GetDatabaseResponse' {} a -> s {database = a} :: GetDatabaseResponse)

-- | The response's http status code.
getDatabaseResponse_httpStatus :: Lens.Lens' GetDatabaseResponse Prelude.Int
getDatabaseResponse_httpStatus = Lens.lens (\GetDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetDatabaseResponse' {} a -> s {httpStatus = a} :: GetDatabaseResponse)

instance Prelude.NFData GetDatabaseResponse where
  rnf GetDatabaseResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getDatabase_applicationId,
    getDatabase_componentId,
    getDatabase_databaseArn,
    getDatabase_databaseId,

    -- * Destructuring the Response
    GetDatabaseResponse (..),
    newGetDatabaseResponse,

    -- * Response Lenses
    getDatabaseResponse_database,
    getDatabaseResponse_tags,
    getDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The ID of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the database.
    databaseArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the database.
    databaseId :: Prelude.Maybe Prelude.Text
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
-- 'applicationId', 'getDatabase_applicationId' - The ID of the application.
--
-- 'componentId', 'getDatabase_componentId' - The ID of the component.
--
-- 'databaseArn', 'getDatabase_databaseArn' - The Amazon Resource Name (ARN) of the database.
--
-- 'databaseId', 'getDatabase_databaseId' - The ID of the database.
newGetDatabase ::
  GetDatabase
newGetDatabase =
  GetDatabase'
    { applicationId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      databaseArn = Prelude.Nothing,
      databaseId = Prelude.Nothing
    }

-- | The ID of the application.
getDatabase_applicationId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_applicationId = Lens.lens (\GetDatabase' {applicationId} -> applicationId) (\s@GetDatabase' {} a -> s {applicationId = a} :: GetDatabase)

-- | The ID of the component.
getDatabase_componentId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_componentId = Lens.lens (\GetDatabase' {componentId} -> componentId) (\s@GetDatabase' {} a -> s {componentId = a} :: GetDatabase)

-- | The Amazon Resource Name (ARN) of the database.
getDatabase_databaseArn :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_databaseArn = Lens.lens (\GetDatabase' {databaseArn} -> databaseArn) (\s@GetDatabase' {} a -> s {databaseArn = a} :: GetDatabase)

-- | The ID of the database.
getDatabase_databaseId :: Lens.Lens' GetDatabase (Prelude.Maybe Prelude.Text)
getDatabase_databaseId = Lens.lens (\GetDatabase' {databaseId} -> databaseId) (\s@GetDatabase' {} a -> s {databaseId = a} :: GetDatabase)

instance Core.AWSRequest GetDatabase where
  type AWSResponse GetDatabase = GetDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Prelude.<$> (x Data..?> "Database")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDatabase where
  hashWithSalt _salt GetDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` databaseArn
      `Prelude.hashWithSalt` databaseId

instance Prelude.NFData GetDatabase where
  rnf GetDatabase' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf databaseArn
      `Prelude.seq` Prelude.rnf databaseId

instance Data.ToHeaders GetDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationId" Data..=) Prelude.<$> applicationId,
            ("ComponentId" Data..=) Prelude.<$> componentId,
            ("DatabaseArn" Data..=) Prelude.<$> databaseArn,
            ("DatabaseId" Data..=) Prelude.<$> databaseId
          ]
      )

instance Data.ToPath GetDatabase where
  toPath = Prelude.const "/get-database"

instance Data.ToQuery GetDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { -- | The SAP HANA database of an application registered with AWS Systems
    -- Manager for SAP.
    database :: Prelude.Maybe Database,
    -- | The tags of a database.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'database', 'getDatabaseResponse_database' - The SAP HANA database of an application registered with AWS Systems
-- Manager for SAP.
--
-- 'tags', 'getDatabaseResponse_tags' - The tags of a database.
--
-- 'httpStatus', 'getDatabaseResponse_httpStatus' - The response's http status code.
newGetDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatabaseResponse
newGetDatabaseResponse pHttpStatus_ =
  GetDatabaseResponse'
    { database = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The SAP HANA database of an application registered with AWS Systems
-- Manager for SAP.
getDatabaseResponse_database :: Lens.Lens' GetDatabaseResponse (Prelude.Maybe Database)
getDatabaseResponse_database = Lens.lens (\GetDatabaseResponse' {database} -> database) (\s@GetDatabaseResponse' {} a -> s {database = a} :: GetDatabaseResponse)

-- | The tags of a database.
getDatabaseResponse_tags :: Lens.Lens' GetDatabaseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDatabaseResponse_tags = Lens.lens (\GetDatabaseResponse' {tags} -> tags) (\s@GetDatabaseResponse' {} a -> s {tags = a} :: GetDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDatabaseResponse_httpStatus :: Lens.Lens' GetDatabaseResponse Prelude.Int
getDatabaseResponse_httpStatus = Lens.lens (\GetDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetDatabaseResponse' {} a -> s {httpStatus = a} :: GetDatabaseResponse)

instance Prelude.NFData GetDatabaseResponse where
  rnf GetDatabaseResponse' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

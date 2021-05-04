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
-- Module      : Network.AWS.Glue.CreateDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in a Data Catalog.
module Network.AWS.Glue.CreateDatabase
  ( -- * Creating a Request
    CreateDatabase (..),
    newCreateDatabase,

    -- * Request Lenses
    createDatabase_catalogId,
    createDatabase_databaseInput,

    -- * Destructuring the Response
    CreateDatabaseResponse (..),
    newCreateDatabaseResponse,

    -- * Response Lenses
    createDatabaseResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { -- | The ID of the Data Catalog in which to create the database. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The metadata for the database.
    databaseInput :: DatabaseInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createDatabase_catalogId' - The ID of the Data Catalog in which to create the database. If none is
-- provided, the AWS account ID is used by default.
--
-- 'databaseInput', 'createDatabase_databaseInput' - The metadata for the database.
newCreateDatabase ::
  -- | 'databaseInput'
  DatabaseInput ->
  CreateDatabase
newCreateDatabase pDatabaseInput_ =
  CreateDatabase'
    { catalogId = Prelude.Nothing,
      databaseInput = pDatabaseInput_
    }

-- | The ID of the Data Catalog in which to create the database. If none is
-- provided, the AWS account ID is used by default.
createDatabase_catalogId :: Lens.Lens' CreateDatabase (Prelude.Maybe Prelude.Text)
createDatabase_catalogId = Lens.lens (\CreateDatabase' {catalogId} -> catalogId) (\s@CreateDatabase' {} a -> s {catalogId = a} :: CreateDatabase)

-- | The metadata for the database.
createDatabase_databaseInput :: Lens.Lens' CreateDatabase DatabaseInput
createDatabase_databaseInput = Lens.lens (\CreateDatabase' {databaseInput} -> databaseInput) (\s@CreateDatabase' {} a -> s {databaseInput = a} :: CreateDatabase)

instance Prelude.AWSRequest CreateDatabase where
  type Rs CreateDatabase = CreateDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatabase

instance Prelude.NFData CreateDatabase

instance Prelude.ToHeaders CreateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.CreateDatabase" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateDatabase where
  toJSON CreateDatabase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseInput" Prelude..= databaseInput)
          ]
      )

instance Prelude.ToPath CreateDatabase where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatabaseResponse' smart constructor.
data CreateDatabaseResponse = CreateDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDatabaseResponse_httpStatus' - The response's http status code.
newCreateDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatabaseResponse
newCreateDatabaseResponse pHttpStatus_ =
  CreateDatabaseResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createDatabaseResponse_httpStatus :: Lens.Lens' CreateDatabaseResponse Prelude.Int
createDatabaseResponse_httpStatus = Lens.lens (\CreateDatabaseResponse' {httpStatus} -> httpStatus) (\s@CreateDatabaseResponse' {} a -> s {httpStatus = a} :: CreateDatabaseResponse)

instance Prelude.NFData CreateDatabaseResponse

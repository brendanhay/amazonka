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
-- Module      : Amazonka.Glue.CreateDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in a Data Catalog.
module Amazonka.Glue.CreateDatabase
  ( -- * Creating a Request
    CreateDatabase (..),
    newCreateDatabase,

    -- * Request Lenses
    createDatabase_catalogId,
    createDatabase_tags,
    createDatabase_databaseInput,

    -- * Destructuring the Response
    CreateDatabaseResponse (..),
    newCreateDatabaseResponse,

    -- * Response Lenses
    createDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { -- | The ID of the Data Catalog in which to create the database. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The tags you assign to the database.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The metadata for the database.
    databaseInput :: DatabaseInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createDatabase_catalogId' - The ID of the Data Catalog in which to create the database. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'tags', 'createDatabase_tags' - The tags you assign to the database.
--
-- 'databaseInput', 'createDatabase_databaseInput' - The metadata for the database.
newCreateDatabase ::
  -- | 'databaseInput'
  DatabaseInput ->
  CreateDatabase
newCreateDatabase pDatabaseInput_ =
  CreateDatabase'
    { catalogId = Prelude.Nothing,
      tags = Prelude.Nothing,
      databaseInput = pDatabaseInput_
    }

-- | The ID of the Data Catalog in which to create the database. If none is
-- provided, the Amazon Web Services account ID is used by default.
createDatabase_catalogId :: Lens.Lens' CreateDatabase (Prelude.Maybe Prelude.Text)
createDatabase_catalogId = Lens.lens (\CreateDatabase' {catalogId} -> catalogId) (\s@CreateDatabase' {} a -> s {catalogId = a} :: CreateDatabase)

-- | The tags you assign to the database.
createDatabase_tags :: Lens.Lens' CreateDatabase (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDatabase_tags = Lens.lens (\CreateDatabase' {tags} -> tags) (\s@CreateDatabase' {} a -> s {tags = a} :: CreateDatabase) Prelude.. Lens.mapping Lens.coerced

-- | The metadata for the database.
createDatabase_databaseInput :: Lens.Lens' CreateDatabase DatabaseInput
createDatabase_databaseInput = Lens.lens (\CreateDatabase' {databaseInput} -> databaseInput) (\s@CreateDatabase' {} a -> s {databaseInput = a} :: CreateDatabase)

instance Core.AWSRequest CreateDatabase where
  type
    AWSResponse CreateDatabase =
      CreateDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatabase where
  hashWithSalt _salt CreateDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` databaseInput

instance Prelude.NFData CreateDatabase where
  rnf CreateDatabase' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf databaseInput

instance Data.ToHeaders CreateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateDatabase" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatabase where
  toJSON CreateDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("DatabaseInput" Data..= databaseInput)
          ]
      )

instance Data.ToPath CreateDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatabaseResponse' smart constructor.
data CreateDatabaseResponse = CreateDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateDatabaseResponse where
  rnf CreateDatabaseResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.Lightsail.GetRelationalDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database in Amazon Lightsail.
module Amazonka.Lightsail.GetRelationalDatabase
  ( -- * Creating a Request
    GetRelationalDatabase (..),
    newGetRelationalDatabase,

    -- * Request Lenses
    getRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    GetRelationalDatabaseResponse (..),
    newGetRelationalDatabaseResponse,

    -- * Response Lenses
    getRelationalDatabaseResponse_relationalDatabase,
    getRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabase' smart constructor.
data GetRelationalDatabase = GetRelationalDatabase'
  { -- | The name of the database that you are looking up.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseName', 'getRelationalDatabase_relationalDatabaseName' - The name of the database that you are looking up.
newGetRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  GetRelationalDatabase
newGetRelationalDatabase pRelationalDatabaseName_ =
  GetRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of the database that you are looking up.
getRelationalDatabase_relationalDatabaseName :: Lens.Lens' GetRelationalDatabase Prelude.Text
getRelationalDatabase_relationalDatabaseName = Lens.lens (\GetRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabase)

instance Core.AWSRequest GetRelationalDatabase where
  type
    AWSResponse GetRelationalDatabase =
      GetRelationalDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseResponse'
            Prelude.<$> (x Core..?> "relationalDatabase")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRelationalDatabase where
  hashWithSalt _salt GetRelationalDatabase' {..} =
    _salt `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData GetRelationalDatabase where
  rnf GetRelationalDatabase' {..} =
    Prelude.rnf relationalDatabaseName

instance Core.ToHeaders GetRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabase where
  toJSON GetRelationalDatabase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath GetRelationalDatabase where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseResponse' smart constructor.
data GetRelationalDatabaseResponse = GetRelationalDatabaseResponse'
  { -- | An object describing the specified database.
    relationalDatabase :: Prelude.Maybe RelationalDatabase,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabase', 'getRelationalDatabaseResponse_relationalDatabase' - An object describing the specified database.
--
-- 'httpStatus', 'getRelationalDatabaseResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseResponse
newGetRelationalDatabaseResponse pHttpStatus_ =
  GetRelationalDatabaseResponse'
    { relationalDatabase =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object describing the specified database.
getRelationalDatabaseResponse_relationalDatabase :: Lens.Lens' GetRelationalDatabaseResponse (Prelude.Maybe RelationalDatabase)
getRelationalDatabaseResponse_relationalDatabase = Lens.lens (\GetRelationalDatabaseResponse' {relationalDatabase} -> relationalDatabase) (\s@GetRelationalDatabaseResponse' {} a -> s {relationalDatabase = a} :: GetRelationalDatabaseResponse)

-- | The response's http status code.
getRelationalDatabaseResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseResponse Prelude.Int
getRelationalDatabaseResponse_httpStatus = Lens.lens (\GetRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseResponse)

instance Prelude.NFData GetRelationalDatabaseResponse where
  rnf GetRelationalDatabaseResponse' {..} =
    Prelude.rnf relationalDatabase
      `Prelude.seq` Prelude.rnf httpStatus

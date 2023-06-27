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
-- Module      : Amazonka.FinSpace.UpdateKxDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information for the given kdb database.
module Amazonka.FinSpace.UpdateKxDatabase
  ( -- * Creating a Request
    UpdateKxDatabase (..),
    newUpdateKxDatabase,

    -- * Request Lenses
    updateKxDatabase_description,
    updateKxDatabase_environmentId,
    updateKxDatabase_databaseName,
    updateKxDatabase_clientToken,

    -- * Destructuring the Response
    UpdateKxDatabaseResponse (..),
    newUpdateKxDatabaseResponse,

    -- * Response Lenses
    updateKxDatabaseResponse_databaseName,
    updateKxDatabaseResponse_description,
    updateKxDatabaseResponse_environmentId,
    updateKxDatabaseResponse_lastModifiedTimestamp,
    updateKxDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKxDatabase' smart constructor.
data UpdateKxDatabase = UpdateKxDatabase'
  { -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateKxDatabase_description' - A description of the database.
--
-- 'environmentId', 'updateKxDatabase_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'updateKxDatabase_databaseName' - The name of the kdb database.
--
-- 'clientToken', 'updateKxDatabase_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
newUpdateKxDatabase ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateKxDatabase
newUpdateKxDatabase
  pEnvironmentId_
  pDatabaseName_
  pClientToken_ =
    UpdateKxDatabase'
      { description = Prelude.Nothing,
        environmentId = pEnvironmentId_,
        databaseName = pDatabaseName_,
        clientToken = pClientToken_
      }

-- | A description of the database.
updateKxDatabase_description :: Lens.Lens' UpdateKxDatabase (Prelude.Maybe Prelude.Text)
updateKxDatabase_description = Lens.lens (\UpdateKxDatabase' {description} -> description) (\s@UpdateKxDatabase' {} a -> s {description = a} :: UpdateKxDatabase)

-- | A unique identifier for the kdb environment.
updateKxDatabase_environmentId :: Lens.Lens' UpdateKxDatabase Prelude.Text
updateKxDatabase_environmentId = Lens.lens (\UpdateKxDatabase' {environmentId} -> environmentId) (\s@UpdateKxDatabase' {} a -> s {environmentId = a} :: UpdateKxDatabase)

-- | The name of the kdb database.
updateKxDatabase_databaseName :: Lens.Lens' UpdateKxDatabase Prelude.Text
updateKxDatabase_databaseName = Lens.lens (\UpdateKxDatabase' {databaseName} -> databaseName) (\s@UpdateKxDatabase' {} a -> s {databaseName = a} :: UpdateKxDatabase)

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateKxDatabase_clientToken :: Lens.Lens' UpdateKxDatabase Prelude.Text
updateKxDatabase_clientToken = Lens.lens (\UpdateKxDatabase' {clientToken} -> clientToken) (\s@UpdateKxDatabase' {} a -> s {clientToken = a} :: UpdateKxDatabase)

instance Core.AWSRequest UpdateKxDatabase where
  type
    AWSResponse UpdateKxDatabase =
      UpdateKxDatabaseResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKxDatabaseResponse'
            Prelude.<$> (x Data..?> "databaseName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateKxDatabase where
  hashWithSalt _salt UpdateKxDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateKxDatabase where
  rnf UpdateKxDatabase' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders UpdateKxDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKxDatabase where
  toJSON UpdateKxDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath UpdateKxDatabase where
  toPath UpdateKxDatabase' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName
      ]

instance Data.ToQuery UpdateKxDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKxDatabaseResponse' smart constructor.
data UpdateKxDatabaseResponse = UpdateKxDatabaseResponse'
  { -- | The name of the kdb database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The last time that the database was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'updateKxDatabaseResponse_databaseName' - The name of the kdb database.
--
-- 'description', 'updateKxDatabaseResponse_description' - A description of the database.
--
-- 'environmentId', 'updateKxDatabaseResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'lastModifiedTimestamp', 'updateKxDatabaseResponse_lastModifiedTimestamp' - The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'httpStatus', 'updateKxDatabaseResponse_httpStatus' - The response's http status code.
newUpdateKxDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKxDatabaseResponse
newUpdateKxDatabaseResponse pHttpStatus_ =
  UpdateKxDatabaseResponse'
    { databaseName =
        Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the kdb database.
updateKxDatabaseResponse_databaseName :: Lens.Lens' UpdateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
updateKxDatabaseResponse_databaseName = Lens.lens (\UpdateKxDatabaseResponse' {databaseName} -> databaseName) (\s@UpdateKxDatabaseResponse' {} a -> s {databaseName = a} :: UpdateKxDatabaseResponse)

-- | A description of the database.
updateKxDatabaseResponse_description :: Lens.Lens' UpdateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
updateKxDatabaseResponse_description = Lens.lens (\UpdateKxDatabaseResponse' {description} -> description) (\s@UpdateKxDatabaseResponse' {} a -> s {description = a} :: UpdateKxDatabaseResponse)

-- | A unique identifier for the kdb environment.
updateKxDatabaseResponse_environmentId :: Lens.Lens' UpdateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
updateKxDatabaseResponse_environmentId = Lens.lens (\UpdateKxDatabaseResponse' {environmentId} -> environmentId) (\s@UpdateKxDatabaseResponse' {} a -> s {environmentId = a} :: UpdateKxDatabaseResponse)

-- | The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
updateKxDatabaseResponse_lastModifiedTimestamp :: Lens.Lens' UpdateKxDatabaseResponse (Prelude.Maybe Prelude.UTCTime)
updateKxDatabaseResponse_lastModifiedTimestamp = Lens.lens (\UpdateKxDatabaseResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@UpdateKxDatabaseResponse' {} a -> s {lastModifiedTimestamp = a} :: UpdateKxDatabaseResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateKxDatabaseResponse_httpStatus :: Lens.Lens' UpdateKxDatabaseResponse Prelude.Int
updateKxDatabaseResponse_httpStatus = Lens.lens (\UpdateKxDatabaseResponse' {httpStatus} -> httpStatus) (\s@UpdateKxDatabaseResponse' {} a -> s {httpStatus = a} :: UpdateKxDatabaseResponse)

instance Prelude.NFData UpdateKxDatabaseResponse where
  rnf UpdateKxDatabaseResponse' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf httpStatus

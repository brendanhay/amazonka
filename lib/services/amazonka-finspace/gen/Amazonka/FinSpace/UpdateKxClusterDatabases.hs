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
-- Module      : Amazonka.FinSpace.UpdateKxClusterDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the databases mounted on a kdb cluster, which includes the
-- @changesetId@ and all the dbPaths to be cached. This API does not allow
-- you to change a database name or add a database if you created a cluster
-- without one.
--
-- Using this API you can point a cluster to a different changeset and
-- modify a list of partitions being cached.
module Amazonka.FinSpace.UpdateKxClusterDatabases
  ( -- * Creating a Request
    UpdateKxClusterDatabases (..),
    newUpdateKxClusterDatabases,

    -- * Request Lenses
    updateKxClusterDatabases_clientToken,
    updateKxClusterDatabases_environmentId,
    updateKxClusterDatabases_clusterName,
    updateKxClusterDatabases_databases,

    -- * Destructuring the Response
    UpdateKxClusterDatabasesResponse (..),
    newUpdateKxClusterDatabasesResponse,

    -- * Response Lenses
    updateKxClusterDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKxClusterDatabases' smart constructor.
data UpdateKxClusterDatabases = UpdateKxClusterDatabases'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of a kdb environment.
    environmentId :: Prelude.Text,
    -- | A unique name for the cluster that you want to modify.
    clusterName :: Prelude.Text,
    -- | The structure of databases mounted on the cluster.
    databases :: [KxDatabaseConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxClusterDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateKxClusterDatabases_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'environmentId', 'updateKxClusterDatabases_environmentId' - The unique identifier of a kdb environment.
--
-- 'clusterName', 'updateKxClusterDatabases_clusterName' - A unique name for the cluster that you want to modify.
--
-- 'databases', 'updateKxClusterDatabases_databases' - The structure of databases mounted on the cluster.
newUpdateKxClusterDatabases ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  UpdateKxClusterDatabases
newUpdateKxClusterDatabases
  pEnvironmentId_
  pClusterName_ =
    UpdateKxClusterDatabases'
      { clientToken =
          Prelude.Nothing,
        environmentId = pEnvironmentId_,
        clusterName = pClusterName_,
        databases = Prelude.mempty
      }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateKxClusterDatabases_clientToken :: Lens.Lens' UpdateKxClusterDatabases (Prelude.Maybe Prelude.Text)
updateKxClusterDatabases_clientToken = Lens.lens (\UpdateKxClusterDatabases' {clientToken} -> clientToken) (\s@UpdateKxClusterDatabases' {} a -> s {clientToken = a} :: UpdateKxClusterDatabases)

-- | The unique identifier of a kdb environment.
updateKxClusterDatabases_environmentId :: Lens.Lens' UpdateKxClusterDatabases Prelude.Text
updateKxClusterDatabases_environmentId = Lens.lens (\UpdateKxClusterDatabases' {environmentId} -> environmentId) (\s@UpdateKxClusterDatabases' {} a -> s {environmentId = a} :: UpdateKxClusterDatabases)

-- | A unique name for the cluster that you want to modify.
updateKxClusterDatabases_clusterName :: Lens.Lens' UpdateKxClusterDatabases Prelude.Text
updateKxClusterDatabases_clusterName = Lens.lens (\UpdateKxClusterDatabases' {clusterName} -> clusterName) (\s@UpdateKxClusterDatabases' {} a -> s {clusterName = a} :: UpdateKxClusterDatabases)

-- | The structure of databases mounted on the cluster.
updateKxClusterDatabases_databases :: Lens.Lens' UpdateKxClusterDatabases [KxDatabaseConfiguration]
updateKxClusterDatabases_databases = Lens.lens (\UpdateKxClusterDatabases' {databases} -> databases) (\s@UpdateKxClusterDatabases' {} a -> s {databases = a} :: UpdateKxClusterDatabases) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateKxClusterDatabases where
  type
    AWSResponse UpdateKxClusterDatabases =
      UpdateKxClusterDatabasesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateKxClusterDatabasesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateKxClusterDatabases where
  hashWithSalt _salt UpdateKxClusterDatabases' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` databases

instance Prelude.NFData UpdateKxClusterDatabases where
  rnf UpdateKxClusterDatabases' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf databases

instance Data.ToHeaders UpdateKxClusterDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKxClusterDatabases where
  toJSON UpdateKxClusterDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("databases" Data..= databases)
          ]
      )

instance Data.ToPath UpdateKxClusterDatabases where
  toPath UpdateKxClusterDatabases' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters/",
        Data.toBS clusterName,
        "/configuration/databases"
      ]

instance Data.ToQuery UpdateKxClusterDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKxClusterDatabasesResponse' smart constructor.
data UpdateKxClusterDatabasesResponse = UpdateKxClusterDatabasesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxClusterDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateKxClusterDatabasesResponse_httpStatus' - The response's http status code.
newUpdateKxClusterDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKxClusterDatabasesResponse
newUpdateKxClusterDatabasesResponse pHttpStatus_ =
  UpdateKxClusterDatabasesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateKxClusterDatabasesResponse_httpStatus :: Lens.Lens' UpdateKxClusterDatabasesResponse Prelude.Int
updateKxClusterDatabasesResponse_httpStatus = Lens.lens (\UpdateKxClusterDatabasesResponse' {httpStatus} -> httpStatus) (\s@UpdateKxClusterDatabasesResponse' {} a -> s {httpStatus = a} :: UpdateKxClusterDatabasesResponse)

instance
  Prelude.NFData
    UpdateKxClusterDatabasesResponse
  where
  rnf UpdateKxClusterDatabasesResponse' {..} =
    Prelude.rnf httpStatus

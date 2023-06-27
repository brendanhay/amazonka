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
-- Module      : Amazonka.FinSpace.DeleteKxCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a kdb cluster.
module Amazonka.FinSpace.DeleteKxCluster
  ( -- * Creating a Request
    DeleteKxCluster (..),
    newDeleteKxCluster,

    -- * Request Lenses
    deleteKxCluster_clientToken,
    deleteKxCluster_environmentId,
    deleteKxCluster_clusterName,

    -- * Destructuring the Response
    DeleteKxClusterResponse (..),
    newDeleteKxClusterResponse,

    -- * Response Lenses
    deleteKxClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKxCluster' smart constructor.
data DeleteKxCluster = DeleteKxCluster'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the cluster that you want to delete.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteKxCluster_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'environmentId', 'deleteKxCluster_environmentId' - A unique identifier for the kdb environment.
--
-- 'clusterName', 'deleteKxCluster_clusterName' - The name of the cluster that you want to delete.
newDeleteKxCluster ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  DeleteKxCluster
newDeleteKxCluster pEnvironmentId_ pClusterName_ =
  DeleteKxCluster'
    { clientToken = Prelude.Nothing,
      environmentId = pEnvironmentId_,
      clusterName = pClusterName_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
deleteKxCluster_clientToken :: Lens.Lens' DeleteKxCluster (Prelude.Maybe Prelude.Text)
deleteKxCluster_clientToken = Lens.lens (\DeleteKxCluster' {clientToken} -> clientToken) (\s@DeleteKxCluster' {} a -> s {clientToken = a} :: DeleteKxCluster)

-- | A unique identifier for the kdb environment.
deleteKxCluster_environmentId :: Lens.Lens' DeleteKxCluster Prelude.Text
deleteKxCluster_environmentId = Lens.lens (\DeleteKxCluster' {environmentId} -> environmentId) (\s@DeleteKxCluster' {} a -> s {environmentId = a} :: DeleteKxCluster)

-- | The name of the cluster that you want to delete.
deleteKxCluster_clusterName :: Lens.Lens' DeleteKxCluster Prelude.Text
deleteKxCluster_clusterName = Lens.lens (\DeleteKxCluster' {clusterName} -> clusterName) (\s@DeleteKxCluster' {} a -> s {clusterName = a} :: DeleteKxCluster)

instance Core.AWSRequest DeleteKxCluster where
  type
    AWSResponse DeleteKxCluster =
      DeleteKxClusterResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKxClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKxCluster where
  hashWithSalt _salt DeleteKxCluster' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData DeleteKxCluster where
  rnf DeleteKxCluster' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders DeleteKxCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteKxCluster where
  toPath DeleteKxCluster' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters/",
        Data.toBS clusterName
      ]

instance Data.ToQuery DeleteKxCluster where
  toQuery DeleteKxCluster' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteKxClusterResponse' smart constructor.
data DeleteKxClusterResponse = DeleteKxClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKxClusterResponse_httpStatus' - The response's http status code.
newDeleteKxClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKxClusterResponse
newDeleteKxClusterResponse pHttpStatus_ =
  DeleteKxClusterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteKxClusterResponse_httpStatus :: Lens.Lens' DeleteKxClusterResponse Prelude.Int
deleteKxClusterResponse_httpStatus = Lens.lens (\DeleteKxClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteKxClusterResponse' {} a -> s {httpStatus = a} :: DeleteKxClusterResponse)

instance Prelude.NFData DeleteKxClusterResponse where
  rnf DeleteKxClusterResponse' {..} =
    Prelude.rnf httpStatus

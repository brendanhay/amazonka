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
-- Module      : Amazonka.EKS.UpdateClusterVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EKS cluster to the specified Kubernetes version. Your
-- cluster continues to function during the update. The response output
-- includes an update ID that you can use to track the status of your
-- cluster update with the DescribeUpdate API operation.
--
-- Cluster updates are asynchronous, and they should finish within a few
-- minutes. During an update, the cluster status moves to @UPDATING@ (this
-- status transition is eventually consistent). When the update is complete
-- (either @Failed@ or @Successful@), the cluster status moves to @Active@.
--
-- If your cluster has managed node groups attached to it, all of your node
-- groups’ Kubernetes versions must match the cluster’s Kubernetes version
-- in order to update the cluster to a new Kubernetes version.
module Amazonka.EKS.UpdateClusterVersion
  ( -- * Creating a Request
    UpdateClusterVersion (..),
    newUpdateClusterVersion,

    -- * Request Lenses
    updateClusterVersion_clientRequestToken,
    updateClusterVersion_name,
    updateClusterVersion_version,

    -- * Destructuring the Response
    UpdateClusterVersionResponse (..),
    newUpdateClusterVersionResponse,

    -- * Response Lenses
    updateClusterVersionResponse_update,
    updateClusterVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClusterVersion' smart constructor.
data UpdateClusterVersion = UpdateClusterVersion'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EKS cluster to update.
    name :: Prelude.Text,
    -- | The desired Kubernetes version following a successful update.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateClusterVersion_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'name', 'updateClusterVersion_name' - The name of the Amazon EKS cluster to update.
--
-- 'version', 'updateClusterVersion_version' - The desired Kubernetes version following a successful update.
newUpdateClusterVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  UpdateClusterVersion
newUpdateClusterVersion pName_ pVersion_ =
  UpdateClusterVersion'
    { clientRequestToken =
        Prelude.Nothing,
      name = pName_,
      version = pVersion_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateClusterVersion_clientRequestToken :: Lens.Lens' UpdateClusterVersion (Prelude.Maybe Prelude.Text)
updateClusterVersion_clientRequestToken = Lens.lens (\UpdateClusterVersion' {clientRequestToken} -> clientRequestToken) (\s@UpdateClusterVersion' {} a -> s {clientRequestToken = a} :: UpdateClusterVersion)

-- | The name of the Amazon EKS cluster to update.
updateClusterVersion_name :: Lens.Lens' UpdateClusterVersion Prelude.Text
updateClusterVersion_name = Lens.lens (\UpdateClusterVersion' {name} -> name) (\s@UpdateClusterVersion' {} a -> s {name = a} :: UpdateClusterVersion)

-- | The desired Kubernetes version following a successful update.
updateClusterVersion_version :: Lens.Lens' UpdateClusterVersion Prelude.Text
updateClusterVersion_version = Lens.lens (\UpdateClusterVersion' {version} -> version) (\s@UpdateClusterVersion' {} a -> s {version = a} :: UpdateClusterVersion)

instance Core.AWSRequest UpdateClusterVersion where
  type
    AWSResponse UpdateClusterVersion =
      UpdateClusterVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterVersionResponse'
            Prelude.<$> (x Core..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClusterVersion where
  hashWithSalt _salt UpdateClusterVersion' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData UpdateClusterVersion where
  rnf UpdateClusterVersion' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version

instance Core.ToHeaders UpdateClusterVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateClusterVersion where
  toJSON UpdateClusterVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("version" Core..= version)
          ]
      )

instance Core.ToPath UpdateClusterVersion where
  toPath UpdateClusterVersion' {..} =
    Prelude.mconcat
      ["/clusters/", Core.toBS name, "/updates"]

instance Core.ToQuery UpdateClusterVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterVersionResponse' smart constructor.
data UpdateClusterVersionResponse = UpdateClusterVersionResponse'
  { -- | The full description of the specified update
    update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'updateClusterVersionResponse_update' - The full description of the specified update
--
-- 'httpStatus', 'updateClusterVersionResponse_httpStatus' - The response's http status code.
newUpdateClusterVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterVersionResponse
newUpdateClusterVersionResponse pHttpStatus_ =
  UpdateClusterVersionResponse'
    { update =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the specified update
updateClusterVersionResponse_update :: Lens.Lens' UpdateClusterVersionResponse (Prelude.Maybe Update)
updateClusterVersionResponse_update = Lens.lens (\UpdateClusterVersionResponse' {update} -> update) (\s@UpdateClusterVersionResponse' {} a -> s {update = a} :: UpdateClusterVersionResponse)

-- | The response's http status code.
updateClusterVersionResponse_httpStatus :: Lens.Lens' UpdateClusterVersionResponse Prelude.Int
updateClusterVersionResponse_httpStatus = Lens.lens (\UpdateClusterVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterVersionResponse' {} a -> s {httpStatus = a} :: UpdateClusterVersionResponse)

instance Prelude.NFData UpdateClusterVersionResponse where
  rnf UpdateClusterVersionResponse' {..} =
    Prelude.rnf update
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.ECS.DeleteCapacityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified capacity provider.
--
-- The @FARGATE@ and @FARGATE_SPOT@ capacity providers are reserved and
-- can\'t be deleted. You can disassociate them from a cluster using either
-- the PutClusterCapacityProviders API or by deleting the cluster.
--
-- Prior to a capacity provider being deleted, the capacity provider must
-- be removed from the capacity provider strategy from all services. The
-- UpdateService API can be used to remove a capacity provider from a
-- service\'s capacity provider strategy. When updating a service, the
-- @forceNewDeployment@ option can be used to ensure that any tasks using
-- the Amazon EC2 instance capacity provided by the capacity provider are
-- transitioned to use the capacity from the remaining capacity providers.
-- Only capacity providers that aren\'t associated with a cluster can be
-- deleted. To remove a capacity provider from a cluster, you can either
-- use PutClusterCapacityProviders or delete the cluster.
module Amazonka.ECS.DeleteCapacityProvider
  ( -- * Creating a Request
    DeleteCapacityProvider (..),
    newDeleteCapacityProvider,

    -- * Request Lenses
    deleteCapacityProvider_capacityProvider,

    -- * Destructuring the Response
    DeleteCapacityProviderResponse (..),
    newDeleteCapacityProviderResponse,

    -- * Response Lenses
    deleteCapacityProviderResponse_capacityProvider,
    deleteCapacityProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCapacityProvider' smart constructor.
data DeleteCapacityProvider = DeleteCapacityProvider'
  { -- | The short name or full Amazon Resource Name (ARN) of the capacity
    -- provider to delete.
    capacityProvider :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvider', 'deleteCapacityProvider_capacityProvider' - The short name or full Amazon Resource Name (ARN) of the capacity
-- provider to delete.
newDeleteCapacityProvider ::
  -- | 'capacityProvider'
  Prelude.Text ->
  DeleteCapacityProvider
newDeleteCapacityProvider pCapacityProvider_ =
  DeleteCapacityProvider'
    { capacityProvider =
        pCapacityProvider_
    }

-- | The short name or full Amazon Resource Name (ARN) of the capacity
-- provider to delete.
deleteCapacityProvider_capacityProvider :: Lens.Lens' DeleteCapacityProvider Prelude.Text
deleteCapacityProvider_capacityProvider = Lens.lens (\DeleteCapacityProvider' {capacityProvider} -> capacityProvider) (\s@DeleteCapacityProvider' {} a -> s {capacityProvider = a} :: DeleteCapacityProvider)

instance Core.AWSRequest DeleteCapacityProvider where
  type
    AWSResponse DeleteCapacityProvider =
      DeleteCapacityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCapacityProviderResponse'
            Prelude.<$> (x Core..?> "capacityProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCapacityProvider where
  hashWithSalt _salt DeleteCapacityProvider' {..} =
    _salt `Prelude.hashWithSalt` capacityProvider

instance Prelude.NFData DeleteCapacityProvider where
  rnf DeleteCapacityProvider' {..} =
    Prelude.rnf capacityProvider

instance Core.ToHeaders DeleteCapacityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteCapacityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCapacityProvider where
  toJSON DeleteCapacityProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("capacityProvider" Core..= capacityProvider)
          ]
      )

instance Core.ToPath DeleteCapacityProvider where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCapacityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCapacityProviderResponse' smart constructor.
data DeleteCapacityProviderResponse = DeleteCapacityProviderResponse'
  { -- | The details of the capacity provider.
    capacityProvider :: Prelude.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCapacityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvider', 'deleteCapacityProviderResponse_capacityProvider' - The details of the capacity provider.
--
-- 'httpStatus', 'deleteCapacityProviderResponse_httpStatus' - The response's http status code.
newDeleteCapacityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCapacityProviderResponse
newDeleteCapacityProviderResponse pHttpStatus_ =
  DeleteCapacityProviderResponse'
    { capacityProvider =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the capacity provider.
deleteCapacityProviderResponse_capacityProvider :: Lens.Lens' DeleteCapacityProviderResponse (Prelude.Maybe CapacityProvider)
deleteCapacityProviderResponse_capacityProvider = Lens.lens (\DeleteCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@DeleteCapacityProviderResponse' {} a -> s {capacityProvider = a} :: DeleteCapacityProviderResponse)

-- | The response's http status code.
deleteCapacityProviderResponse_httpStatus :: Lens.Lens' DeleteCapacityProviderResponse Prelude.Int
deleteCapacityProviderResponse_httpStatus = Lens.lens (\DeleteCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@DeleteCapacityProviderResponse' {} a -> s {httpStatus = a} :: DeleteCapacityProviderResponse)

instance
  Prelude.NFData
    DeleteCapacityProviderResponse
  where
  rnf DeleteCapacityProviderResponse' {..} =
    Prelude.rnf capacityProvider
      `Prelude.seq` Prelude.rnf httpStatus

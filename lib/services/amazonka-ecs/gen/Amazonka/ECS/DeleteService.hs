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
-- Module      : Amazonka.ECS.DeleteService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster. You can delete a service
-- if you have no running tasks in it and the desired task count is zero.
-- If the service is actively maintaining tasks, you can\'t delete it, and
-- you must update the service to a desired task count of zero. For more
-- information, see UpdateService.
--
-- When you delete a service, if there are still running tasks that require
-- cleanup, the service status moves from @ACTIVE@ to @DRAINING@, and the
-- service is no longer visible in the console or in the ListServices API
-- operation. After all tasks have transitioned to either @STOPPING@ or
-- @STOPPED@ status, the service status moves from @DRAINING@ to
-- @INACTIVE@. Services in the @DRAINING@ or @INACTIVE@ status can still be
-- viewed with the DescribeServices API operation. However, in the future,
-- @INACTIVE@ services may be cleaned up and purged from Amazon ECS record
-- keeping, and DescribeServices calls on those services return a
-- @ServiceNotFoundException@ error.
--
-- If you attempt to create a new service with the same name as an existing
-- service in either @ACTIVE@ or @DRAINING@ status, you receive an error.
module Amazonka.ECS.DeleteService
  ( -- * Creating a Request
    DeleteService (..),
    newDeleteService,

    -- * Request Lenses
    deleteService_cluster,
    deleteService_force,
    deleteService_service,

    -- * Destructuring the Response
    DeleteServiceResponse (..),
    newDeleteServiceResponse,

    -- * Response Lenses
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service to delete. If you do not specify a cluster, the
    -- default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | If @true@, allows you to delete a service even if it wasn\'t scaled down
    -- to zero tasks. It\'s only necessary to use this if the service uses the
    -- @REPLICA@ scheduling strategy.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the service to delete.
    service :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to delete. If you do not specify a cluster, the
-- default cluster is assumed.
--
-- 'force', 'deleteService_force' - If @true@, allows you to delete a service even if it wasn\'t scaled down
-- to zero tasks. It\'s only necessary to use this if the service uses the
-- @REPLICA@ scheduling strategy.
--
-- 'service', 'deleteService_service' - The name of the service to delete.
newDeleteService ::
  -- | 'service'
  Prelude.Text ->
  DeleteService
newDeleteService pService_ =
  DeleteService'
    { cluster = Prelude.Nothing,
      force = Prelude.Nothing,
      service = pService_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to delete. If you do not specify a cluster, the
-- default cluster is assumed.
deleteService_cluster :: Lens.Lens' DeleteService (Prelude.Maybe Prelude.Text)
deleteService_cluster = Lens.lens (\DeleteService' {cluster} -> cluster) (\s@DeleteService' {} a -> s {cluster = a} :: DeleteService)

-- | If @true@, allows you to delete a service even if it wasn\'t scaled down
-- to zero tasks. It\'s only necessary to use this if the service uses the
-- @REPLICA@ scheduling strategy.
deleteService_force :: Lens.Lens' DeleteService (Prelude.Maybe Prelude.Bool)
deleteService_force = Lens.lens (\DeleteService' {force} -> force) (\s@DeleteService' {} a -> s {force = a} :: DeleteService)

-- | The name of the service to delete.
deleteService_service :: Lens.Lens' DeleteService Prelude.Text
deleteService_service = Lens.lens (\DeleteService' {service} -> service) (\s@DeleteService' {} a -> s {service = a} :: DeleteService)

instance Core.AWSRequest DeleteService where
  type
    AWSResponse DeleteService =
      DeleteServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceResponse'
            Prelude.<$> (x Data..?> "service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteService where
  hashWithSalt _salt DeleteService' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` service

instance Prelude.NFData DeleteService where
  rnf DeleteService' {..} =
    Prelude.rnf cluster `Prelude.seq`
      Prelude.rnf force `Prelude.seq`
        Prelude.rnf service

instance Data.ToHeaders DeleteService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DeleteService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteService where
  toJSON DeleteService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            ("force" Data..=) Prelude.<$> force,
            Prelude.Just ("service" Data..= service)
          ]
      )

instance Data.ToPath DeleteService where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The full description of the deleted service.
    service :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'deleteServiceResponse_service' - The full description of the deleted service.
--
-- 'httpStatus', 'deleteServiceResponse_httpStatus' - The response's http status code.
newDeleteServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceResponse
newDeleteServiceResponse pHttpStatus_ =
  DeleteServiceResponse'
    { service = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the deleted service.
deleteServiceResponse_service :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe ContainerService)
deleteServiceResponse_service = Lens.lens (\DeleteServiceResponse' {service} -> service) (\s@DeleteServiceResponse' {} a -> s {service = a} :: DeleteServiceResponse)

-- | The response's http status code.
deleteServiceResponse_httpStatus :: Lens.Lens' DeleteServiceResponse Prelude.Int
deleteServiceResponse_httpStatus = Lens.lens (\DeleteServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceResponse' {} a -> s {httpStatus = a} :: DeleteServiceResponse)

instance Prelude.NFData DeleteServiceResponse where
  rnf DeleteServiceResponse' {..} =
    Prelude.rnf service `Prelude.seq`
      Prelude.rnf httpStatus

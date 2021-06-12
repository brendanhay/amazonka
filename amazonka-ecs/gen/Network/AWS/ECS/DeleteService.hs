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
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster. You can delete a service
-- if you have no running tasks in it and the desired task count is zero.
-- If the service is actively maintaining tasks, you cannot delete it, and
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
module Network.AWS.ECS.DeleteService
  ( -- * Creating a Request
    DeleteService (..),
    newDeleteService,

    -- * Request Lenses
    deleteService_force,
    deleteService_cluster,
    deleteService_service,

    -- * Destructuring the Response
    DeleteServiceResponse (..),
    newDeleteServiceResponse,

    -- * Response Lenses
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | If @true@, allows you to delete a service even if it has not been scaled
    -- down to zero tasks. It is only necessary to use this if the service is
    -- using the @REPLICA@ scheduling strategy.
    force :: Core.Maybe Core.Bool,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service to delete. If you do not specify a cluster, the
    -- default cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | The name of the service to delete.
    service :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteService_force' - If @true@, allows you to delete a service even if it has not been scaled
-- down to zero tasks. It is only necessary to use this if the service is
-- using the @REPLICA@ scheduling strategy.
--
-- 'cluster', 'deleteService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to delete. If you do not specify a cluster, the
-- default cluster is assumed.
--
-- 'service', 'deleteService_service' - The name of the service to delete.
newDeleteService ::
  -- | 'service'
  Core.Text ->
  DeleteService
newDeleteService pService_ =
  DeleteService'
    { force = Core.Nothing,
      cluster = Core.Nothing,
      service = pService_
    }

-- | If @true@, allows you to delete a service even if it has not been scaled
-- down to zero tasks. It is only necessary to use this if the service is
-- using the @REPLICA@ scheduling strategy.
deleteService_force :: Lens.Lens' DeleteService (Core.Maybe Core.Bool)
deleteService_force = Lens.lens (\DeleteService' {force} -> force) (\s@DeleteService' {} a -> s {force = a} :: DeleteService)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to delete. If you do not specify a cluster, the
-- default cluster is assumed.
deleteService_cluster :: Lens.Lens' DeleteService (Core.Maybe Core.Text)
deleteService_cluster = Lens.lens (\DeleteService' {cluster} -> cluster) (\s@DeleteService' {} a -> s {cluster = a} :: DeleteService)

-- | The name of the service to delete.
deleteService_service :: Lens.Lens' DeleteService Core.Text
deleteService_service = Lens.lens (\DeleteService' {service} -> service) (\s@DeleteService' {} a -> s {service = a} :: DeleteService)

instance Core.AWSRequest DeleteService where
  type
    AWSResponse DeleteService =
      DeleteServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceResponse'
            Core.<$> (x Core..?> "service")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteService

instance Core.NFData DeleteService

instance Core.ToHeaders DeleteService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteService where
  toJSON DeleteService' {..} =
    Core.object
      ( Core.catMaybes
          [ ("force" Core..=) Core.<$> force,
            ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("service" Core..= service)
          ]
      )

instance Core.ToPath DeleteService where
  toPath = Core.const "/"

instance Core.ToQuery DeleteService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The full description of the deleted service.
    service :: Core.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteServiceResponse
newDeleteServiceResponse pHttpStatus_ =
  DeleteServiceResponse'
    { service = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the deleted service.
deleteServiceResponse_service :: Lens.Lens' DeleteServiceResponse (Core.Maybe ContainerService)
deleteServiceResponse_service = Lens.lens (\DeleteServiceResponse' {service} -> service) (\s@DeleteServiceResponse' {} a -> s {service = a} :: DeleteServiceResponse)

-- | The response's http status code.
deleteServiceResponse_httpStatus :: Lens.Lens' DeleteServiceResponse Core.Int
deleteServiceResponse_httpStatus = Lens.lens (\DeleteServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceResponse' {} a -> s {httpStatus = a} :: DeleteServiceResponse)

instance Core.NFData DeleteServiceResponse

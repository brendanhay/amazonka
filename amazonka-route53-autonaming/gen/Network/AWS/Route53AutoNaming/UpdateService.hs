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
-- Module      : Network.AWS.Route53AutoNaming.UpdateService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to perform the following operations:
--
-- -   Update the TTL setting for existing @DnsRecords@ configurations
--
-- -   Add, update, or delete @HealthCheckConfig@ for a specified service
--
--     You can\'t add, update, or delete a @HealthCheckCustomConfig@
--     configuration.
--
-- For public and private DNS namespaces, note the following:
--
-- -   If you omit any existing @DnsRecords@ or @HealthCheckConfig@
--     configurations from an @UpdateService@ request, the configurations
--     are deleted from the service.
--
-- -   If you omit an existing @HealthCheckCustomConfig@ configuration from
--     an @UpdateService@ request, the configuration is not deleted from
--     the service.
--
-- When you update settings for a service, AWS Cloud Map also updates the
-- corresponding settings in all the records and health checks that were
-- created by using the specified service.
module Network.AWS.Route53AutoNaming.UpdateService
  ( -- * Creating a Request
    UpdateService (..),
    newUpdateService,

    -- * Request Lenses
    updateService_id,
    updateService_service,

    -- * Destructuring the Response
    UpdateServiceResponse (..),
    newUpdateServiceResponse,

    -- * Response Lenses
    updateServiceResponse_operationId,
    updateServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | The ID of the service that you want to update.
    id :: Core.Text,
    -- | A complex type that contains the new settings for the service.
    service :: ServiceChange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateService_id' - The ID of the service that you want to update.
--
-- 'service', 'updateService_service' - A complex type that contains the new settings for the service.
newUpdateService ::
  -- | 'id'
  Core.Text ->
  -- | 'service'
  ServiceChange ->
  UpdateService
newUpdateService pId_ pService_ =
  UpdateService' {id = pId_, service = pService_}

-- | The ID of the service that you want to update.
updateService_id :: Lens.Lens' UpdateService Core.Text
updateService_id = Lens.lens (\UpdateService' {id} -> id) (\s@UpdateService' {} a -> s {id = a} :: UpdateService)

-- | A complex type that contains the new settings for the service.
updateService_service :: Lens.Lens' UpdateService ServiceChange
updateService_service = Lens.lens (\UpdateService' {service} -> service) (\s@UpdateService' {} a -> s {service = a} :: UpdateService)

instance Core.AWSRequest UpdateService where
  type
    AWSResponse UpdateService =
      UpdateServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Core.<$> (x Core..?> "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateService

instance Core.NFData UpdateService

instance Core.ToHeaders UpdateService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.UpdateService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            Core.Just ("Service" Core..= service)
          ]
      )

instance Core.ToPath UpdateService where
  toPath = Core.const "/"

instance Core.ToQuery UpdateService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateServiceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'updateServiceResponse_httpStatus' - The response's http status code.
newUpdateServiceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateServiceResponse
newUpdateServiceResponse pHttpStatus_ =
  UpdateServiceResponse'
    { operationId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
updateServiceResponse_operationId :: Lens.Lens' UpdateServiceResponse (Core.Maybe Core.Text)
updateServiceResponse_operationId = Lens.lens (\UpdateServiceResponse' {operationId} -> operationId) (\s@UpdateServiceResponse' {} a -> s {operationId = a} :: UpdateServiceResponse)

-- | The response's http status code.
updateServiceResponse_httpStatus :: Lens.Lens' UpdateServiceResponse Core.Int
updateServiceResponse_httpStatus = Lens.lens (\UpdateServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceResponse' {} a -> s {httpStatus = a} :: UpdateServiceResponse)

instance Core.NFData UpdateServiceResponse

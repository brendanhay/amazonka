{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to change the health status of a custom health check
-- to healthy or unhealthy.
--
-- You can use @UpdateInstanceCustomHealthStatus@ to change the status only
-- for custom health checks, which you define using
-- @HealthCheckCustomConfig@ when you create a service. You can\'t use it
-- to change the status for Route 53 health checks, which you define using
-- @HealthCheckConfig@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_HealthCheckCustomConfig.html HealthCheckCustomConfig>.
module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
  ( -- * Creating a Request
    UpdateInstanceCustomHealthStatus (..),
    newUpdateInstanceCustomHealthStatus,

    -- * Request Lenses
    updateInstanceCustomHealthStatus_serviceId,
    updateInstanceCustomHealthStatus_instanceId,
    updateInstanceCustomHealthStatus_status,

    -- * Destructuring the Response
    UpdateInstanceCustomHealthStatusResponse (..),
    newUpdateInstanceCustomHealthStatusResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newUpdateInstanceCustomHealthStatus' smart constructor.
data UpdateInstanceCustomHealthStatus = UpdateInstanceCustomHealthStatus'
  { -- | The ID of the service that includes the configuration for the custom
    -- health check that you want to change the status for.
    serviceId :: Prelude.Text,
    -- | The ID of the instance that you want to change the health status for.
    instanceId :: Prelude.Text,
    -- | The new status of the instance, @HEALTHY@ or @UNHEALTHY@.
    status :: CustomHealthStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceCustomHealthStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceId', 'updateInstanceCustomHealthStatus_serviceId' - The ID of the service that includes the configuration for the custom
-- health check that you want to change the status for.
--
-- 'instanceId', 'updateInstanceCustomHealthStatus_instanceId' - The ID of the instance that you want to change the health status for.
--
-- 'status', 'updateInstanceCustomHealthStatus_status' - The new status of the instance, @HEALTHY@ or @UNHEALTHY@.
newUpdateInstanceCustomHealthStatus ::
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'status'
  CustomHealthStatus ->
  UpdateInstanceCustomHealthStatus
newUpdateInstanceCustomHealthStatus
  pServiceId_
  pInstanceId_
  pStatus_ =
    UpdateInstanceCustomHealthStatus'
      { serviceId =
          pServiceId_,
        instanceId = pInstanceId_,
        status = pStatus_
      }

-- | The ID of the service that includes the configuration for the custom
-- health check that you want to change the status for.
updateInstanceCustomHealthStatus_serviceId :: Lens.Lens' UpdateInstanceCustomHealthStatus Prelude.Text
updateInstanceCustomHealthStatus_serviceId = Lens.lens (\UpdateInstanceCustomHealthStatus' {serviceId} -> serviceId) (\s@UpdateInstanceCustomHealthStatus' {} a -> s {serviceId = a} :: UpdateInstanceCustomHealthStatus)

-- | The ID of the instance that you want to change the health status for.
updateInstanceCustomHealthStatus_instanceId :: Lens.Lens' UpdateInstanceCustomHealthStatus Prelude.Text
updateInstanceCustomHealthStatus_instanceId = Lens.lens (\UpdateInstanceCustomHealthStatus' {instanceId} -> instanceId) (\s@UpdateInstanceCustomHealthStatus' {} a -> s {instanceId = a} :: UpdateInstanceCustomHealthStatus)

-- | The new status of the instance, @HEALTHY@ or @UNHEALTHY@.
updateInstanceCustomHealthStatus_status :: Lens.Lens' UpdateInstanceCustomHealthStatus CustomHealthStatus
updateInstanceCustomHealthStatus_status = Lens.lens (\UpdateInstanceCustomHealthStatus' {status} -> status) (\s@UpdateInstanceCustomHealthStatus' {} a -> s {status = a} :: UpdateInstanceCustomHealthStatus)

instance
  Prelude.AWSRequest
    UpdateInstanceCustomHealthStatus
  where
  type
    Rs UpdateInstanceCustomHealthStatus =
      UpdateInstanceCustomHealthStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateInstanceCustomHealthStatusResponse'

instance
  Prelude.Hashable
    UpdateInstanceCustomHealthStatus

instance
  Prelude.NFData
    UpdateInstanceCustomHealthStatus

instance
  Prelude.ToHeaders
    UpdateInstanceCustomHealthStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.UpdateInstanceCustomHealthStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateInstanceCustomHealthStatus
  where
  toJSON UpdateInstanceCustomHealthStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceId" Prelude..= serviceId),
            Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("Status" Prelude..= status)
          ]
      )

instance
  Prelude.ToPath
    UpdateInstanceCustomHealthStatus
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateInstanceCustomHealthStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceCustomHealthStatusResponse' smart constructor.
data UpdateInstanceCustomHealthStatusResponse = UpdateInstanceCustomHealthStatusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceCustomHealthStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceCustomHealthStatusResponse ::
  UpdateInstanceCustomHealthStatusResponse
newUpdateInstanceCustomHealthStatusResponse =
  UpdateInstanceCustomHealthStatusResponse'

instance
  Prelude.NFData
    UpdateInstanceCustomHealthStatusResponse

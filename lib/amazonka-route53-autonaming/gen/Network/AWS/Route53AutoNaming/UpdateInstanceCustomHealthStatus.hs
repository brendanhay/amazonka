{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to change the health status of a custom health check to healthy or unhealthy.
--
-- You can use @UpdateInstanceCustomHealthStatus@ to change the status only for custom health checks, which you define using @HealthCheckCustomConfig@ when you create a service. You can't use it to change the status for Route 53 health checks, which you define using @HealthCheckConfig@ .
-- For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_HealthCheckCustomConfig.html HealthCheckCustomConfig> .
module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
  ( -- * Creating a request
    UpdateInstanceCustomHealthStatus (..),
    mkUpdateInstanceCustomHealthStatus,

    -- ** Request lenses
    uichsServiceId,
    uichsInstanceId,
    uichsStatus,

    -- * Destructuring the response
    UpdateInstanceCustomHealthStatusResponse (..),
    mkUpdateInstanceCustomHealthStatusResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkUpdateInstanceCustomHealthStatus' smart constructor.
data UpdateInstanceCustomHealthStatus = UpdateInstanceCustomHealthStatus'
  { serviceId ::
      Lude.Text,
    instanceId :: Lude.Text,
    status ::
      CustomHealthStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceCustomHealthStatus' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance that you want to change the health status for.
-- * 'serviceId' - The ID of the service that includes the configuration for the custom health check that you want to change the status for.
-- * 'status' - The new status of the instance, @HEALTHY@ or @UNHEALTHY@ .
mkUpdateInstanceCustomHealthStatus ::
  -- | 'serviceId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'status'
  CustomHealthStatus ->
  UpdateInstanceCustomHealthStatus
mkUpdateInstanceCustomHealthStatus
  pServiceId_
  pInstanceId_
  pStatus_ =
    UpdateInstanceCustomHealthStatus'
      { serviceId = pServiceId_,
        instanceId = pInstanceId_,
        status = pStatus_
      }

-- | The ID of the service that includes the configuration for the custom health check that you want to change the status for.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uichsServiceId :: Lens.Lens' UpdateInstanceCustomHealthStatus Lude.Text
uichsServiceId = Lens.lens (serviceId :: UpdateInstanceCustomHealthStatus -> Lude.Text) (\s a -> s {serviceId = a} :: UpdateInstanceCustomHealthStatus)
{-# DEPRECATED uichsServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The ID of the instance that you want to change the health status for.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uichsInstanceId :: Lens.Lens' UpdateInstanceCustomHealthStatus Lude.Text
uichsInstanceId = Lens.lens (instanceId :: UpdateInstanceCustomHealthStatus -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateInstanceCustomHealthStatus)
{-# DEPRECATED uichsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The new status of the instance, @HEALTHY@ or @UNHEALTHY@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uichsStatus :: Lens.Lens' UpdateInstanceCustomHealthStatus CustomHealthStatus
uichsStatus = Lens.lens (status :: UpdateInstanceCustomHealthStatus -> CustomHealthStatus) (\s a -> s {status = a} :: UpdateInstanceCustomHealthStatus)
{-# DEPRECATED uichsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest UpdateInstanceCustomHealthStatus where
  type
    Rs UpdateInstanceCustomHealthStatus =
      UpdateInstanceCustomHealthStatusResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveNull UpdateInstanceCustomHealthStatusResponse'

instance Lude.ToHeaders UpdateInstanceCustomHealthStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.UpdateInstanceCustomHealthStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInstanceCustomHealthStatus where
  toJSON UpdateInstanceCustomHealthStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServiceId" Lude..= serviceId),
            Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("Status" Lude..= status)
          ]
      )

instance Lude.ToPath UpdateInstanceCustomHealthStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateInstanceCustomHealthStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateInstanceCustomHealthStatusResponse' smart constructor.
data UpdateInstanceCustomHealthStatusResponse = UpdateInstanceCustomHealthStatusResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceCustomHealthStatusResponse' with the minimum fields required to make a request.
mkUpdateInstanceCustomHealthStatusResponse ::
  UpdateInstanceCustomHealthStatusResponse
mkUpdateInstanceCustomHealthStatusResponse =
  UpdateInstanceCustomHealthStatusResponse'

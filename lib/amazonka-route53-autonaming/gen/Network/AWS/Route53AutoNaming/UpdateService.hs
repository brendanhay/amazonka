{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.UpdateService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to perform the following operations:
--
--
--     * Update the TTL setting for existing @DnsRecords@ configurations
--
--
--     * Add, update, or delete @HealthCheckConfig@ for a specified service
--
--
-- For public and private DNS namespaces, note the following:
--
--     * If you omit any existing @DnsRecords@ or @HealthCheckConfig@ configurations from an @UpdateService@ request, the configurations are deleted from the service.
--
--
--     * If you omit an existing @HealthCheckCustomConfig@ configuration from an @UpdateService@ request, the configuration is not deleted from the service.
--
--
-- When you update settings for a service, AWS Cloud Map also updates the corresponding settings in all the records and health checks that were created by using the specified service.
module Network.AWS.Route53AutoNaming.UpdateService
  ( -- * Creating a request
    UpdateService (..),
    mkUpdateService,

    -- ** Request lenses
    usService,
    usId,

    -- * Destructuring the response
    UpdateServiceResponse (..),
    mkUpdateServiceResponse,

    -- ** Response lenses
    usrsOperationId,
    usrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | A complex type that contains the new settings for the service.
    service :: ServiceChange,
    -- | The ID of the service that you want to update.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateService' with the minimum fields required to make a request.
--
-- * 'service' - A complex type that contains the new settings for the service.
-- * 'id' - The ID of the service that you want to update.
mkUpdateService ::
  -- | 'service'
  ServiceChange ->
  -- | 'id'
  Lude.Text ->
  UpdateService
mkUpdateService pService_ pId_ =
  UpdateService' {service = pService_, id = pId_}

-- | A complex type that contains the new settings for the service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usService :: Lens.Lens' UpdateService ServiceChange
usService = Lens.lens (service :: UpdateService -> ServiceChange) (\s a -> s {service = a} :: UpdateService)
{-# DEPRECATED usService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The ID of the service that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usId :: Lens.Lens' UpdateService Lude.Text
usId = Lens.lens (id :: UpdateService -> Lude.Text) (\s a -> s {id = a} :: UpdateService)
{-# DEPRECATED usId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateService where
  type Rs UpdateService = UpdateServiceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.UpdateService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Service" Lude..= service),
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdateService where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkUpdateServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServiceResponse
mkUpdateServiceResponse pResponseStatus_ =
  UpdateServiceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsOperationId :: Lens.Lens' UpdateServiceResponse (Lude.Maybe Lude.Text)
usrsOperationId = Lens.lens (operationId :: UpdateServiceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: UpdateServiceResponse)
{-# DEPRECATED usrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateServiceResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServiceResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

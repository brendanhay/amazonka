{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeregisterInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Route 53 DNS records and health check, if any, that AWS Cloud Map created for the specified instance.
module Network.AWS.Route53AutoNaming.DeregisterInstance
  ( -- * Creating a request
    DeregisterInstance (..),
    mkDeregisterInstance,

    -- ** Request lenses
    diInstanceId,
    diServiceId,

    -- * Destructuring the response
    DeregisterInstanceResponse (..),
    mkDeregisterInstanceResponse,

    -- ** Response lenses
    drsOperationId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The value that you specified for @Id@ in the <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request.
    instanceId :: Lude.Text,
    -- | The ID of the service that the instance is associated with.
    serviceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The value that you specified for @Id@ in the <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request.
-- * 'serviceId' - The ID of the service that the instance is associated with.
mkDeregisterInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'serviceId'
  Lude.Text ->
  DeregisterInstance
mkDeregisterInstance pInstanceId_ pServiceId_ =
  DeregisterInstance'
    { instanceId = pInstanceId_,
      serviceId = pServiceId_
    }

-- | The value that you specified for @Id@ in the <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DeregisterInstance Lude.Text
diInstanceId = Lens.lens (instanceId :: DeregisterInstance -> Lude.Text) (\s a -> s {instanceId = a} :: DeregisterInstance)
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diServiceId :: Lens.Lens' DeregisterInstance Lude.Text
diServiceId = Lens.lens (serviceId :: DeregisterInstance -> Lude.Text) (\s a -> s {serviceId = a} :: DeregisterInstance)
{-# DEPRECATED diServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

instance Lude.AWSRequest DeregisterInstance where
  type Rs DeregisterInstance = DeregisterInstanceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeregisterInstanceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.DeregisterInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("ServiceId" Lude..= serviceId)
          ]
      )

instance Lude.ToPath DeregisterInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstanceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkDeregisterInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterInstanceResponse
mkDeregisterInstanceResponse pResponseStatus_ =
  DeregisterInstanceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsOperationId :: Lens.Lens' DeregisterInstanceResponse (Lude.Maybe Lude.Text)
drsOperationId = Lens.lens (operationId :: DeregisterInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DeregisterInstanceResponse)
{-# DEPRECATED drsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeregisterInstanceResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeregisterInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterInstanceResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

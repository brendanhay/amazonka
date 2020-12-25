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
    diServiceId,
    diInstanceId,

    -- * Destructuring the response
    DeregisterInstanceResponse (..),
    mkDeregisterInstanceResponse,

    -- ** Response lenses
    drsOperationId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The ID of the service that the instance is associated with.
    serviceId :: Types.ResourceId,
    -- | The value that you specified for @Id@ in the <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request.
    instanceId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstance' value with any optional fields omitted.
mkDeregisterInstance ::
  -- | 'serviceId'
  Types.ResourceId ->
  -- | 'instanceId'
  Types.ResourceId ->
  DeregisterInstance
mkDeregisterInstance serviceId instanceId =
  DeregisterInstance' {serviceId, instanceId}

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diServiceId :: Lens.Lens' DeregisterInstance Types.ResourceId
diServiceId = Lens.field @"serviceId"
{-# DEPRECATED diServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The value that you specified for @Id@ in the <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DeregisterInstance Types.ResourceId
diInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON DeregisterInstance where
  toJSON DeregisterInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServiceId" Core..= serviceId),
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.AWSRequest DeregisterInstance where
  type Rs DeregisterInstance = DeregisterInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.DeregisterInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterInstanceResponse'
            Core.<$> (x Core..:? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Core.Maybe Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstanceResponse' value with any optional fields omitted.
mkDeregisterInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterInstanceResponse
mkDeregisterInstanceResponse responseStatus =
  DeregisterInstanceResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | A value that you can use to determine whether the request completed successfully. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsOperationId :: Lens.Lens' DeregisterInstanceResponse (Core.Maybe Types.OperationId)
drsOperationId = Lens.field @"operationId"
{-# DEPRECATED drsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeregisterInstanceResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

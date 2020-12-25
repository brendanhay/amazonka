{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StartInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the @reboot instance@ operation.
--
-- The @start instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StartInstance
  ( -- * Creating a request
    StartInstance (..),
    mkStartInstance,

    -- ** Request lenses
    sInstanceName,

    -- * Destructuring the response
    StartInstanceResponse (..),
    mkStartInstanceResponse,

    -- ** Response lenses
    sirfrsOperations,
    sirfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartInstance' smart constructor.
newtype StartInstance = StartInstance'
  { -- | The name of the instance (a virtual private server) to start.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartInstance' value with any optional fields omitted.
mkStartInstance ::
  -- | 'instanceName'
  Types.ResourceName ->
  StartInstance
mkStartInstance instanceName = StartInstance' {instanceName}

-- | The name of the instance (a virtual private server) to start.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceName :: Lens.Lens' StartInstance Types.ResourceName
sInstanceName = Lens.field @"instanceName"
{-# DEPRECATED sInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON StartInstance where
  toJSON StartInstance {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest StartInstance where
  type Rs StartInstance = StartInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.StartInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartInstanceResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartInstanceResponse' value with any optional fields omitted.
mkStartInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartInstanceResponse
mkStartInstanceResponse responseStatus =
  StartInstanceResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirfrsOperations :: Lens.Lens' StartInstanceResponse (Core.Maybe [Types.Operation])
sirfrsOperations = Lens.field @"operations"
{-# DEPRECATED sirfrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirfrsResponseStatus :: Lens.Lens' StartInstanceResponse Core.Int
sirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

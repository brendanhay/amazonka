{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StopInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
-- The @stop instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StopInstance
  ( -- * Creating a request
    StopInstance (..),
    mkStopInstance,

    -- ** Request lenses
    siInstanceName,
    siForce,

    -- * Destructuring the response
    StopInstanceResponse (..),
    mkStopInstanceResponse,

    -- ** Response lenses
    sirrsOperations,
    sirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopInstance' smart constructor.
data StopInstance = StopInstance'
  { -- | The name of the instance (a virtual private server) to stop.
    instanceName :: Types.ResourceName,
    -- | When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
    --
    -- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstance' value with any optional fields omitted.
mkStopInstance ::
  -- | 'instanceName'
  Types.ResourceName ->
  StopInstance
mkStopInstance instanceName =
  StopInstance' {instanceName, force = Core.Nothing}

-- | The name of the instance (a virtual private server) to stop.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceName :: Lens.Lens' StopInstance Types.ResourceName
siInstanceName = Lens.field @"instanceName"
{-# DEPRECATED siInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
--
-- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstance (Core.Maybe Core.Bool)
siForce = Lens.field @"force"
{-# DEPRECATED siForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON StopInstance where
  toJSON StopInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("instanceName" Core..= instanceName),
            ("force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest StopInstance where
  type Rs StopInstance = StopInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.StopInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopInstanceResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopInstanceResponse' value with any optional fields omitted.
mkStopInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopInstanceResponse
mkStopInstanceResponse responseStatus =
  StopInstanceResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsOperations :: Lens.Lens' StopInstanceResponse (Core.Maybe [Types.Operation])
sirrsOperations = Lens.field @"operations"
{-# DEPRECATED sirrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' StopInstanceResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

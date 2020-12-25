{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterManagedInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling SSM Agent first.
module Network.AWS.SSM.DeregisterManagedInstance
  ( -- * Creating a request
    DeregisterManagedInstance (..),
    mkDeregisterManagedInstance,

    -- ** Request lenses
    dmiInstanceId,

    -- * Destructuring the response
    DeregisterManagedInstanceResponse (..),
    mkDeregisterManagedInstanceResponse,

    -- ** Response lenses
    dmirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeregisterManagedInstance' smart constructor.
newtype DeregisterManagedInstance = DeregisterManagedInstance'
  { -- | The ID assigned to the managed instance when you registered it using the activation process.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterManagedInstance' value with any optional fields omitted.
mkDeregisterManagedInstance ::
  -- | 'instanceId'
  Types.InstanceId ->
  DeregisterManagedInstance
mkDeregisterManagedInstance instanceId =
  DeregisterManagedInstance' {instanceId}

-- | The ID assigned to the managed instance when you registered it using the activation process.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmiInstanceId :: Lens.Lens' DeregisterManagedInstance Types.InstanceId
dmiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dmiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON DeregisterManagedInstance where
  toJSON DeregisterManagedInstance {..} =
    Core.object
      (Core.catMaybes [Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest DeregisterManagedInstance where
  type
    Rs DeregisterManagedInstance =
      DeregisterManagedInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeregisterManagedInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterManagedInstanceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterManagedInstanceResponse' smart constructor.
newtype DeregisterManagedInstanceResponse = DeregisterManagedInstanceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterManagedInstanceResponse' value with any optional fields omitted.
mkDeregisterManagedInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterManagedInstanceResponse
mkDeregisterManagedInstanceResponse responseStatus =
  DeregisterManagedInstanceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmirrsResponseStatus :: Lens.Lens' DeregisterManagedInstanceResponse Core.Int
dmirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

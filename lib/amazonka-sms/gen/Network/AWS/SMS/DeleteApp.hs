{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack associated with the application and all AWS SMS replication jobs for servers in the application.
module Network.AWS.SMS.DeleteApp
  ( -- * Creating a request
    DeleteApp (..),
    mkDeleteApp,

    -- ** Request lenses
    daAppId,
    daForceStopAppReplication,
    daForceTerminateApp,

    -- * Destructuring the response
    DeleteAppResponse (..),
    mkDeleteAppResponse,

    -- ** Response lenses
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId,
    -- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
    forceStopAppReplication :: Core.Maybe Core.Bool,
    -- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
    forceTerminateApp :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApp' value with any optional fields omitted.
mkDeleteApp ::
  DeleteApp
mkDeleteApp =
  DeleteApp'
    { appId = Core.Nothing,
      forceStopAppReplication = Core.Nothing,
      forceTerminateApp = Core.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppId :: Lens.Lens' DeleteApp (Core.Maybe Types.AppId)
daAppId = Lens.field @"appId"
{-# DEPRECATED daAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
--
-- /Note:/ Consider using 'forceStopAppReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceStopAppReplication :: Lens.Lens' DeleteApp (Core.Maybe Core.Bool)
daForceStopAppReplication = Lens.field @"forceStopAppReplication"
{-# DEPRECATED daForceStopAppReplication "Use generic-lens or generic-optics with 'forceStopAppReplication' instead." #-}

-- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
--
-- /Note:/ Consider using 'forceTerminateApp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceTerminateApp :: Lens.Lens' DeleteApp (Core.Maybe Core.Bool)
daForceTerminateApp = Lens.field @"forceTerminateApp"
{-# DEPRECATED daForceTerminateApp "Use generic-lens or generic-optics with 'forceTerminateApp' instead." #-}

instance Core.FromJSON DeleteApp where
  toJSON DeleteApp {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("forceStopAppReplication" Core..=)
              Core.<$> forceStopAppReplication,
            ("forceTerminateApp" Core..=) Core.<$> forceTerminateApp
          ]
      )

instance Core.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.DeleteApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
newtype DeleteAppResponse = DeleteAppResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppResponse' value with any optional fields omitted.
mkDeleteAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAppResponse
mkDeleteAppResponse responseStatus =
  DeleteAppResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAppResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Network.AWS.SMS.DeleteAppLaunchConfiguration
  ( -- * Creating a request
    DeleteAppLaunchConfiguration (..),
    mkDeleteAppLaunchConfiguration,

    -- ** Request lenses
    dalcAppId,

    -- * Destructuring the response
    DeleteAppLaunchConfigurationResponse (..),
    mkDeleteAppLaunchConfigurationResponse,

    -- ** Response lenses
    dalcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteAppLaunchConfiguration' smart constructor.
newtype DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppLaunchConfiguration' value with any optional fields omitted.
mkDeleteAppLaunchConfiguration ::
  DeleteAppLaunchConfiguration
mkDeleteAppLaunchConfiguration =
  DeleteAppLaunchConfiguration' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcAppId :: Lens.Lens' DeleteAppLaunchConfiguration (Core.Maybe Types.AppId)
dalcAppId = Lens.field @"appId"
{-# DEPRECATED dalcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest DeleteAppLaunchConfiguration where
  type
    Rs DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAppLaunchConfigurationResponse' smart constructor.
newtype DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppLaunchConfigurationResponse' value with any optional fields omitted.
mkDeleteAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAppLaunchConfigurationResponse
mkDeleteAppLaunchConfigurationResponse responseStatus =
  DeleteAppLaunchConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcrrsResponseStatus :: Lens.Lens' DeleteAppLaunchConfigurationResponse Core.Int
dalcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dalcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

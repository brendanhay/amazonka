{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application launch configuration associated with the specified application.
module Network.AWS.SMS.GetAppLaunchConfiguration
  ( -- * Creating a request
    GetAppLaunchConfiguration (..),
    mkGetAppLaunchConfiguration,

    -- ** Request lenses
    galcAppId,

    -- * Destructuring the response
    GetAppLaunchConfigurationResponse (..),
    mkGetAppLaunchConfigurationResponse,

    -- ** Response lenses
    galcrrsAppId,
    galcrrsAutoLaunch,
    galcrrsRoleName,
    galcrrsServerGroupLaunchConfigurations,
    galcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetAppLaunchConfiguration' smart constructor.
newtype GetAppLaunchConfiguration = GetAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppLaunchConfiguration' value with any optional fields omitted.
mkGetAppLaunchConfiguration ::
  GetAppLaunchConfiguration
mkGetAppLaunchConfiguration =
  GetAppLaunchConfiguration' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcAppId :: Lens.Lens' GetAppLaunchConfiguration (Core.Maybe Types.AppId)
galcAppId = Lens.field @"appId"
{-# DEPRECATED galcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON GetAppLaunchConfiguration where
  toJSON GetAppLaunchConfiguration {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest GetAppLaunchConfiguration where
  type
    Rs GetAppLaunchConfiguration =
      GetAppLaunchConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GetAppLaunchConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppLaunchConfigurationResponse'
            Core.<$> (x Core..:? "appId")
            Core.<*> (x Core..:? "autoLaunch")
            Core.<*> (x Core..:? "roleName")
            Core.<*> (x Core..:? "serverGroupLaunchConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAppLaunchConfigurationResponse' smart constructor.
data GetAppLaunchConfigurationResponse = GetAppLaunchConfigurationResponse'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId,
    -- | Indicates whether the application is configured to launch automatically after replication is complete.
    autoLaunch :: Core.Maybe Core.Bool,
    -- | The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
    roleName :: Core.Maybe Types.RoleName,
    -- | The launch configurations for server groups in this application.
    serverGroupLaunchConfigurations :: Core.Maybe [Types.ServerGroupLaunchConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppLaunchConfigurationResponse' value with any optional fields omitted.
mkGetAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAppLaunchConfigurationResponse
mkGetAppLaunchConfigurationResponse responseStatus =
  GetAppLaunchConfigurationResponse'
    { appId = Core.Nothing,
      autoLaunch = Core.Nothing,
      roleName = Core.Nothing,
      serverGroupLaunchConfigurations = Core.Nothing,
      responseStatus
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrrsAppId :: Lens.Lens' GetAppLaunchConfigurationResponse (Core.Maybe Types.AppId)
galcrrsAppId = Lens.field @"appId"
{-# DEPRECATED galcrrsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | Indicates whether the application is configured to launch automatically after replication is complete.
--
-- /Note:/ Consider using 'autoLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrrsAutoLaunch :: Lens.Lens' GetAppLaunchConfigurationResponse (Core.Maybe Core.Bool)
galcrrsAutoLaunch = Lens.field @"autoLaunch"
{-# DEPRECATED galcrrsAutoLaunch "Use generic-lens or generic-optics with 'autoLaunch' instead." #-}

-- | The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrrsRoleName :: Lens.Lens' GetAppLaunchConfigurationResponse (Core.Maybe Types.RoleName)
galcrrsRoleName = Lens.field @"roleName"
{-# DEPRECATED galcrrsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The launch configurations for server groups in this application.
--
-- /Note:/ Consider using 'serverGroupLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrrsServerGroupLaunchConfigurations :: Lens.Lens' GetAppLaunchConfigurationResponse (Core.Maybe [Types.ServerGroupLaunchConfiguration])
galcrrsServerGroupLaunchConfigurations = Lens.field @"serverGroupLaunchConfigurations"
{-# DEPRECATED galcrrsServerGroupLaunchConfigurations "Use generic-lens or generic-optics with 'serverGroupLaunchConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrrsResponseStatus :: Lens.Lens' GetAppLaunchConfigurationResponse Core.Int
galcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED galcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

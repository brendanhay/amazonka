{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the launch configuration for the specified application.
module Network.AWS.SMS.PutAppLaunchConfiguration
  ( -- * Creating a request
    PutAppLaunchConfiguration (..),
    mkPutAppLaunchConfiguration,

    -- ** Request lenses
    palcAppId,
    palcAutoLaunch,
    palcRoleName,
    palcServerGroupLaunchConfigurations,

    -- * Destructuring the response
    PutAppLaunchConfigurationResponse (..),
    mkPutAppLaunchConfigurationResponse,

    -- ** Response lenses
    palcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkPutAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId,
    -- | Indicates whether the application is configured to launch automatically after replication is complete.
    autoLaunch :: Core.Maybe Core.Bool,
    -- | The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
    roleName :: Core.Maybe Types.RoleName,
    -- | Information about the launch configurations for server groups in the application.
    serverGroupLaunchConfigurations :: Core.Maybe [Types.ServerGroupLaunchConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppLaunchConfiguration' value with any optional fields omitted.
mkPutAppLaunchConfiguration ::
  PutAppLaunchConfiguration
mkPutAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { appId = Core.Nothing,
      autoLaunch = Core.Nothing,
      roleName = Core.Nothing,
      serverGroupLaunchConfigurations = Core.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcAppId :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Types.AppId)
palcAppId = Lens.field @"appId"
{-# DEPRECATED palcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | Indicates whether the application is configured to launch automatically after replication is complete.
--
-- /Note:/ Consider using 'autoLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcAutoLaunch :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Core.Bool)
palcAutoLaunch = Lens.field @"autoLaunch"
{-# DEPRECATED palcAutoLaunch "Use generic-lens or generic-optics with 'autoLaunch' instead." #-}

-- | The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcRoleName :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Types.RoleName)
palcRoleName = Lens.field @"roleName"
{-# DEPRECATED palcRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Information about the launch configurations for server groups in the application.
--
-- /Note:/ Consider using 'serverGroupLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcServerGroupLaunchConfigurations :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe [Types.ServerGroupLaunchConfiguration])
palcServerGroupLaunchConfigurations = Lens.field @"serverGroupLaunchConfigurations"
{-# DEPRECATED palcServerGroupLaunchConfigurations "Use generic-lens or generic-optics with 'serverGroupLaunchConfigurations' instead." #-}

instance Core.FromJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("autoLaunch" Core..=) Core.<$> autoLaunch,
            ("roleName" Core..=) Core.<$> roleName,
            ("serverGroupLaunchConfigurations" Core..=)
              Core.<$> serverGroupLaunchConfigurations
          ]
      )

instance Core.AWSRequest PutAppLaunchConfiguration where
  type
    Rs PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAppLaunchConfigurationResponse' smart constructor.
newtype PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppLaunchConfigurationResponse' value with any optional fields omitted.
mkPutAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAppLaunchConfigurationResponse
mkPutAppLaunchConfigurationResponse responseStatus =
  PutAppLaunchConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcrrsResponseStatus :: Lens.Lens' PutAppLaunchConfigurationResponse Core.Int
palcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED palcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

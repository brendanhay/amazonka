{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a configuration for validating an application.
module Network.AWS.SMS.GetAppValidationConfiguration
  ( -- * Creating a request
    GetAppValidationConfiguration (..),
    mkGetAppValidationConfiguration,

    -- ** Request lenses
    gavcAppId,

    -- * Destructuring the response
    GetAppValidationConfigurationResponse (..),
    mkGetAppValidationConfigurationResponse,

    -- ** Response lenses
    gavcrrsAppValidationConfigurations,
    gavcrrsServerGroupValidationConfigurations,
    gavcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetAppValidationConfiguration' smart constructor.
newtype GetAppValidationConfiguration = GetAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppValidationConfiguration' value with any optional fields omitted.
mkGetAppValidationConfiguration ::
  -- | 'appId'
  Types.AppId ->
  GetAppValidationConfiguration
mkGetAppValidationConfiguration appId =
  GetAppValidationConfiguration' {appId}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcAppId :: Lens.Lens' GetAppValidationConfiguration Types.AppId
gavcAppId = Lens.field @"appId"
{-# DEPRECATED gavcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON GetAppValidationConfiguration where
  toJSON GetAppValidationConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.AWSRequest GetAppValidationConfiguration where
  type
    Rs GetAppValidationConfiguration =
      GetAppValidationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GetAppValidationConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppValidationConfigurationResponse'
            Core.<$> (x Core..:? "appValidationConfigurations")
            Core.<*> (x Core..:? "serverGroupValidationConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAppValidationConfigurationResponse' smart constructor.
data GetAppValidationConfigurationResponse = GetAppValidationConfigurationResponse'
  { -- | The configuration for application validation.
    appValidationConfigurations :: Core.Maybe [Types.AppValidationConfiguration],
    -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Core.Maybe [Types.ServerGroupValidationConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppValidationConfigurationResponse' value with any optional fields omitted.
mkGetAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAppValidationConfigurationResponse
mkGetAppValidationConfigurationResponse responseStatus =
  GetAppValidationConfigurationResponse'
    { appValidationConfigurations =
        Core.Nothing,
      serverGroupValidationConfigurations = Core.Nothing,
      responseStatus
    }

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsAppValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [Types.AppValidationConfiguration])
gavcrrsAppValidationConfigurations = Lens.field @"appValidationConfigurations"
{-# DEPRECATED gavcrrsAppValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead." #-}

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsServerGroupValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [Types.ServerGroupValidationConfiguration])
gavcrrsServerGroupValidationConfigurations = Lens.field @"serverGroupValidationConfigurations"
{-# DEPRECATED gavcrrsServerGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsResponseStatus :: Lens.Lens' GetAppValidationConfigurationResponse Core.Int
gavcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gavcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

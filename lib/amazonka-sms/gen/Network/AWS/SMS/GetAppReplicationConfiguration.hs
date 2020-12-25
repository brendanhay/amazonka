{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application replication configuration associated with the specified application.
module Network.AWS.SMS.GetAppReplicationConfiguration
  ( -- * Creating a request
    GetAppReplicationConfiguration (..),
    mkGetAppReplicationConfiguration,

    -- ** Request lenses
    garcAppId,

    -- * Destructuring the response
    GetAppReplicationConfigurationResponse (..),
    mkGetAppReplicationConfigurationResponse,

    -- ** Response lenses
    garcrrsServerGroupReplicationConfigurations,
    garcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetAppReplicationConfiguration' smart constructor.
newtype GetAppReplicationConfiguration = GetAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppReplicationConfiguration' value with any optional fields omitted.
mkGetAppReplicationConfiguration ::
  GetAppReplicationConfiguration
mkGetAppReplicationConfiguration =
  GetAppReplicationConfiguration' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcAppId :: Lens.Lens' GetAppReplicationConfiguration (Core.Maybe Types.AppId)
garcAppId = Lens.field @"appId"
{-# DEPRECATED garcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON GetAppReplicationConfiguration where
  toJSON GetAppReplicationConfiguration {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest GetAppReplicationConfiguration where
  type
    Rs GetAppReplicationConfiguration =
      GetAppReplicationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GetAppReplicationConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppReplicationConfigurationResponse'
            Core.<$> (x Core..:? "serverGroupReplicationConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAppReplicationConfigurationResponse' smart constructor.
data GetAppReplicationConfigurationResponse = GetAppReplicationConfigurationResponse'
  { -- | The replication configurations associated with server groups in this application.
    serverGroupReplicationConfigurations :: Core.Maybe [Types.ServerGroupReplicationConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAppReplicationConfigurationResponse' value with any optional fields omitted.
mkGetAppReplicationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAppReplicationConfigurationResponse
mkGetAppReplicationConfigurationResponse responseStatus =
  GetAppReplicationConfigurationResponse'
    { serverGroupReplicationConfigurations =
        Core.Nothing,
      responseStatus
    }

-- | The replication configurations associated with server groups in this application.
--
-- /Note:/ Consider using 'serverGroupReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsServerGroupReplicationConfigurations :: Lens.Lens' GetAppReplicationConfigurationResponse (Core.Maybe [Types.ServerGroupReplicationConfiguration])
garcrrsServerGroupReplicationConfigurations = Lens.field @"serverGroupReplicationConfigurations"
{-# DEPRECATED garcrrsServerGroupReplicationConfigurations "Use generic-lens or generic-optics with 'serverGroupReplicationConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsResponseStatus :: Lens.Lens' GetAppReplicationConfigurationResponse Core.Int
garcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

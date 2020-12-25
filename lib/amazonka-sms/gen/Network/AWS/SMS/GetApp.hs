{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the specified application.
module Network.AWS.SMS.GetApp
  ( -- * Creating a request
    GetApp (..),
    mkGetApp,

    -- ** Request lenses
    gaAppId,

    -- * Destructuring the response
    GetAppResponse (..),
    mkGetAppResponse,

    -- ** Response lenses
    garrsAppSummary,
    garrsServerGroups,
    garrsTags,
    garrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetApp' smart constructor.
newtype GetApp = GetApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApp' value with any optional fields omitted.
mkGetApp ::
  GetApp
mkGetApp = GetApp' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAppId :: Lens.Lens' GetApp (Core.Maybe Types.AppId)
gaAppId = Lens.field @"appId"
{-# DEPRECATED gaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON GetApp where
  toJSON GetApp {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest GetApp where
  type Rs GetApp = GetAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.GetApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Core.<$> (x Core..:? "appSummary")
            Core.<*> (x Core..:? "serverGroups")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { -- | Information about the application.
    appSummary :: Core.Maybe Types.AppSummary,
    -- | The server groups that belong to the application.
    serverGroups :: Core.Maybe [Types.ServerGroup],
    -- | The tags associated with the application.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAppResponse' value with any optional fields omitted.
mkGetAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAppResponse
mkGetAppResponse responseStatus =
  GetAppResponse'
    { appSummary = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Information about the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAppSummary :: Lens.Lens' GetAppResponse (Core.Maybe Types.AppSummary)
garrsAppSummary = Lens.field @"appSummary"
{-# DEPRECATED garrsAppSummary "Use generic-lens or generic-optics with 'appSummary' instead." #-}

-- | The server groups that belong to the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsServerGroups :: Lens.Lens' GetAppResponse (Core.Maybe [Types.ServerGroup])
garrsServerGroups = Lens.field @"serverGroups"
{-# DEPRECATED garrsServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsTags :: Lens.Lens' GetAppResponse (Core.Maybe [Types.Tag])
garrsTags = Lens.field @"tags"
{-# DEPRECATED garrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAppResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

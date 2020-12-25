{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.UpdateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.SMS.UpdateApp
  ( -- * Creating a request
    UpdateApp (..),
    mkUpdateApp,

    -- ** Request lenses
    uaAppId,
    uaDescription,
    uaName,
    uaRoleName,
    uaServerGroups,
    uaTags,

    -- * Destructuring the response
    UpdateAppResponse (..),
    mkUpdateAppResponse,

    -- ** Response lenses
    uarrsAppSummary,
    uarrsServerGroups,
    uarrsTags,
    uarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId,
    -- | The new description of the application.
    description :: Core.Maybe Types.AppDescription,
    -- | The new name of the application.
    name :: Core.Maybe Types.AppName,
    -- | The name of the service role in the customer's account used by AWS SMS.
    roleName :: Core.Maybe Types.RoleName,
    -- | The server groups in the application to update.
    serverGroups :: Core.Maybe [Types.ServerGroup],
    -- | The tags to associate with the application.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApp' value with any optional fields omitted.
mkUpdateApp ::
  UpdateApp
mkUpdateApp =
  UpdateApp'
    { appId = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      roleName = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppId :: Lens.Lens' UpdateApp (Core.Maybe Types.AppId)
uaAppId = Lens.field @"appId"
{-# DEPRECATED uaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The new description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApp (Core.Maybe Types.AppDescription)
uaDescription = Lens.field @"description"
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApp (Core.Maybe Types.AppName)
uaName = Lens.field @"name"
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the service role in the customer's account used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoleName :: Lens.Lens' UpdateApp (Core.Maybe Types.RoleName)
uaRoleName = Lens.field @"roleName"
{-# DEPRECATED uaRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The server groups in the application to update.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServerGroups :: Lens.Lens' UpdateApp (Core.Maybe [Types.ServerGroup])
uaServerGroups = Lens.field @"serverGroups"
{-# DEPRECATED uaServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags to associate with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTags :: Lens.Lens' UpdateApp (Core.Maybe [Types.Tag])
uaTags = Lens.field @"tags"
{-# DEPRECATED uaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UpdateApp where
  toJSON UpdateApp {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("description" Core..=) Core.<$> description,
            ("name" Core..=) Core.<$> name,
            ("roleName" Core..=) Core.<$> roleName,
            ("serverGroups" Core..=) Core.<$> serverGroups,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest UpdateApp where
  type Rs UpdateApp = UpdateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.UpdateApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Core.<$> (x Core..:? "appSummary")
            Core.<*> (x Core..:? "serverGroups")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { -- | A summary description of the application.
    appSummary :: Core.Maybe Types.AppSummary,
    -- | The updated server groups in the application.
    serverGroups :: Core.Maybe [Types.ServerGroup],
    -- | The tags associated with the application.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAppResponse' value with any optional fields omitted.
mkUpdateAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAppResponse
mkUpdateAppResponse responseStatus =
  UpdateAppResponse'
    { appSummary = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | A summary description of the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAppSummary :: Lens.Lens' UpdateAppResponse (Core.Maybe Types.AppSummary)
uarrsAppSummary = Lens.field @"appSummary"
{-# DEPRECATED uarrsAppSummary "Use generic-lens or generic-optics with 'appSummary' instead." #-}

-- | The updated server groups in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsServerGroups :: Lens.Lens' UpdateAppResponse (Core.Maybe [Types.ServerGroup])
uarrsServerGroups = Lens.field @"serverGroups"
{-# DEPRECATED uarrsServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsTags :: Lens.Lens' UpdateAppResponse (Core.Maybe [Types.Tag])
uarrsTags = Lens.field @"tags"
{-# DEPRECATED uarrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateAppResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

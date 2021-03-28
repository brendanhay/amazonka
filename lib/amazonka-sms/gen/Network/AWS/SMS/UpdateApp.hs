{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateApp (..)
    , mkUpdateApp
    -- ** Request lenses
    , uaAppId
    , uaDescription
    , uaName
    , uaRoleName
    , uaServerGroups
    , uaTags

    -- * Destructuring the response
    , UpdateAppResponse (..)
    , mkUpdateAppResponse
    -- ** Response lenses
    , uarrsAppSummary
    , uarrsServerGroups
    , uarrsTags
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  , description :: Core.Maybe Types.AppDescription
    -- ^ The new description of the application.
  , name :: Core.Maybe Types.AppName
    -- ^ The new name of the application.
  , roleName :: Core.Maybe Types.RoleName
    -- ^ The name of the service role in the customer's account used by AWS SMS.
  , serverGroups :: Core.Maybe [Types.ServerGroup]
    -- ^ The server groups in the application to update.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to associate with the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApp' value with any optional fields omitted.
mkUpdateApp
    :: UpdateApp
mkUpdateApp
  = UpdateApp'{appId = Core.Nothing, description = Core.Nothing,
               name = Core.Nothing, roleName = Core.Nothing,
               serverGroups = Core.Nothing, tags = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppId :: Lens.Lens' UpdateApp (Core.Maybe Types.AppId)
uaAppId = Lens.field @"appId"
{-# INLINEABLE uaAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The new description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApp (Core.Maybe Types.AppDescription)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The new name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApp (Core.Maybe Types.AppName)
uaName = Lens.field @"name"
{-# INLINEABLE uaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the service role in the customer's account used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoleName :: Lens.Lens' UpdateApp (Core.Maybe Types.RoleName)
uaRoleName = Lens.field @"roleName"
{-# INLINEABLE uaRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The server groups in the application to update.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServerGroups :: Lens.Lens' UpdateApp (Core.Maybe [Types.ServerGroup])
uaServerGroups = Lens.field @"serverGroups"
{-# INLINEABLE uaServerGroups #-}
{-# DEPRECATED serverGroups "Use generic-lens or generic-optics with 'serverGroups' instead"  #-}

-- | The tags to associate with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTags :: Lens.Lens' UpdateApp (Core.Maybe [Types.Tag])
uaTags = Lens.field @"tags"
{-# INLINEABLE uaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery UpdateApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApp where
        toHeaders UpdateApp{..}
          = Core.pure
              ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.UpdateApp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApp where
        toJSON UpdateApp{..}
          = Core.object
              (Core.catMaybes
                 [("appId" Core..=) Core.<$> appId,
                  ("description" Core..=) Core.<$> description,
                  ("name" Core..=) Core.<$> name,
                  ("roleName" Core..=) Core.<$> roleName,
                  ("serverGroups" Core..=) Core.<$> serverGroups,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest UpdateApp where
        type Rs UpdateApp = UpdateAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAppResponse' Core.<$>
                   (x Core..:? "appSummary") Core.<*> x Core..:? "serverGroups"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { appSummary :: Core.Maybe Types.AppSummary
    -- ^ A summary description of the application.
  , serverGroups :: Core.Maybe [Types.ServerGroup]
    -- ^ The updated server groups in the application.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateAppResponse' value with any optional fields omitted.
mkUpdateAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAppResponse
mkUpdateAppResponse responseStatus
  = UpdateAppResponse'{appSummary = Core.Nothing,
                       serverGroups = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | A summary description of the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAppSummary :: Lens.Lens' UpdateAppResponse (Core.Maybe Types.AppSummary)
uarrsAppSummary = Lens.field @"appSummary"
{-# INLINEABLE uarrsAppSummary #-}
{-# DEPRECATED appSummary "Use generic-lens or generic-optics with 'appSummary' instead"  #-}

-- | The updated server groups in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsServerGroups :: Lens.Lens' UpdateAppResponse (Core.Maybe [Types.ServerGroup])
uarrsServerGroups = Lens.field @"serverGroups"
{-# INLINEABLE uarrsServerGroups #-}
{-# DEPRECATED serverGroups "Use generic-lens or generic-optics with 'serverGroups' instead"  #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsTags :: Lens.Lens' UpdateAppResponse (Core.Maybe [Types.Tag])
uarrsTags = Lens.field @"tags"
{-# INLINEABLE uarrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateAppResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

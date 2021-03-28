{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application. An application consists of one or more server groups. Each server group contain one or more servers.
module Network.AWS.SMS.CreateApp
    (
    -- * Creating a request
      CreateApp (..)
    , mkCreateApp
    -- ** Request lenses
    , caClientToken
    , caDescription
    , caName
    , caRoleName
    , caServerGroups
    , caTags

    -- * Destructuring the response
    , CreateAppResponse (..)
    , mkCreateAppResponse
    -- ** Response lenses
    , carrsAppSummary
    , carrsServerGroups
    , carrsTags
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { clientToken :: Core.Maybe Types.ClientToken
    -- ^ A unique, case-sensitive identifier that you provide to ensure the idempotency of application creation.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the new application
  , name :: Core.Maybe Types.AppName
    -- ^ The name of the new application.
  , roleName :: Core.Maybe Types.RoleName
    -- ^ The name of the service role in the customer's account to be used by AWS SMS.
  , serverGroups :: Core.Maybe [Types.ServerGroup]
    -- ^ The server groups to include in the application.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be associated with the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp
    :: CreateApp
mkCreateApp
  = CreateApp'{clientToken = Core.Nothing,
               description = Core.Nothing, name = Core.Nothing,
               roleName = Core.Nothing, serverGroups = Core.Nothing,
               tags = Core.Nothing}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of application creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caClientToken :: Lens.Lens' CreateApp (Core.Maybe Types.ClientToken)
caClientToken = Lens.field @"clientToken"
{-# INLINEABLE caClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description of the new application
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApp (Core.Maybe Types.Description)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the new application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApp (Core.Maybe Types.AppName)
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the service role in the customer's account to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoleName :: Lens.Lens' CreateApp (Core.Maybe Types.RoleName)
caRoleName = Lens.field @"roleName"
{-# INLINEABLE caRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The server groups to include in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caServerGroups :: Lens.Lens' CreateApp (Core.Maybe [Types.ServerGroup])
caServerGroups = Lens.field @"serverGroups"
{-# INLINEABLE caServerGroups #-}
{-# DEPRECATED serverGroups "Use generic-lens or generic-optics with 'serverGroups' instead"  #-}

-- | The tags to be associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApp where
        toHeaders CreateApp{..}
          = Core.pure
              ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.CreateApp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApp where
        toJSON CreateApp{..}
          = Core.object
              (Core.catMaybes
                 [("clientToken" Core..=) Core.<$> clientToken,
                  ("description" Core..=) Core.<$> description,
                  ("name" Core..=) Core.<$> name,
                  ("roleName" Core..=) Core.<$> roleName,
                  ("serverGroups" Core..=) Core.<$> serverGroups,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateApp where
        type Rs CreateApp = CreateAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAppResponse' Core.<$>
                   (x Core..:? "appSummary") Core.<*> x Core..:? "serverGroups"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { appSummary :: Core.Maybe Types.AppSummary
    -- ^ A summary description of the application.
  , serverGroups :: Core.Maybe [Types.ServerGroup]
    -- ^ The server groups included in the application.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAppResponse
mkCreateAppResponse responseStatus
  = CreateAppResponse'{appSummary = Core.Nothing,
                       serverGroups = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | A summary description of the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAppSummary :: Lens.Lens' CreateAppResponse (Core.Maybe Types.AppSummary)
carrsAppSummary = Lens.field @"appSummary"
{-# INLINEABLE carrsAppSummary #-}
{-# DEPRECATED appSummary "Use generic-lens or generic-optics with 'appSummary' instead"  #-}

-- | The server groups included in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsServerGroups :: Lens.Lens' CreateAppResponse (Core.Maybe [Types.ServerGroup])
carrsServerGroups = Lens.field @"serverGroups"
{-# INLINEABLE carrsServerGroups #-}
{-# DEPRECATED serverGroups "Use generic-lens or generic-optics with 'serverGroups' instead"  #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsTags :: Lens.Lens' CreateAppResponse (Core.Maybe [Types.Tag])
carrsTags = Lens.field @"tags"
{-# INLINEABLE carrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail resource. 
module Network.AWS.WorkMail.CreateResource
    (
    -- * Creating a request
      CreateResource (..)
    , mkCreateResource
    -- ** Request lenses
    , crOrganizationId
    , crName
    , crType

    -- * Destructuring the response
    , CreateResourceResponse (..)
    , mkCreateResourceResponse
    -- ** Response lenses
    , crrrsResourceId
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateResource' smart constructor.
data CreateResource = CreateResource'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier associated with the organization for which the resource is created.
  , name :: Types.ResourceName
    -- ^ The name of the new resource.
  , type' :: Types.ResourceType
    -- ^ The type of the new resource. The available types are @equipment@ and @room@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResource' value with any optional fields omitted.
mkCreateResource
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.ResourceName -- ^ 'name'
    -> Types.ResourceType -- ^ 'type\''
    -> CreateResource
mkCreateResource organizationId name type'
  = CreateResource'{organizationId, name, type'}

-- | The identifier associated with the organization for which the resource is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crOrganizationId :: Lens.Lens' CreateResource Types.OrganizationId
crOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE crOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The name of the new resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CreateResource Types.ResourceName
crName = Lens.field @"name"
{-# INLINEABLE crName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of the new resource. The available types are @equipment@ and @room@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crType :: Lens.Lens' CreateResource Types.ResourceType
crType = Lens.field @"type'"
{-# INLINEABLE crType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery CreateResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateResource where
        toHeaders CreateResource{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.CreateResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateResource where
        toJSON CreateResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type')])

instance Core.AWSRequest CreateResource where
        type Rs CreateResource = CreateResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateResourceResponse' Core.<$>
                   (x Core..:? "ResourceId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { resourceId :: Core.Maybe Types.ResourceId
    -- ^ The identifier of the new resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceResponse' value with any optional fields omitted.
mkCreateResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateResourceResponse
mkCreateResourceResponse responseStatus
  = CreateResourceResponse'{resourceId = Core.Nothing,
                            responseStatus}

-- | The identifier of the new resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResourceId :: Lens.Lens' CreateResourceResponse (Core.Maybe Types.ResourceId)
crrrsResourceId = Lens.field @"resourceId"
{-# INLINEABLE crrrsResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateResourceResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

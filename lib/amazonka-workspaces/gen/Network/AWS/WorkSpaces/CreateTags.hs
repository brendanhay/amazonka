{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.CreateTags
    (
    -- * Creating a request
      CreateTags (..)
    , mkCreateTags
    -- ** Request lenses
    , ctResourceId
    , ctTags

    -- * Destructuring the response
    , CreateTagsResponse (..)
    , mkCreateTagsResponse
    -- ** Response lenses
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceId :: Types.ResourceId
    -- ^ The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
  , tags :: [Types.Tag]
    -- ^ The tags. Each WorkSpaces resource can have a maximum of 50 tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags
    :: Types.ResourceId -- ^ 'resourceId'
    -> CreateTags
mkCreateTags resourceId
  = CreateTags'{resourceId, tags = Core.mempty}

-- | The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceId :: Lens.Lens' CreateTags Types.ResourceId
ctResourceId = Lens.field @"resourceId"
{-# INLINEABLE ctResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Types.Tag]
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTags where
        toHeaders CreateTags{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.CreateTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTags where
        toJSON CreateTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTagsResponse
mkCreateTagsResponse responseStatus
  = CreateTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTagsResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, @AddTags@ updates the tag's value.
module Network.AWS.MachineLearning.AddTags
    (
    -- * Creating a request
      AddTags (..)
    , mkAddTags
    -- ** Request lenses
    , atTags
    , atResourceId
    , atResourceType

    -- * Destructuring the response
    , AddTagsResponse (..)
    , mkAddTagsResponse
    -- ** Response lenses
    , atrrsResourceId
    , atrrsResourceType
    , atrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { tags :: [Types.Tag]
    -- ^ The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
  , resourceId :: Types.EntityId
    -- ^ The ID of the ML object to tag. For example, @exampleModelId@ .
  , resourceType :: Types.TaggableResourceType
    -- ^ The type of the ML object to tag. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags
    :: Types.EntityId -- ^ 'resourceId'
    -> Types.TaggableResourceType -- ^ 'resourceType'
    -> AddTags
mkAddTags resourceId resourceType
  = AddTags'{tags = Core.mempty, resourceId, resourceType}

-- | The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Types.Tag]
atTags = Lens.field @"tags"
{-# INLINEABLE atTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the ML object to tag. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Types.EntityId
atResourceId = Lens.field @"resourceId"
{-# INLINEABLE atResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of the ML object to tag. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceType :: Lens.Lens' AddTags Types.TaggableResourceType
atResourceType = Lens.field @"resourceType"
{-# INLINEABLE atResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery AddTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTags where
        toHeaders AddTags{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.AddTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTags where
        toJSON AddTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Tags" Core..= tags),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ResourceType" Core..= resourceType)])

instance Core.AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddTagsResponse' Core.<$>
                   (x Core..:? "ResourceId") Core.<*> x Core..:? "ResourceType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Amazon ML returns the following elements. 
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { resourceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the ML object that was tagged.
  , resourceType :: Core.Maybe Types.TaggableResourceType
    -- ^ The type of the ML object that was tagged.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsResponse' value with any optional fields omitted.
mkAddTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsResponse
mkAddTagsResponse responseStatus
  = AddTagsResponse'{resourceId = Core.Nothing,
                     resourceType = Core.Nothing, responseStatus}

-- | The ID of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResourceId :: Lens.Lens' AddTagsResponse (Core.Maybe Types.ResourceId)
atrrsResourceId = Lens.field @"resourceId"
{-# INLINEABLE atrrsResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResourceType :: Lens.Lens' AddTagsResponse (Core.Maybe Types.TaggableResourceType)
atrrsResourceType = Lens.field @"resourceType"
{-# INLINEABLE atrrsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

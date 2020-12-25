{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags associated with an ML object. After this operation is complete, you can't recover deleted tags.
--
-- If you specify a tag that doesn't exist, Amazon ML ignores it.
module Network.AWS.MachineLearning.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dTagKeys,
    dResourceId,
    dResourceType,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    drsResourceId,
    drsResourceType,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | One or more tags to delete.
    tagKeys :: [Types.TagKey],
    -- | The ID of the tagged ML object. For example, @exampleModelId@ .
    resourceId :: Types.ResourceId,
    -- | The type of the tagged ML object.
    resourceType :: Types.TaggableResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags ::
  -- | 'resourceId'
  Types.ResourceId ->
  -- | 'resourceType'
  Types.TaggableResourceType ->
  DeleteTags
mkDeleteTags resourceId resourceType =
  DeleteTags' {tagKeys = Core.mempty, resourceId, resourceType}

-- | One or more tags to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagKeys :: Lens.Lens' DeleteTags [Types.TagKey]
dTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The ID of the tagged ML object. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceId :: Lens.Lens' DeleteTags Types.ResourceId
dResourceId = Lens.field @"resourceId"
{-# DEPRECATED dResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the tagged ML object.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceType :: Lens.Lens' DeleteTags Types.TaggableResourceType
dResourceType = Lens.field @"resourceType"
{-# DEPRECATED dResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON DeleteTags where
  toJSON DeleteTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TagKeys" Core..= tagKeys),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.DeleteTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTagsResponse'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Amazon ML returns the following elements.
--
-- /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  { -- | The ID of the ML object from which tags were deleted.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The type of the ML object from which tags were deleted.
    resourceType :: Core.Maybe Types.TaggableResourceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTagsResponse
mkDeleteTagsResponse responseStatus =
  DeleteTagsResponse'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      responseStatus
    }

-- | The ID of the ML object from which tags were deleted.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResourceId :: Lens.Lens' DeleteTagsResponse (Core.Maybe Types.ResourceId)
drsResourceId = Lens.field @"resourceId"
{-# DEPRECATED drsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object from which tags were deleted.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResourceType :: Lens.Lens' DeleteTagsResponse (Core.Maybe Types.TaggableResourceType)
drsResourceType = Lens.field @"resourceType"
{-# DEPRECATED drsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTagsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

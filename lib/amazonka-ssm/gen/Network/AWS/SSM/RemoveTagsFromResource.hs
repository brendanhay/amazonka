{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tag keys from the specified resource.
module Network.AWS.SSM.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceType,
    rtfrResourceId,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The type of resource from which you want to remove a tag.
    resourceType :: Types.ResourceTypeForTagging,
    -- | The ID of the resource from which you want to remove tags. For example:
    --
    -- ManagedInstance: mi-012345abcde
    -- MaintenanceWindow: mw-012345abcde
    -- PatchBaseline: pb-012345abcde
    -- For the Document and Parameter values, use the name of the resource.
    resourceId :: Types.ResourceId,
    -- | Tag keys that you want to remove from the specified resource.
    tagKeys :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource ::
  -- | 'resourceType'
  Types.ResourceTypeForTagging ->
  -- | 'resourceId'
  Types.ResourceId ->
  RemoveTagsFromResource
mkRemoveTagsFromResource resourceType resourceId =
  RemoveTagsFromResource'
    { resourceType,
      resourceId,
      tagKeys = Core.mempty
    }

-- | The type of resource from which you want to remove a tag.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceType :: Lens.Lens' RemoveTagsFromResource Types.ResourceTypeForTagging
rtfrResourceType = Lens.field @"resourceType"
{-# DEPRECATED rtfrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
-- MaintenanceWindow: mw-012345abcde
-- PatchBaseline: pb-012345abcde
-- For the Document and Parameter values, use the name of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceId :: Lens.Lens' RemoveTagsFromResource Types.ResourceId
rtfrResourceId = Lens.field @"resourceId"
{-# DEPRECATED rtfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Tag keys that you want to remove from the specified resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.RemoveTagsFromResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse responseStatus =
  RemoveTagsFromResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

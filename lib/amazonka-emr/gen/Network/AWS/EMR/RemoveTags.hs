{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
--
-- The following example removes the stack tag with value Prod from a cluster:
module Network.AWS.EMR.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtResourceId,
    rtTagKeys,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input identifies a cluster and a list of tags to remove.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
    resourceId :: Types.ResourceId,
    -- | A list of tag keys to remove from a resource.
    tagKeys :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTags' value with any optional fields omitted.
mkRemoveTags ::
  -- | 'resourceId'
  Types.ResourceId ->
  RemoveTags
mkRemoveTags resourceId =
  RemoveTags' {resourceId, tagKeys = Core.mempty}

-- | The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceId :: Lens.Lens' RemoveTags Types.ResourceId
rtResourceId = Lens.field @"resourceId"
{-# DEPRECATED rtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tag keys to remove from a resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Types.String]
rtTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTags where
  toJSON RemoveTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.RemoveTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | This output indicates the result of removing tags from a resource.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsResponse' value with any optional fields omitted.
mkRemoveTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveTagsResponse
mkRemoveTagsResponse responseStatus =
  RemoveTagsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RemoveTagsResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

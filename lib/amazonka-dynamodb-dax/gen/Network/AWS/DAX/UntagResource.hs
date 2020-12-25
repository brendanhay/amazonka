{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association of tags from a DAX resource. You can call @UntagResource@ up to 5 times per second, per account.
module Network.AWS.DAX.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urResourceName,
    urTagKeys,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrrsTags,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The name of the DAX resource from which the tags should be removed.
    resourceName :: Types.String,
    -- | A list of tag keys. If the DAX cluster has any tags with these keys, then the tags are removed from the cluster.
    tagKeys :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource ::
  -- | 'resourceName'
  Types.String ->
  UntagResource
mkUntagResource resourceName =
  UntagResource' {resourceName, tagKeys = Core.mempty}

-- | The name of the DAX resource from which the tags should be removed.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceName :: Lens.Lens' UntagResource Types.String
urResourceName = Lens.field @"resourceName"
{-# DEPRECATED urResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A list of tag keys. If the DAX cluster has any tags with these keys, then the tags are removed from the cluster.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Types.String]
urTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON UntagResource where
  toJSON UntagResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceName" Core..= resourceName),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.UntagResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourceResponse'
            Core.<$> (x Core..:? "Tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The tag keys that have been removed from the cluster.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourceResponse' value with any optional fields omitted.
mkUntagResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UntagResourceResponse
mkUntagResourceResponse responseStatus =
  UntagResourceResponse' {tags = Core.Nothing, responseStatus}

-- | The tag keys that have been removed from the cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsTags :: Lens.Lens' UntagResourceResponse (Core.Maybe [Types.Tag])
urrrsTags = Lens.field @"tags"
{-# DEPRECATED urrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UntagResourceResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an AWS DMS resource, including replication instance, endpoint, security group, and migration task. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html @Tag@ > data type description.
module Network.AWS.DMS.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceArn,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes one or more tags from an AWS DMS resource.
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | An AWS DMS resource from which you want to remove tag(s). The value for this parameter is an Amazon Resource Name (ARN).
    resourceArn :: Types.String,
    -- | The tag key (name) of the tag to be removed.
    tagKeys :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource ::
  -- | 'resourceArn'
  Types.String ->
  RemoveTagsFromResource
mkRemoveTagsFromResource resourceArn =
  RemoveTagsFromResource' {resourceArn, tagKeys = Core.mempty}

-- | An AWS DMS resource from which you want to remove tag(s). The value for this parameter is an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceArn :: Lens.Lens' RemoveTagsFromResource Types.String
rtfrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED rtfrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Types.String]
rtfrTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceArn" Core..= resourceArn),
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
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.RemoveTagsFromResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
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

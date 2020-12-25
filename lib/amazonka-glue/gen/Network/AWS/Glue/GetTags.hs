{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of tags associated with a resource.
module Network.AWS.Glue.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtResourceArn,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrrsTags,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTags' smart constructor.
newtype GetTags = GetTags'
  { -- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
    resourceArn :: Types.GlueResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTags' value with any optional fields omitted.
mkGetTags ::
  -- | 'resourceArn'
  Types.GlueResourceArn ->
  GetTags
mkGetTags resourceArn = GetTags' {resourceArn}

-- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtResourceArn :: Lens.Lens' GetTags Types.GlueResourceArn
gtResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED gtResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON GetTags where
  toJSON GetTags {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Core.<$> (x Core..:? "Tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The requested tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagsResponse' value with any optional fields omitted.
mkGetTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTagsResponse
mkGetTagsResponse responseStatus =
  GetTagsResponse' {tags = Core.Nothing, responseStatus}

-- | The requested tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTags :: Lens.Lens' GetTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gtrrsTags = Lens.field @"tags"
{-# DEPRECATED gtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTagsResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

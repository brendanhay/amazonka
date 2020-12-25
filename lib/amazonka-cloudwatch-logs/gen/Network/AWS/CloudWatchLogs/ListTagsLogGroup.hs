{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.ListTagsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified log group.
module Network.AWS.CloudWatchLogs.ListTagsLogGroup
  ( -- * Creating a request
    ListTagsLogGroup (..),
    mkListTagsLogGroup,

    -- ** Request lenses
    ltlgLogGroupName,

    -- * Destructuring the response
    ListTagsLogGroupResponse (..),
    mkListTagsLogGroupResponse,

    -- ** Response lenses
    ltlgrrsTags,
    ltlgrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsLogGroup' smart constructor.
newtype ListTagsLogGroup = ListTagsLogGroup'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsLogGroup' value with any optional fields omitted.
mkListTagsLogGroup ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  ListTagsLogGroup
mkListTagsLogGroup logGroupName = ListTagsLogGroup' {logGroupName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgLogGroupName :: Lens.Lens' ListTagsLogGroup Types.LogGroupName
ltlgLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED ltlgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Core.FromJSON ListTagsLogGroup where
  toJSON ListTagsLogGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.AWSRequest ListTagsLogGroup where
  type Rs ListTagsLogGroup = ListTagsLogGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.ListTagsLogGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsLogGroupResponse'
            Core.<$> (x Core..:? "tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTagsLogGroupResponse' smart constructor.
data ListTagsLogGroupResponse = ListTagsLogGroupResponse'
  { -- | The tags for the log group.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsLogGroupResponse' value with any optional fields omitted.
mkListTagsLogGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTagsLogGroupResponse
mkListTagsLogGroupResponse responseStatus =
  ListTagsLogGroupResponse' {tags = Core.Nothing, responseStatus}

-- | The tags for the log group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrrsTags :: Lens.Lens' ListTagsLogGroupResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltlgrrsTags = Lens.field @"tags"
{-# DEPRECATED ltlgrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrrsResponseStatus :: Lens.Lens' ListTagsLogGroupResponse Core.Int
ltlgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltlgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

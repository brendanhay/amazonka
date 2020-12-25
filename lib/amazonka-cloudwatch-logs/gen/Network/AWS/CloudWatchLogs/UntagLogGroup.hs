{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.UntagLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified log group.
--
-- To list the tags for a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup> . To add tags, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TagLogGroup.html TagLogGroup> .
module Network.AWS.CloudWatchLogs.UntagLogGroup
  ( -- * Creating a request
    UntagLogGroup (..),
    mkUntagLogGroup,

    -- ** Request lenses
    ulgLogGroupName,
    ulgTags,

    -- * Destructuring the response
    UntagLogGroupResponse (..),
    mkUntagLogGroupResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagLogGroup' smart constructor.
data UntagLogGroup = UntagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The tag keys. The corresponding tags are removed from the log group.
    tags :: Core.NonEmpty Types.TagKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagLogGroup' value with any optional fields omitted.
mkUntagLogGroup ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'tags'
  Core.NonEmpty Types.TagKey ->
  UntagLogGroup
mkUntagLogGroup logGroupName tags =
  UntagLogGroup' {logGroupName, tags}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulgLogGroupName :: Lens.Lens' UntagLogGroup Types.LogGroupName
ulgLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED ulgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The tag keys. The corresponding tags are removed from the log group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulgTags :: Lens.Lens' UntagLogGroup (Core.NonEmpty Types.TagKey)
ulgTags = Lens.field @"tags"
{-# DEPRECATED ulgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UntagLogGroup where
  toJSON UntagLogGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("tags" Core..= tags)
          ]
      )

instance Core.AWSRequest UntagLogGroup where
  type Rs UntagLogGroup = UntagLogGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.UntagLogGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UntagLogGroupResponse'

-- | /See:/ 'mkUntagLogGroupResponse' smart constructor.
data UntagLogGroupResponse = UntagLogGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagLogGroupResponse' value with any optional fields omitted.
mkUntagLogGroupResponse ::
  UntagLogGroupResponse
mkUntagLogGroupResponse = UntagLogGroupResponse'

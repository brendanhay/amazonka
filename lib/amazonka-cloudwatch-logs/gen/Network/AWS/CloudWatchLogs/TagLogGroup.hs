{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TagLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the specified tags for the specified log group.
--
-- To list the tags for a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup> . To remove tags, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_UntagLogGroup.html UntagLogGroup> .
-- For more information about tags, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html#log-group-tagging Tag Log Groups in Amazon CloudWatch Logs> in the /Amazon CloudWatch Logs User Guide/ .
module Network.AWS.CloudWatchLogs.TagLogGroup
    (
    -- * Creating a request
      TagLogGroup (..)
    , mkTagLogGroup
    -- ** Request lenses
    , tlgLogGroupName
    , tlgTags

    -- * Destructuring the response
    , TagLogGroupResponse (..)
    , mkTagLogGroupResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagLogGroup' smart constructor.
data TagLogGroup = TagLogGroup'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , tags :: Core.HashMap Types.TagKey Types.TagValue
    -- ^ The key-value pairs to use for the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagLogGroup' value with any optional fields omitted.
mkTagLogGroup
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> TagLogGroup
mkTagLogGroup logGroupName
  = TagLogGroup'{logGroupName, tags = Core.mempty}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlgLogGroupName :: Lens.Lens' TagLogGroup Types.LogGroupName
tlgLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE tlgLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The key-value pairs to use for the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlgTags :: Lens.Lens' TagLogGroup (Core.HashMap Types.TagKey Types.TagValue)
tlgTags = Lens.field @"tags"
{-# INLINEABLE tlgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagLogGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagLogGroup where
        toHeaders TagLogGroup{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.TagLogGroup") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagLogGroup where
        toJSON TagLogGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("tags" Core..= tags)])

instance Core.AWSRequest TagLogGroup where
        type Rs TagLogGroup = TagLogGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull TagLogGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagLogGroupResponse' smart constructor.
data TagLogGroupResponse = TagLogGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagLogGroupResponse' value with any optional fields omitted.
mkTagLogGroupResponse
    :: TagLogGroupResponse
mkTagLogGroupResponse = TagLogGroupResponse'

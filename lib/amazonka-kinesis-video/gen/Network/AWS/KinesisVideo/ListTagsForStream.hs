{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
-- In the request, you must specify either the @StreamName@ or the @StreamARN@ .
module Network.AWS.KinesisVideo.ListTagsForStream
  ( -- * Creating a request
    ListTagsForStream (..),
    mkListTagsForStream,

    -- ** Request lenses
    ltfsNextToken,
    ltfsStreamARN,
    ltfsStreamName,

    -- * Destructuring the response
    ListTagsForStreamResponse (..),
    mkListTagsForStreamResponse,

    -- ** Response lenses
    ltfsrrsNextToken,
    ltfsrrsTags,
    ltfsrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The Amazon Resource Name (ARN) of the stream that you want to list tags for.
    streamARN :: Core.Maybe Types.StreamARN,
    -- | The name of the stream that you want to list tags for.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStream' value with any optional fields omitted.
mkListTagsForStream ::
  ListTagsForStream
mkListTagsForStream =
  ListTagsForStream'
    { nextToken = Core.Nothing,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsNextToken :: Lens.Lens' ListTagsForStream (Core.Maybe Types.NextToken)
ltfsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamARN :: Lens.Lens' ListTagsForStream (Core.Maybe Types.StreamARN)
ltfsStreamARN = Lens.field @"streamARN"
{-# DEPRECATED ltfsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamName :: Lens.Lens' ListTagsForStream (Core.Maybe Types.StreamName)
ltfsStreamName = Lens.field @"streamName"
{-# DEPRECATED ltfsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON ListTagsForStream where
  toJSON ListTagsForStream {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest ListTagsForStream where
  type Rs ListTagsForStream = ListTagsForStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/listTagsForStream",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { -- | If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A map of tag keys and values associated with the specified stream.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStreamResponse' value with any optional fields omitted.
mkListTagsForStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTagsForStreamResponse
mkListTagsForStreamResponse responseStatus =
  ListTagsForStreamResponse'
    { nextToken = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsNextToken :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe Types.NextToken)
ltfsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A map of tag keys and values associated with the specified stream.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsTags :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltfsrrsTags = Lens.field @"tags"
{-# DEPRECATED ltfsrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsResponseStatus :: Lens.Lens' ListTagsForStreamResponse Core.Int
ltfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

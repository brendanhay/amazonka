{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.TagStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a stream. A /tag/ is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- You must provide either the @StreamName@ or the @StreamARN@ .
-- This operation requires permission for the @KinesisVideo:TagStream@ action.
-- Kinesis video streams support up to 50 tags.
module Network.AWS.KinesisVideo.TagStream
  ( -- * Creating a request
    TagStream (..),
    mkTagStream,

    -- ** Request lenses
    tsTags,
    tsStreamARN,
    tsStreamName,

    -- * Destructuring the response
    TagStreamResponse (..),
    mkTagStreamResponse,

    -- ** Response lenses
    tsrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagStream' smart constructor.
data TagStream = TagStream'
  { -- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
    tags :: Core.HashMap Types.TagKey Types.TagValue,
    -- | The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
    streamARN :: Core.Maybe Types.ResourceARN,
    -- | The name of the stream that you want to add the tag or tags to.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagStream' value with any optional fields omitted.
mkTagStream ::
  TagStream
mkTagStream =
  TagStream'
    { tags = Core.mempty,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTags :: Lens.Lens' TagStream (Core.HashMap Types.TagKey Types.TagValue)
tsTags = Lens.field @"tags"
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStreamARN :: Lens.Lens' TagStream (Core.Maybe Types.ResourceARN)
tsStreamARN = Lens.field @"streamARN"
{-# DEPRECATED tsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to add the tag or tags to.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStreamName :: Lens.Lens' TagStream (Core.Maybe Types.StreamName)
tsStreamName = Lens.field @"streamName"
{-# DEPRECATED tsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON TagStream where
  toJSON TagStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Tags" Core..= tags),
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest TagStream where
  type Rs TagStream = TagStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/tagStream",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagStreamResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTagStreamResponse' smart constructor.
newtype TagStreamResponse = TagStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagStreamResponse' value with any optional fields omitted.
mkTagStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TagStreamResponse
mkTagStreamResponse responseStatus =
  TagStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrrsResponseStatus :: Lens.Lens' TagStreamResponse Core.Int
tsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

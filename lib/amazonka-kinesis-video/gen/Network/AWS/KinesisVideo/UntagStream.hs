{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.
--
-- In the request, you must provide the @StreamName@ or @StreamARN@ .
module Network.AWS.KinesisVideo.UntagStream
  ( -- * Creating a request
    UntagStream (..),
    mkUntagStream,

    -- ** Request lenses
    usTagKeyList,
    usStreamARN,
    usStreamName,

    -- * Destructuring the response
    UntagStreamResponse (..),
    mkUntagStreamResponse,

    -- ** Response lenses
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagStream' smart constructor.
data UntagStream = UntagStream'
  { -- | A list of the keys of the tags that you want to remove.
    tagKeyList :: Core.NonEmpty Types.TagKey,
    -- | The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
    streamARN :: Core.Maybe Types.StreamARN,
    -- | The name of the stream that you want to remove tags from.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagStream' value with any optional fields omitted.
mkUntagStream ::
  -- | 'tagKeyList'
  Core.NonEmpty Types.TagKey ->
  UntagStream
mkUntagStream tagKeyList =
  UntagStream'
    { tagKeyList,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | A list of the keys of the tags that you want to remove.
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTagKeyList :: Lens.Lens' UntagStream (Core.NonEmpty Types.TagKey)
usTagKeyList = Lens.field @"tagKeyList"
{-# DEPRECATED usTagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamARN :: Lens.Lens' UntagStream (Core.Maybe Types.StreamARN)
usStreamARN = Lens.field @"streamARN"
{-# DEPRECATED usStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamName :: Lens.Lens' UntagStream (Core.Maybe Types.StreamName)
usStreamName = Lens.field @"streamName"
{-# DEPRECATED usStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON UntagStream where
  toJSON UntagStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TagKeyList" Core..= tagKeyList),
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest UntagStream where
  type Rs UntagStream = UntagStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/untagStream",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagStreamResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUntagStreamResponse' smart constructor.
newtype UntagStreamResponse = UntagStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagStreamResponse' value with any optional fields omitted.
mkUntagStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UntagStreamResponse
mkUntagStreamResponse responseStatus =
  UntagStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UntagStreamResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

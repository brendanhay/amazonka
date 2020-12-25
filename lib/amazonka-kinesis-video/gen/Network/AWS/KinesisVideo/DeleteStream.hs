{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis video stream and the data contained in the stream.
--
-- This method marks the stream for deletion, and makes the data in the stream inaccessible immediately.
--
-- To ensure that you have the latest version of the stream before deleting it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the @DescribeStream@ API.
-- This operation requires permission for the @KinesisVideo:DeleteStream@ action.
module Network.AWS.KinesisVideo.DeleteStream
  ( -- * Creating a request
    DeleteStream (..),
    mkDeleteStream,

    -- ** Request lenses
    dsStreamARN,
    dsCurrentVersion,

    -- * Destructuring the response
    DeleteStreamResponse (..),
    mkDeleteStreamResponse,

    -- ** Response lenses
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | The Amazon Resource Name (ARN) of the stream that you want to delete.
    streamARN :: Types.ResourceARN,
    -- | Optional: The version of the stream that you want to delete.
    --
    -- Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API.
    -- If not specified, only the @CreationTime@ is checked before deleting the stream.
    currentVersion :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStream' value with any optional fields omitted.
mkDeleteStream ::
  -- | 'streamARN'
  Types.ResourceARN ->
  DeleteStream
mkDeleteStream streamARN =
  DeleteStream' {streamARN, currentVersion = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the stream that you want to delete.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamARN :: Lens.Lens' DeleteStream Types.ResourceARN
dsStreamARN = Lens.field @"streamARN"
{-# DEPRECATED dsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | Optional: The version of the stream that you want to delete.
--
-- Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API.
-- If not specified, only the @CreationTime@ is checked before deleting the stream.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCurrentVersion :: Lens.Lens' DeleteStream (Core.Maybe Types.Version)
dsCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED dsCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

instance Core.FromJSON DeleteStream where
  toJSON DeleteStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamARN" Core..= streamARN),
            ("CurrentVersion" Core..=) Core.<$> currentVersion
          ]
      )

instance Core.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/deleteStream",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
newtype DeleteStreamResponse = DeleteStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamResponse' value with any optional fields omitted.
mkDeleteStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteStreamResponse
mkDeleteStreamResponse responseStatus =
  DeleteStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteStreamResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

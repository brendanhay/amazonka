{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing stream. The stream version will be incremented by one.
module Network.AWS.IoT.UpdateStream
  ( -- * Creating a request
    UpdateStream (..),
    mkUpdateStream,

    -- ** Request lenses
    usStreamId,
    usDescription,
    usFiles,
    usRoleArn,

    -- * Destructuring the response
    UpdateStreamResponse (..),
    mkUpdateStreamResponse,

    -- ** Response lenses
    usrrsDescription,
    usrrsStreamArn,
    usrrsStreamId,
    usrrsStreamVersion,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { -- | The stream ID.
    streamId :: Types.StreamId,
    -- | The description of the stream.
    description :: Core.Maybe Types.StreamDescription,
    -- | The files associated with the stream.
    files :: Core.Maybe (Core.NonEmpty Types.StreamFile),
    -- | An IAM role that allows the IoT service principal assumes to access your S3 files.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStream' value with any optional fields omitted.
mkUpdateStream ::
  -- | 'streamId'
  Types.StreamId ->
  UpdateStream
mkUpdateStream streamId =
  UpdateStream'
    { streamId,
      description = Core.Nothing,
      files = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamId :: Lens.Lens' UpdateStream Types.StreamId
usStreamId = Lens.field @"streamId"
{-# DEPRECATED usStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateStream (Core.Maybe Types.StreamDescription)
usDescription = Lens.field @"description"
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The files associated with the stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usFiles :: Lens.Lens' UpdateStream (Core.Maybe (Core.NonEmpty Types.StreamFile))
usFiles = Lens.field @"files"
{-# DEPRECATED usFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRoleArn :: Lens.Lens' UpdateStream (Core.Maybe Types.RoleArn)
usRoleArn = Lens.field @"roleArn"
{-# DEPRECATED usRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON UpdateStream where
  toJSON UpdateStream {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            ("files" Core..=) Core.<$> files,
            ("roleArn" Core..=) Core.<$> roleArn
          ]
      )

instance Core.AWSRequest UpdateStream where
  type Rs UpdateStream = UpdateStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/streams/" Core.<> (Core.toText streamId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStreamResponse'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "streamArn")
            Core.<*> (x Core..:? "streamId")
            Core.<*> (x Core..:? "streamVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateStreamResponse' smart constructor.
data UpdateStreamResponse = UpdateStreamResponse'
  { -- | A description of the stream.
    description :: Core.Maybe Types.StreamDescription,
    -- | The stream ARN.
    streamArn :: Core.Maybe Types.StreamArn,
    -- | The stream ID.
    streamId :: Core.Maybe Types.StreamId,
    -- | The stream version.
    streamVersion :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStreamResponse' value with any optional fields omitted.
mkUpdateStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateStreamResponse
mkUpdateStreamResponse responseStatus =
  UpdateStreamResponse'
    { description = Core.Nothing,
      streamArn = Core.Nothing,
      streamId = Core.Nothing,
      streamVersion = Core.Nothing,
      responseStatus
    }

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsDescription :: Lens.Lens' UpdateStreamResponse (Core.Maybe Types.StreamDescription)
usrrsDescription = Lens.field @"description"
{-# DEPRECATED usrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsStreamArn :: Lens.Lens' UpdateStreamResponse (Core.Maybe Types.StreamArn)
usrrsStreamArn = Lens.field @"streamArn"
{-# DEPRECATED usrrsStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsStreamId :: Lens.Lens' UpdateStreamResponse (Core.Maybe Types.StreamId)
usrrsStreamId = Lens.field @"streamId"
{-# DEPRECATED usrrsStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsStreamVersion :: Lens.Lens' UpdateStreamResponse (Core.Maybe Core.Natural)
usrrsStreamVersion = Lens.field @"streamVersion"
{-# DEPRECATED usrrsStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateStreamResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

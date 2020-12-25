{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream.
module Network.AWS.IoT.CreateStream
  ( -- * Creating a request
    CreateStream (..),
    mkCreateStream,

    -- ** Request lenses
    csStreamId,
    csFiles,
    csRoleArn,
    csDescription,
    csTags,

    -- * Destructuring the response
    CreateStreamResponse (..),
    mkCreateStreamResponse,

    -- ** Response lenses
    csrrsDescription,
    csrrsStreamArn,
    csrrsStreamId,
    csrrsStreamVersion,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | The stream ID.
    streamId :: Types.StreamId,
    -- | The files to stream.
    files :: Core.NonEmpty Types.StreamFile,
    -- | An IAM role that allows the IoT service principal assumes to access your S3 files.
    roleArn :: Types.RoleArn,
    -- | A description of the stream.
    description :: Core.Maybe Types.StreamDescription,
    -- | Metadata which can be used to manage streams.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStream' value with any optional fields omitted.
mkCreateStream ::
  -- | 'streamId'
  Types.StreamId ->
  -- | 'files'
  Core.NonEmpty Types.StreamFile ->
  -- | 'roleArn'
  Types.RoleArn ->
  CreateStream
mkCreateStream streamId files roleArn =
  CreateStream'
    { streamId,
      files,
      roleArn,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamId :: Lens.Lens' CreateStream Types.StreamId
csStreamId = Lens.field @"streamId"
{-# DEPRECATED csStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The files to stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csFiles :: Lens.Lens' CreateStream (Core.NonEmpty Types.StreamFile)
csFiles = Lens.field @"files"
{-# DEPRECATED csFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleArn :: Lens.Lens' CreateStream Types.RoleArn
csRoleArn = Lens.field @"roleArn"
{-# DEPRECATED csRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateStream (Core.Maybe Types.StreamDescription)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Metadata which can be used to manage streams.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStream (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateStream where
  toJSON CreateStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("files" Core..= files),
            Core.Just ("roleArn" Core..= roleArn),
            ("description" Core..=) Core.<$> description,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateStream where
  type Rs CreateStream = CreateStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/streams/" Core.<> (Core.toText streamId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamResponse'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "streamArn")
            Core.<*> (x Core..:? "streamId")
            Core.<*> (x Core..:? "streamVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { -- | A description of the stream.
    description :: Core.Maybe Types.StreamDescription,
    -- | The stream ARN.
    streamArn :: Core.Maybe Types.StreamArn,
    -- | The stream ID.
    streamId :: Core.Maybe Types.StreamId,
    -- | The version of the stream.
    streamVersion :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamResponse' value with any optional fields omitted.
mkCreateStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStreamResponse
mkCreateStreamResponse responseStatus =
  CreateStreamResponse'
    { description = Core.Nothing,
      streamArn = Core.Nothing,
      streamId = Core.Nothing,
      streamVersion = Core.Nothing,
      responseStatus
    }

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsDescription :: Lens.Lens' CreateStreamResponse (Core.Maybe Types.StreamDescription)
csrrsDescription = Lens.field @"description"
{-# DEPRECATED csrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStreamArn :: Lens.Lens' CreateStreamResponse (Core.Maybe Types.StreamArn)
csrrsStreamArn = Lens.field @"streamArn"
{-# DEPRECATED csrrsStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStreamId :: Lens.Lens' CreateStreamResponse (Core.Maybe Types.StreamId)
csrrsStreamId = Lens.field @"streamId"
{-# DEPRECATED csrrsStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The version of the stream.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStreamVersion :: Lens.Lens' CreateStreamResponse (Core.Maybe Core.Natural)
csrrsStreamVersion = Lens.field @"streamVersion"
{-# DEPRECATED csrrsStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStreamResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

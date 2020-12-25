{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates stream metadata, such as the device name and media type.
--
-- You must provide the stream name or the Amazon Resource Name (ARN) of the stream.
-- To make sure that you have the latest version of the stream before updating it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the @DescribeStream@ API.
-- @UpdateStream@ is an asynchronous operation, and takes time to complete.
module Network.AWS.KinesisVideo.UpdateStream
  ( -- * Creating a request
    UpdateStream (..),
    mkUpdateStream,

    -- ** Request lenses
    uCurrentVersion,
    uDeviceName,
    uMediaType,
    uStreamARN,
    uStreamName,

    -- * Destructuring the response
    UpdateStreamResponse (..),
    mkUpdateStreamResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { -- | The version of the stream whose metadata you want to update.
    currentVersion :: Types.Version,
    -- | The name of the device that is writing to the stream.
    deviceName :: Core.Maybe Types.DeviceName,
    -- | The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> .
    --
    -- To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
    mediaType :: Core.Maybe Types.MediaType,
    -- | The ARN of the stream whose metadata you want to update.
    streamARN :: Core.Maybe Types.ResourceARN,
    -- | The name of the stream whose metadata you want to update.
    --
    -- The stream name is an identifier for the stream, and must be unique for each account and region.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStream' value with any optional fields omitted.
mkUpdateStream ::
  -- | 'currentVersion'
  Types.Version ->
  UpdateStream
mkUpdateStream currentVersion =
  UpdateStream'
    { currentVersion,
      deviceName = Core.Nothing,
      mediaType = Core.Nothing,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | The version of the stream whose metadata you want to update.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCurrentVersion :: Lens.Lens' UpdateStream Types.Version
uCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED uCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The name of the device that is writing to the stream.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDeviceName :: Lens.Lens' UpdateStream (Core.Maybe Types.DeviceName)
uDeviceName = Lens.field @"deviceName"
{-# DEPRECATED uDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> .
--
-- To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMediaType :: Lens.Lens' UpdateStream (Core.Maybe Types.MediaType)
uMediaType = Lens.field @"mediaType"
{-# DEPRECATED uMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The ARN of the stream whose metadata you want to update.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStreamARN :: Lens.Lens' UpdateStream (Core.Maybe Types.ResourceARN)
uStreamARN = Lens.field @"streamARN"
{-# DEPRECATED uStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream whose metadata you want to update.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStreamName :: Lens.Lens' UpdateStream (Core.Maybe Types.StreamName)
uStreamName = Lens.field @"streamName"
{-# DEPRECATED uStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON UpdateStream where
  toJSON UpdateStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CurrentVersion" Core..= currentVersion),
            ("DeviceName" Core..=) Core.<$> deviceName,
            ("MediaType" Core..=) Core.<$> mediaType,
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest UpdateStream where
  type Rs UpdateStream = UpdateStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/updateStream",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateStreamResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateStreamResponse' smart constructor.
newtype UpdateStreamResponse = UpdateStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStreamResponse' value with any optional fields omitted.
mkUpdateStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateStreamResponse
mkUpdateStreamResponse responseStatus =
  UpdateStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateStreamResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

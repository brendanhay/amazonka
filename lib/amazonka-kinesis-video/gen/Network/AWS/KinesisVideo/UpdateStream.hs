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
    uMediaType,
    uCurrentVersion,
    uStreamARN,
    uDeviceName,
    uStreamName,

    -- * Destructuring the response
    UpdateStreamResponse (..),
    mkUpdateStreamResponse,

    -- ** Response lenses
    usrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { -- | The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> .
    --
    -- To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
    mediaType :: Lude.Maybe Lude.Text,
    -- | The version of the stream whose metadata you want to update.
    currentVersion :: Lude.Text,
    -- | The ARN of the stream whose metadata you want to update.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The name of the device that is writing to the stream.
    deviceName :: Lude.Maybe Lude.Text,
    -- | The name of the stream whose metadata you want to update.
    --
    -- The stream name is an identifier for the stream, and must be unique for each account and region.
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStream' with the minimum fields required to make a request.
--
-- * 'mediaType' - The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> .
--
-- To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
-- * 'currentVersion' - The version of the stream whose metadata you want to update.
-- * 'streamARN' - The ARN of the stream whose metadata you want to update.
-- * 'deviceName' - The name of the device that is writing to the stream.
-- * 'streamName' - The name of the stream whose metadata you want to update.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
mkUpdateStream ::
  -- | 'currentVersion'
  Lude.Text ->
  UpdateStream
mkUpdateStream pCurrentVersion_ =
  UpdateStream'
    { mediaType = Lude.Nothing,
      currentVersion = pCurrentVersion_,
      streamARN = Lude.Nothing,
      deviceName = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> .
--
-- To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMediaType :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
uMediaType = Lens.lens (mediaType :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {mediaType = a} :: UpdateStream)
{-# DEPRECATED uMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The version of the stream whose metadata you want to update.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCurrentVersion :: Lens.Lens' UpdateStream Lude.Text
uCurrentVersion = Lens.lens (currentVersion :: UpdateStream -> Lude.Text) (\s a -> s {currentVersion = a} :: UpdateStream)
{-# DEPRECATED uCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The ARN of the stream whose metadata you want to update.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStreamARN :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
uStreamARN = Lens.lens (streamARN :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: UpdateStream)
{-# DEPRECATED uStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the device that is writing to the stream.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDeviceName :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
uDeviceName = Lens.lens (deviceName :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: UpdateStream)
{-# DEPRECATED uDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The name of the stream whose metadata you want to update.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStreamName :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
uStreamName = Lens.lens (streamName :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: UpdateStream)
{-# DEPRECATED uStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest UpdateStream where
  type Rs UpdateStream = UpdateStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateStream where
  toJSON UpdateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MediaType" Lude..=) Lude.<$> mediaType,
            Lude.Just ("CurrentVersion" Lude..= currentVersion),
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("DeviceName" Lude..=) Lude.<$> deviceName,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath UpdateStream where
  toPath = Lude.const "/updateStream"

instance Lude.ToQuery UpdateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStreamResponse' smart constructor.
newtype UpdateStreamResponse = UpdateStreamResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStreamResponse
mkUpdateStreamResponse pResponseStatus_ =
  UpdateStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateStreamResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

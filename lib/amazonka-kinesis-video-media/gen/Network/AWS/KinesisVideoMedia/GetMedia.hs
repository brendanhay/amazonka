{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.GetMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to retrieve media content from a Kinesis video stream. In the request, you identify the stream name or stream Amazon Resource Name (ARN), and the starting chunk. Kinesis Video Streams then returns a stream of chunks in order by fragment number.
--
-- When you put media data (fragments) on a stream, Kinesis Video Streams stores each incoming fragment and related metadata in what is called a "chunk." For more information, see <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia> . The @GetMedia@ API returns a stream of these chunks starting from the chunk that you specify in the request.
-- The following limits apply when using the @GetMedia@ API:
--
--     * A client can call @GetMedia@ up to five times per second per stream.
--
--
--     * Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a @GetMedia@ session.
module Network.AWS.KinesisVideoMedia.GetMedia
  ( -- * Creating a request
    GetMedia (..),
    mkGetMedia,

    -- ** Request lenses
    gmStartSelector,
    gmStreamARN,
    gmStreamName,

    -- * Destructuring the response
    GetMediaResponse (..),
    mkGetMediaResponse,

    -- ** Response lenses
    gmrsPayload,
    gmrsContentType,
    gmrsResponseStatus,
  )
where

import Network.AWS.KinesisVideoMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMedia' smart constructor.
data GetMedia = GetMedia'
  { -- | Identifies the starting chunk to get from the specified stream.
    startSelector :: StartSelector,
    -- | The ARN of the stream from where you want to get the media content. If you don't specify the @streamARN@ , you must specify the @streamName@ .
    streamARN :: Lude.Maybe Lude.Text,
    -- | The Kinesis video stream name from where you want to get the media content. If you don't specify the @streamName@ , you must specify the @streamARN@ .
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMedia' with the minimum fields required to make a request.
--
-- * 'startSelector' - Identifies the starting chunk to get from the specified stream.
-- * 'streamARN' - The ARN of the stream from where you want to get the media content. If you don't specify the @streamARN@ , you must specify the @streamName@ .
-- * 'streamName' - The Kinesis video stream name from where you want to get the media content. If you don't specify the @streamName@ , you must specify the @streamARN@ .
mkGetMedia ::
  -- | 'startSelector'
  StartSelector ->
  GetMedia
mkGetMedia pStartSelector_ =
  GetMedia'
    { startSelector = pStartSelector_,
      streamARN = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | Identifies the starting chunk to get from the specified stream.
--
-- /Note:/ Consider using 'startSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmStartSelector :: Lens.Lens' GetMedia StartSelector
gmStartSelector = Lens.lens (startSelector :: GetMedia -> StartSelector) (\s a -> s {startSelector = a} :: GetMedia)
{-# DEPRECATED gmStartSelector "Use generic-lens or generic-optics with 'startSelector' instead." #-}

-- | The ARN of the stream from where you want to get the media content. If you don't specify the @streamARN@ , you must specify the @streamName@ .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmStreamARN :: Lens.Lens' GetMedia (Lude.Maybe Lude.Text)
gmStreamARN = Lens.lens (streamARN :: GetMedia -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: GetMedia)
{-# DEPRECATED gmStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The Kinesis video stream name from where you want to get the media content. If you don't specify the @streamName@ , you must specify the @streamARN@ .
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmStreamName :: Lens.Lens' GetMedia (Lude.Maybe Lude.Text)
gmStreamName = Lens.lens (streamName :: GetMedia -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: GetMedia)
{-# DEPRECATED gmStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest GetMedia where
  type Rs GetMedia = GetMediaResponse
  request = Req.postJSON kinesisVideoMediaService
  response =
    Res.receiveBody
      ( \s h x ->
          GetMediaResponse'
            Lude.<$> (Lude.pure x)
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMedia where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetMedia where
  toJSON GetMedia' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartSelector" Lude..= startSelector),
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath GetMedia where
  toPath = Lude.const "/getMedia"

instance Lude.ToQuery GetMedia where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMediaResponse' smart constructor.
data GetMediaResponse = GetMediaResponse'
  { -- | The payload Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see . The chunks that Kinesis Video Streams returns in the @GetMedia@ call also include the following additional Matroska (MKV) tags:
    --
    --
    --     * AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event your @GetMedia@ call terminates, you can use this continuation token in your next request to get the next chunk where the last request terminated.
    --
    --
    --     * AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client applications can use this tag value to determine how far behind the chunk returned in the response is from the latest chunk on the stream.
    --
    --
    --     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.
    --
    --
    --     * AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the fragment.
    --
    --
    --     * AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the fragment.
    --
    --
    -- The following tags will be present if an error occurs:
    --
    --     * AWS_KINESISVIDEO_ERROR_CODE - String description of an error that caused GetMedia to stop.
    --
    --
    --     * AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
    --
    --
    -- The error codes are as follows:
    --
    --     * 3002 - Error writing to the stream
    --
    --
    --     * 4000 - Requested fragment is not found
    --
    --
    --     * 4500 - Access denied for the stream's KMS key
    --
    --
    --     * 4501 - Stream's KMS key is disabled
    --
    --
    --     * 4502 - Validation error on the stream's KMS key
    --
    --
    --     * 4503 - KMS key specified in the stream is unavailable
    --
    --
    --     * 4504 - Invalid usage of the KMS key specified in the stream
    --
    --
    --     * 4505 - Invalid state of the KMS key specified in the stream
    --
    --
    --     * 4506 - Unable to find the KMS key specified in the stream
    --
    --
    --     * 5000 - Internal error
    payload :: Lude.RsBody,
    -- | The content type of the requested media.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'GetMediaResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The payload Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see . The chunks that Kinesis Video Streams returns in the @GetMedia@ call also include the following additional Matroska (MKV) tags:
--
--
--     * AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event your @GetMedia@ call terminates, you can use this continuation token in your next request to get the next chunk where the last request terminated.
--
--
--     * AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client applications can use this tag value to determine how far behind the chunk returned in the response is from the latest chunk on the stream.
--
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.
--
--
--     * AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the fragment.
--
--
--     * AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the fragment.
--
--
-- The following tags will be present if an error occurs:
--
--     * AWS_KINESISVIDEO_ERROR_CODE - String description of an error that caused GetMedia to stop.
--
--
--     * AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
--
--
-- The error codes are as follows:
--
--     * 3002 - Error writing to the stream
--
--
--     * 4000 - Requested fragment is not found
--
--
--     * 4500 - Access denied for the stream's KMS key
--
--
--     * 4501 - Stream's KMS key is disabled
--
--
--     * 4502 - Validation error on the stream's KMS key
--
--
--     * 4503 - KMS key specified in the stream is unavailable
--
--
--     * 4504 - Invalid usage of the KMS key specified in the stream
--
--
--     * 4505 - Invalid state of the KMS key specified in the stream
--
--
--     * 4506 - Unable to find the KMS key specified in the stream
--
--
--     * 5000 - Internal error
--
--
-- * 'contentType' - The content type of the requested media.
-- * 'responseStatus' - The response status code.
mkGetMediaResponse ::
  -- | 'payload'
  Lude.RsBody ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMediaResponse
mkGetMediaResponse pPayload_ pResponseStatus_ =
  GetMediaResponse'
    { payload = pPayload_,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The payload Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see . The chunks that Kinesis Video Streams returns in the @GetMedia@ call also include the following additional Matroska (MKV) tags:
--
--
--     * AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event your @GetMedia@ call terminates, you can use this continuation token in your next request to get the next chunk where the last request terminated.
--
--
--     * AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client applications can use this tag value to determine how far behind the chunk returned in the response is from the latest chunk on the stream.
--
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.
--
--
--     * AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the fragment.
--
--
--     * AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the fragment.
--
--
-- The following tags will be present if an error occurs:
--
--     * AWS_KINESISVIDEO_ERROR_CODE - String description of an error that caused GetMedia to stop.
--
--
--     * AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
--
--
-- The error codes are as follows:
--
--     * 3002 - Error writing to the stream
--
--
--     * 4000 - Requested fragment is not found
--
--
--     * 4500 - Access denied for the stream's KMS key
--
--
--     * 4501 - Stream's KMS key is disabled
--
--
--     * 4502 - Validation error on the stream's KMS key
--
--
--     * 4503 - KMS key specified in the stream is unavailable
--
--
--     * 4504 - Invalid usage of the KMS key specified in the stream
--
--
--     * 4505 - Invalid state of the KMS key specified in the stream
--
--
--     * 4506 - Unable to find the KMS key specified in the stream
--
--
--     * 5000 - Internal error
--
--
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsPayload :: Lens.Lens' GetMediaResponse Lude.RsBody
gmrsPayload = Lens.lens (payload :: GetMediaResponse -> Lude.RsBody) (\s a -> s {payload = a} :: GetMediaResponse)
{-# DEPRECATED gmrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The content type of the requested media.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsContentType :: Lens.Lens' GetMediaResponse (Lude.Maybe Lude.Text)
gmrsContentType = Lens.lens (contentType :: GetMediaResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetMediaResponse)
{-# DEPRECATED gmrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsResponseStatus :: Lens.Lens' GetMediaResponse Lude.Int
gmrsResponseStatus = Lens.lens (responseStatus :: GetMediaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMediaResponse)
{-# DEPRECATED gmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

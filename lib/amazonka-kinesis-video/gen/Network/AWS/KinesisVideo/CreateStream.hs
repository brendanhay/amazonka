{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.CreateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Kinesis video stream.
--
-- When you create a new stream, Kinesis Video Streams assigns it a version number. When you change the stream's metadata, Kinesis Video Streams updates the version.
-- @CreateStream@ is an asynchronous operation.
-- For information about how the service works, see <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/how-it-works.html How it Works> .
-- You must have permissions for the @KinesisVideo:CreateStream@ action.
module Network.AWS.KinesisVideo.CreateStream
  ( -- * Creating a request
    CreateStream (..),
    mkCreateStream,

    -- ** Request lenses
    csMediaType,
    csDataRetentionInHours,
    csKMSKeyId,
    csDeviceName,
    csTags,
    csStreamName,

    -- * Destructuring the response
    CreateStreamResponse (..),
    mkCreateStreamResponse,

    -- ** Response lenses
    csrsStreamARN,
    csrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { mediaType ::
      Lude.Maybe Lude.Text,
    dataRetentionInHours :: Lude.Maybe Lude.Natural,
    kmsKeyId :: Lude.Maybe Lude.Text,
    deviceName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    streamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- * 'dataRetentionInHours' - The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
-- When the @DataRetentionInHours@ value is 0, consumers can still consume the fragments that remain in the service host buffer, which has a retention time limit of 5 minutes and a retention memory limit of 200 MB. Fragments are removed from the buffer when either limit is reached.
-- * 'deviceName' - The name of the device that is writing to the stream.
-- * 'kmsKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> .
-- * 'mediaType' - The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines.
--
-- Example valid values include "video/h264" and "video/h264,audio/aac".
-- This parameter is optional; the default value is @null@ (or empty in JSON).
-- * 'streamName' - A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
-- * 'tags' - A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
mkCreateStream ::
  -- | 'streamName'
  Lude.Text ->
  CreateStream
mkCreateStream pStreamName_ =
  CreateStream'
    { mediaType = Lude.Nothing,
      dataRetentionInHours = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      deviceName = Lude.Nothing,
      tags = Lude.Nothing,
      streamName = pStreamName_
    }

-- | The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines.
--
-- Example valid values include "video/h264" and "video/h264,audio/aac".
-- This parameter is optional; the default value is @null@ (or empty in JSON).
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMediaType :: Lens.Lens' CreateStream (Lude.Maybe Lude.Text)
csMediaType = Lens.lens (mediaType :: CreateStream -> Lude.Maybe Lude.Text) (\s a -> s {mediaType = a} :: CreateStream)
{-# DEPRECATED csMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
-- When the @DataRetentionInHours@ value is 0, consumers can still consume the fragments that remain in the service host buffer, which has a retention time limit of 5 minutes and a retention memory limit of 200 MB. Fragments are removed from the buffer when either limit is reached.
--
-- /Note:/ Consider using 'dataRetentionInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDataRetentionInHours :: Lens.Lens' CreateStream (Lude.Maybe Lude.Natural)
csDataRetentionInHours = Lens.lens (dataRetentionInHours :: CreateStream -> Lude.Maybe Lude.Natural) (\s a -> s {dataRetentionInHours = a} :: CreateStream)
{-# DEPRECATED csDataRetentionInHours "Use generic-lens or generic-optics with 'dataRetentionInHours' instead." #-}

-- | The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKMSKeyId :: Lens.Lens' CreateStream (Lude.Maybe Lude.Text)
csKMSKeyId = Lens.lens (kmsKeyId :: CreateStream -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateStream)
{-# DEPRECATED csKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the device that is writing to the stream.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeviceName :: Lens.Lens' CreateStream (Lude.Maybe Lude.Text)
csDeviceName = Lens.lens (deviceName :: CreateStream -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: CreateStream)
{-# DEPRECATED csDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStream (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csTags = Lens.lens (tags :: CreateStream -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateStream)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CreateStream Lude.Text
csStreamName = Lens.lens (streamName :: CreateStream -> Lude.Text) (\s a -> s {streamName = a} :: CreateStream)
{-# DEPRECATED csStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest CreateStream where
  type Rs CreateStream = CreateStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStreamResponse'
            Lude.<$> (x Lude..?> "StreamARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MediaType" Lude..=) Lude.<$> mediaType,
            ("DataRetentionInHours" Lude..=) Lude.<$> dataRetentionInHours,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("DeviceName" Lude..=) Lude.<$> deviceName,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath CreateStream where
  toPath = Lude.const "/createStream"

instance Lude.ToQuery CreateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { streamARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream.
mkCreateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamResponse
mkCreateStreamResponse pResponseStatus_ =
  CreateStreamResponse'
    { streamARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStreamARN :: Lens.Lens' CreateStreamResponse (Lude.Maybe Lude.Text)
csrsStreamARN = Lens.lens (streamARN :: CreateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: CreateStreamResponse)
{-# DEPRECATED csrsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateStreamResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

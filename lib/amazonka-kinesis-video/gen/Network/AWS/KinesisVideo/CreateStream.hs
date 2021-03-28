{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStream (..)
    , mkCreateStream
    -- ** Request lenses
    , csStreamName
    , csDataRetentionInHours
    , csDeviceName
    , csKmsKeyId
    , csMediaType
    , csTags

    -- * Destructuring the response
    , CreateStreamResponse (..)
    , mkCreateStreamResponse
    -- ** Response lenses
    , csrrsStreamARN
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { streamName :: Types.StreamName
    -- ^ A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
  , dataRetentionInHours :: Core.Maybe Core.Natural
    -- ^ The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
-- When the @DataRetentionInHours@ value is 0, consumers can still consume the fragments that remain in the service host buffer, which has a retention time limit of 5 minutes and a retention memory limit of 200 MB. Fragments are removed from the buffer when either limit is reached.
  , deviceName :: Core.Maybe Types.DeviceName
    -- ^ The name of the device that is writing to the stream. 
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> . 
  , mediaType :: Core.Maybe Types.MediaType
    -- ^ The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines.
--
-- Example valid values include "video/h264" and "video/h264,audio/aac".
-- This parameter is optional; the default value is @null@ (or empty in JSON).
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStream' value with any optional fields omitted.
mkCreateStream
    :: Types.StreamName -- ^ 'streamName'
    -> CreateStream
mkCreateStream streamName
  = CreateStream'{streamName, dataRetentionInHours = Core.Nothing,
                  deviceName = Core.Nothing, kmsKeyId = Core.Nothing,
                  mediaType = Core.Nothing, tags = Core.Nothing}

-- | A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for each account and region.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CreateStream Types.StreamName
csStreamName = Lens.field @"streamName"
{-# INLINEABLE csStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
-- When the @DataRetentionInHours@ value is 0, consumers can still consume the fragments that remain in the service host buffer, which has a retention time limit of 5 minutes and a retention memory limit of 200 MB. Fragments are removed from the buffer when either limit is reached.
--
-- /Note:/ Consider using 'dataRetentionInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDataRetentionInHours :: Lens.Lens' CreateStream (Core.Maybe Core.Natural)
csDataRetentionInHours = Lens.field @"dataRetentionInHours"
{-# INLINEABLE csDataRetentionInHours #-}
{-# DEPRECATED dataRetentionInHours "Use generic-lens or generic-optics with 'dataRetentionInHours' instead"  #-}

-- | The name of the device that is writing to the stream. 
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeviceName :: Lens.Lens' CreateStream (Core.Maybe Types.DeviceName)
csDeviceName = Lens.field @"deviceName"
{-# INLINEABLE csDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> . 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKmsKeyId :: Lens.Lens' CreateStream (Core.Maybe Types.KmsKeyId)
csKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE csKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines.
--
-- Example valid values include "video/h264" and "video/h264,audio/aac".
-- This parameter is optional; the default value is @null@ (or empty in JSON).
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMediaType :: Lens.Lens' CreateStream (Core.Maybe Types.MediaType)
csMediaType = Lens.field @"mediaType"
{-# INLINEABLE csMediaType #-}
{-# DEPRECATED mediaType "Use generic-lens or generic-optics with 'mediaType' instead"  #-}

-- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStream (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
csTags = Lens.field @"tags"
{-# INLINEABLE csTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateStream where
        toJSON CreateStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  ("DataRetentionInHours" Core..=) Core.<$> dataRetentionInHours,
                  ("DeviceName" Core..=) Core.<$> deviceName,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("MediaType" Core..=) Core.<$> mediaType,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateStream where
        type Rs CreateStream = CreateStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/createStream",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateStreamResponse' Core.<$>
                   (x Core..:? "StreamARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { streamARN :: Core.Maybe Types.StreamARN
    -- ^ The Amazon Resource Name (ARN) of the stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamResponse' value with any optional fields omitted.
mkCreateStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStreamResponse
mkCreateStreamResponse responseStatus
  = CreateStreamResponse'{streamARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStreamARN :: Lens.Lens' CreateStreamResponse (Core.Maybe Types.StreamARN)
csrrsStreamARN = Lens.field @"streamARN"
{-# INLINEABLE csrrsStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStreamResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

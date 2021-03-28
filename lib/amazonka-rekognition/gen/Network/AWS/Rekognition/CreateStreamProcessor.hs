{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Rekognition stream processor that you can use to detect and recognize faces in a streaming video.
--
-- Amazon Rekognition Video is a consumer of live video from Amazon Kinesis Video Streams. Amazon Rekognition Video sends analysis results to Amazon Kinesis Data Streams.
-- You provide as input a Kinesis video stream (@Input@ ) and a Kinesis data stream (@Output@ ) stream. You also specify the face recognition criteria in @Settings@ . For example, the collection containing faces that you want to recognize. Use @Name@ to assign an identifier for the stream processor. You use @Name@ to manage the stream processor. For example, you can start processing the source video by calling 'StartStreamProcessor' with the @Name@ field. 
-- After you have finished analyzing a streaming video, use 'StopStreamProcessor' to stop processing. You can delete the stream processor by calling 'DeleteStreamProcessor' .
module Network.AWS.Rekognition.CreateStreamProcessor
    (
    -- * Creating a request
      CreateStreamProcessor (..)
    , mkCreateStreamProcessor
    -- ** Request lenses
    , cspInput
    , cspOutput
    , cspName
    , cspSettings
    , cspRoleArn

    -- * Destructuring the response
    , CreateStreamProcessorResponse (..)
    , mkCreateStreamProcessorResponse
    -- ** Response lenses
    , csprrsStreamProcessorArn
    , csprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStreamProcessor' smart constructor.
data CreateStreamProcessor = CreateStreamProcessor'
  { input :: Types.StreamProcessorInput
    -- ^ Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
  , output :: Types.StreamProcessorOutput
    -- ^ Kinesis data stream stream to which Amazon Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
  , name :: Types.StreamProcessorName
    -- ^ An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling 'DescribeStreamProcessor' . @Name@ is idempotent. 
  , settings :: Types.StreamProcessorSettings
    -- ^ Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
  , roleArn :: Types.RoleArn
    -- ^ ARN of the IAM role that allows access to the stream processor.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamProcessor' value with any optional fields omitted.
mkCreateStreamProcessor
    :: Types.StreamProcessorInput -- ^ 'input'
    -> Types.StreamProcessorOutput -- ^ 'output'
    -> Types.StreamProcessorName -- ^ 'name'
    -> Types.StreamProcessorSettings -- ^ 'settings'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateStreamProcessor
mkCreateStreamProcessor input output name settings roleArn
  = CreateStreamProcessor'{input, output, name, settings, roleArn}

-- | Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspInput :: Lens.Lens' CreateStreamProcessor Types.StreamProcessorInput
cspInput = Lens.field @"input"
{-# INLINEABLE cspInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | Kinesis data stream stream to which Amazon Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspOutput :: Lens.Lens' CreateStreamProcessor Types.StreamProcessorOutput
cspOutput = Lens.field @"output"
{-# INLINEABLE cspOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling 'DescribeStreamProcessor' . @Name@ is idempotent. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspName :: Lens.Lens' CreateStreamProcessor Types.StreamProcessorName
cspName = Lens.field @"name"
{-# INLINEABLE cspName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSettings :: Lens.Lens' CreateStreamProcessor Types.StreamProcessorSettings
cspSettings = Lens.field @"settings"
{-# INLINEABLE cspSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | ARN of the IAM role that allows access to the stream processor.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspRoleArn :: Lens.Lens' CreateStreamProcessor Types.RoleArn
cspRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cspRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery CreateStreamProcessor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStreamProcessor where
        toHeaders CreateStreamProcessor{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.CreateStreamProcessor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateStreamProcessor where
        toJSON CreateStreamProcessor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Input" Core..= input),
                  Core.Just ("Output" Core..= output),
                  Core.Just ("Name" Core..= name),
                  Core.Just ("Settings" Core..= settings),
                  Core.Just ("RoleArn" Core..= roleArn)])

instance Core.AWSRequest CreateStreamProcessor where
        type Rs CreateStreamProcessor = CreateStreamProcessorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateStreamProcessorResponse' Core.<$>
                   (x Core..:? "StreamProcessorArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { streamProcessorArn :: Core.Maybe Types.StreamProcessorArn
    -- ^ ARN for the newly create stream processor.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamProcessorResponse' value with any optional fields omitted.
mkCreateStreamProcessorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStreamProcessorResponse
mkCreateStreamProcessorResponse responseStatus
  = CreateStreamProcessorResponse'{streamProcessorArn = Core.Nothing,
                                   responseStatus}

-- | ARN for the newly create stream processor.
--
-- /Note:/ Consider using 'streamProcessorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsStreamProcessorArn :: Lens.Lens' CreateStreamProcessorResponse (Core.Maybe Types.StreamProcessorArn)
csprrsStreamProcessorArn = Lens.field @"streamProcessorArn"
{-# INLINEABLE csprrsStreamProcessorArn #-}
{-# DEPRECATED streamProcessorArn "Use generic-lens or generic-optics with 'streamProcessorArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsResponseStatus :: Lens.Lens' CreateStreamProcessorResponse Core.Int
csprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

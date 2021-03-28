{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a stream processor created by 'CreateStreamProcessor' . You can get information about the input and output streams, the input parameters for the face recognition being performed, and the current status of the stream processor.
module Network.AWS.Rekognition.DescribeStreamProcessor
    (
    -- * Creating a request
      DescribeStreamProcessor (..)
    , mkDescribeStreamProcessor
    -- ** Request lenses
    , dspName

    -- * Destructuring the response
    , DescribeStreamProcessorResponse (..)
    , mkDescribeStreamProcessorResponse
    -- ** Response lenses
    , dsprrsCreationTimestamp
    , dsprrsInput
    , dsprrsLastUpdateTimestamp
    , dsprrsName
    , dsprrsOutput
    , dsprrsRoleArn
    , dsprrsSettings
    , dsprrsStatus
    , dsprrsStatusMessage
    , dsprrsStreamProcessorArn
    , dsprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStreamProcessor' smart constructor.
newtype DescribeStreamProcessor = DescribeStreamProcessor'
  { name :: Types.StreamProcessorName
    -- ^ Name of the stream processor for which you want information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStreamProcessor' value with any optional fields omitted.
mkDescribeStreamProcessor
    :: Types.StreamProcessorName -- ^ 'name'
    -> DescribeStreamProcessor
mkDescribeStreamProcessor name = DescribeStreamProcessor'{name}

-- | Name of the stream processor for which you want information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspName :: Lens.Lens' DescribeStreamProcessor Types.StreamProcessorName
dspName = Lens.field @"name"
{-# INLINEABLE dspName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DescribeStreamProcessor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStreamProcessor where
        toHeaders DescribeStreamProcessor{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DescribeStreamProcessor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStreamProcessor where
        toJSON DescribeStreamProcessor{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeStreamProcessor where
        type Rs DescribeStreamProcessor = DescribeStreamProcessorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStreamProcessorResponse' Core.<$>
                   (x Core..:? "CreationTimestamp") Core.<*> x Core..:? "Input"
                     Core.<*> x Core..:? "LastUpdateTimestamp"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "Output"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> x Core..:? "Settings"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusMessage"
                     Core.<*> x Core..:? "StreamProcessorArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { creationTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ Date and time the stream processor was created
  , input :: Core.Maybe Types.StreamProcessorInput
    -- ^ Kinesis video stream that provides the source streaming video.
  , lastUpdateTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
  , name :: Core.Maybe Types.StreamProcessorName
    -- ^ Name of the stream processor. 
  , output :: Core.Maybe Types.StreamProcessorOutput
    -- ^ Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ ARN of the IAM role that allows access to the stream processor.
  , settings :: Core.Maybe Types.StreamProcessorSettings
    -- ^ Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
  , status :: Core.Maybe Types.StreamProcessorStatus
    -- ^ Current status of the stream processor.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ Detailed status message about the stream processor.
  , streamProcessorArn :: Core.Maybe Types.StreamProcessorArn
    -- ^ ARN of the stream processor.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStreamProcessorResponse' value with any optional fields omitted.
mkDescribeStreamProcessorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStreamProcessorResponse
mkDescribeStreamProcessorResponse responseStatus
  = DescribeStreamProcessorResponse'{creationTimestamp =
                                       Core.Nothing,
                                     input = Core.Nothing, lastUpdateTimestamp = Core.Nothing,
                                     name = Core.Nothing, output = Core.Nothing,
                                     roleArn = Core.Nothing, settings = Core.Nothing,
                                     status = Core.Nothing, statusMessage = Core.Nothing,
                                     streamProcessorArn = Core.Nothing, responseStatus}

-- | Date and time the stream processor was created
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsCreationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Core.NominalDiffTime)
dsprrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE dsprrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | Kinesis video stream that provides the source streaming video.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsInput :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorInput)
dsprrsInput = Lens.field @"input"
{-# INLINEABLE dsprrsInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsLastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Core.NominalDiffTime)
dsprrsLastUpdateTimestamp = Lens.field @"lastUpdateTimestamp"
{-# INLINEABLE dsprrsLastUpdateTimestamp #-}
{-# DEPRECATED lastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead"  #-}

-- | Name of the stream processor. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsName :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorName)
dsprrsName = Lens.field @"name"
{-# INLINEABLE dsprrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsOutput :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorOutput)
dsprrsOutput = Lens.field @"output"
{-# INLINEABLE dsprrsOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | ARN of the IAM role that allows access to the stream processor.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsRoleArn :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.RoleArn)
dsprrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dsprrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsSettings :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorSettings)
dsprrsSettings = Lens.field @"settings"
{-# INLINEABLE dsprrsSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | Current status of the stream processor.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStatus :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorStatus)
dsprrsStatus = Lens.field @"status"
{-# INLINEABLE dsprrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Detailed status message about the stream processor.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStatusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Core.Text)
dsprrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE dsprrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | ARN of the stream processor.
--
-- /Note:/ Consider using 'streamProcessorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStreamProcessorArn :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorArn)
dsprrsStreamProcessorArn = Lens.field @"streamProcessorArn"
{-# INLINEABLE dsprrsStreamProcessorArn #-}
{-# DEPRECATED streamProcessorArn "Use generic-lens or generic-optics with 'streamProcessorArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DescribeStreamProcessorResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

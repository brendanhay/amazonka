{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeStreamProcessor (..),
    mkDescribeStreamProcessor,

    -- ** Request lenses
    dspName,

    -- * Destructuring the response
    DescribeStreamProcessorResponse (..),
    mkDescribeStreamProcessorResponse,

    -- ** Response lenses
    dsprrsCreationTimestamp,
    dsprrsInput,
    dsprrsLastUpdateTimestamp,
    dsprrsName,
    dsprrsOutput,
    dsprrsRoleArn,
    dsprrsSettings,
    dsprrsStatus,
    dsprrsStatusMessage,
    dsprrsStreamProcessorArn,
    dsprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStreamProcessor' smart constructor.
newtype DescribeStreamProcessor = DescribeStreamProcessor'
  { -- | Name of the stream processor for which you want information.
    name :: Types.StreamProcessorName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStreamProcessor' value with any optional fields omitted.
mkDescribeStreamProcessor ::
  -- | 'name'
  Types.StreamProcessorName ->
  DescribeStreamProcessor
mkDescribeStreamProcessor name = DescribeStreamProcessor' {name}

-- | Name of the stream processor for which you want information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspName :: Lens.Lens' DescribeStreamProcessor Types.StreamProcessorName
dspName = Lens.field @"name"
{-# DEPRECATED dspName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeStreamProcessor where
  toJSON DescribeStreamProcessor {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeStreamProcessor where
  type Rs DescribeStreamProcessor = DescribeStreamProcessorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.DescribeStreamProcessor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamProcessorResponse'
            Core.<$> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Input")
            Core.<*> (x Core..:? "LastUpdateTimestamp")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Output")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "Settings")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "StreamProcessorArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { -- | Date and time the stream processor was created
    creationTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | Kinesis video stream that provides the source streaming video.
    input :: Core.Maybe Types.StreamProcessorInput,
    -- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
    lastUpdateTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | Name of the stream processor.
    name :: Core.Maybe Types.StreamProcessorName,
    -- | Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
    output :: Core.Maybe Types.StreamProcessorOutput,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
    settings :: Core.Maybe Types.StreamProcessorSettings,
    -- | Current status of the stream processor.
    status :: Core.Maybe Types.StreamProcessorStatus,
    -- | Detailed status message about the stream processor.
    statusMessage :: Core.Maybe Types.String,
    -- | ARN of the stream processor.
    streamProcessorArn :: Core.Maybe Types.StreamProcessorArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStreamProcessorResponse' value with any optional fields omitted.
mkDescribeStreamProcessorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStreamProcessorResponse
mkDescribeStreamProcessorResponse responseStatus =
  DescribeStreamProcessorResponse'
    { creationTimestamp =
        Core.Nothing,
      input = Core.Nothing,
      lastUpdateTimestamp = Core.Nothing,
      name = Core.Nothing,
      output = Core.Nothing,
      roleArn = Core.Nothing,
      settings = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      streamProcessorArn = Core.Nothing,
      responseStatus
    }

-- | Date and time the stream processor was created
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsCreationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Core.NominalDiffTime)
dsprrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED dsprrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Kinesis video stream that provides the source streaming video.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsInput :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorInput)
dsprrsInput = Lens.field @"input"
{-# DEPRECATED dsprrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsLastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Core.NominalDiffTime)
dsprrsLastUpdateTimestamp = Lens.field @"lastUpdateTimestamp"
{-# DEPRECATED dsprrsLastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead." #-}

-- | Name of the stream processor.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsName :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorName)
dsprrsName = Lens.field @"name"
{-# DEPRECATED dsprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsOutput :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorOutput)
dsprrsOutput = Lens.field @"output"
{-# DEPRECATED dsprrsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | ARN of the IAM role that allows access to the stream processor.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsRoleArn :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.RoleArn)
dsprrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dsprrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsSettings :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorSettings)
dsprrsSettings = Lens.field @"settings"
{-# DEPRECATED dsprrsSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Current status of the stream processor.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStatus :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorStatus)
dsprrsStatus = Lens.field @"status"
{-# DEPRECATED dsprrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Detailed status message about the stream processor.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStatusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.String)
dsprrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED dsprrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | ARN of the stream processor.
--
-- /Note:/ Consider using 'streamProcessorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsStreamProcessorArn :: Lens.Lens' DescribeStreamProcessorResponse (Core.Maybe Types.StreamProcessorArn)
dsprrsStreamProcessorArn = Lens.field @"streamProcessorArn"
{-# DEPRECATED dsprrsStreamProcessorArn "Use generic-lens or generic-optics with 'streamProcessorArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DescribeStreamProcessorResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

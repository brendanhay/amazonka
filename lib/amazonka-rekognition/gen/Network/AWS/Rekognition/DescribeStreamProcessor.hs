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
    dsprsStatus,
    dsprsSettings,
    dsprsInput,
    dsprsOutput,
    dsprsStreamProcessorARN,
    dsprsStatusMessage,
    dsprsName,
    dsprsCreationTimestamp,
    dsprsLastUpdateTimestamp,
    dsprsRoleARN,
    dsprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStreamProcessor' smart constructor.
newtype DescribeStreamProcessor = DescribeStreamProcessor'
  { -- | Name of the stream processor for which you want information.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamProcessor' with the minimum fields required to make a request.
--
-- * 'name' - Name of the stream processor for which you want information.
mkDescribeStreamProcessor ::
  -- | 'name'
  Lude.Text ->
  DescribeStreamProcessor
mkDescribeStreamProcessor pName_ =
  DescribeStreamProcessor' {name = pName_}

-- | Name of the stream processor for which you want information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspName :: Lens.Lens' DescribeStreamProcessor Lude.Text
dspName = Lens.lens (name :: DescribeStreamProcessor -> Lude.Text) (\s a -> s {name = a} :: DescribeStreamProcessor)
{-# DEPRECATED dspName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeStreamProcessor where
  type Rs DescribeStreamProcessor = DescribeStreamProcessorResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamProcessorResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Settings")
            Lude.<*> (x Lude..?> "Input")
            Lude.<*> (x Lude..?> "Output")
            Lude.<*> (x Lude..?> "StreamProcessorArn")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "LastUpdateTimestamp")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStreamProcessor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DescribeStreamProcessor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStreamProcessor where
  toJSON DescribeStreamProcessor' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DescribeStreamProcessor where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStreamProcessor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { -- | Current status of the stream processor.
    status :: Lude.Maybe StreamProcessorStatus,
    -- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
    settings :: Lude.Maybe StreamProcessorSettings,
    -- | Kinesis video stream that provides the source streaming video.
    input :: Lude.Maybe StreamProcessorInput,
    -- | Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
    output :: Lude.Maybe StreamProcessorOutput,
    -- | ARN of the stream processor.
    streamProcessorARN :: Lude.Maybe Lude.Text,
    -- | Detailed status message about the stream processor.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | Name of the stream processor.
    name :: Lude.Maybe Lude.Text,
    -- | Date and time the stream processor was created
    creationTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
    lastUpdateTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamProcessorResponse' with the minimum fields required to make a request.
--
-- * 'status' - Current status of the stream processor.
-- * 'settings' - Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
-- * 'input' - Kinesis video stream that provides the source streaming video.
-- * 'output' - Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
-- * 'streamProcessorARN' - ARN of the stream processor.
-- * 'statusMessage' - Detailed status message about the stream processor.
-- * 'name' - Name of the stream processor.
-- * 'creationTimestamp' - Date and time the stream processor was created
-- * 'lastUpdateTimestamp' - The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
-- * 'roleARN' - ARN of the IAM role that allows access to the stream processor.
-- * 'responseStatus' - The response status code.
mkDescribeStreamProcessorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStreamProcessorResponse
mkDescribeStreamProcessorResponse pResponseStatus_ =
  DescribeStreamProcessorResponse'
    { status = Lude.Nothing,
      settings = Lude.Nothing,
      input = Lude.Nothing,
      output = Lude.Nothing,
      streamProcessorARN = Lude.Nothing,
      statusMessage = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      lastUpdateTimestamp = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Current status of the stream processor.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsStatus :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe StreamProcessorStatus)
dsprsStatus = Lens.lens (status :: DescribeStreamProcessorResponse -> Lude.Maybe StreamProcessorStatus) (\s a -> s {status = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsSettings :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe StreamProcessorSettings)
dsprsSettings = Lens.lens (settings :: DescribeStreamProcessorResponse -> Lude.Maybe StreamProcessorSettings) (\s a -> s {settings = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Kinesis video stream that provides the source streaming video.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsInput :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe StreamProcessorInput)
dsprsInput = Lens.lens (input :: DescribeStreamProcessorResponse -> Lude.Maybe StreamProcessorInput) (\s a -> s {input = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis results.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsOutput :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe StreamProcessorOutput)
dsprsOutput = Lens.lens (output :: DescribeStreamProcessorResponse -> Lude.Maybe StreamProcessorOutput) (\s a -> s {output = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | ARN of the stream processor.
--
-- /Note:/ Consider using 'streamProcessorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsStreamProcessorARN :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Text)
dsprsStreamProcessorARN = Lens.lens (streamProcessorARN :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamProcessorARN = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsStreamProcessorARN "Use generic-lens or generic-optics with 'streamProcessorARN' instead." #-}

-- | Detailed status message about the stream processor.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsStatusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Text)
dsprsStatusMessage = Lens.lens (statusMessage :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Name of the stream processor.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsName :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Text)
dsprsName = Lens.lens (name :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Date and time the stream processor was created
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsCreationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Timestamp)
dsprsCreationTimestamp = Lens.lens (creationTimestamp :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimestamp = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsLastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Timestamp)
dsprsLastUpdateTimestamp = Lens.lens (lastUpdateTimestamp :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTimestamp = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsLastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead." #-}

-- | ARN of the IAM role that allows access to the stream processor.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsRoleARN :: Lens.Lens' DescribeStreamProcessorResponse (Lude.Maybe Lude.Text)
dsprsRoleARN = Lens.lens (roleARN :: DescribeStreamProcessorResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DescribeStreamProcessorResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DescribeStreamProcessorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamProcessorResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

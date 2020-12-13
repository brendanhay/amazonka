{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateStreamProcessor (..),
    mkCreateStreamProcessor,

    -- ** Request lenses
    cspSettings,
    cspInput,
    cspOutput,
    cspName,
    cspRoleARN,

    -- * Destructuring the response
    CreateStreamProcessorResponse (..),
    mkCreateStreamProcessorResponse,

    -- ** Response lenses
    csprsStreamProcessorARN,
    csprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStreamProcessor' smart constructor.
data CreateStreamProcessor = CreateStreamProcessor'
  { -- | Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
    settings :: StreamProcessorSettings,
    -- | Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
    input :: StreamProcessorInput,
    -- | Kinesis data stream stream to which Amazon Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
    output :: StreamProcessorOutput,
    -- | An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling 'DescribeStreamProcessor' . @Name@ is idempotent.
    name :: Lude.Text,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamProcessor' with the minimum fields required to make a request.
--
-- * 'settings' - Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
-- * 'input' - Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
-- * 'output' - Kinesis data stream stream to which Amazon Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
-- * 'name' - An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling 'DescribeStreamProcessor' . @Name@ is idempotent.
-- * 'roleARN' - ARN of the IAM role that allows access to the stream processor.
mkCreateStreamProcessor ::
  -- | 'settings'
  StreamProcessorSettings ->
  -- | 'input'
  StreamProcessorInput ->
  -- | 'output'
  StreamProcessorOutput ->
  -- | 'name'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateStreamProcessor
mkCreateStreamProcessor
  pSettings_
  pInput_
  pOutput_
  pName_
  pRoleARN_ =
    CreateStreamProcessor'
      { settings = pSettings_,
        input = pInput_,
        output = pOutput_,
        name = pName_,
        roleARN = pRoleARN_
      }

-- | Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSettings :: Lens.Lens' CreateStreamProcessor StreamProcessorSettings
cspSettings = Lens.lens (settings :: CreateStreamProcessor -> StreamProcessorSettings) (\s a -> s {settings = a} :: CreateStreamProcessor)
{-# DEPRECATED cspSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspInput :: Lens.Lens' CreateStreamProcessor StreamProcessorInput
cspInput = Lens.lens (input :: CreateStreamProcessor -> StreamProcessorInput) (\s a -> s {input = a} :: CreateStreamProcessor)
{-# DEPRECATED cspInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Kinesis data stream stream to which Amazon Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspOutput :: Lens.Lens' CreateStreamProcessor StreamProcessorOutput
cspOutput = Lens.lens (output :: CreateStreamProcessor -> StreamProcessorOutput) (\s a -> s {output = a} :: CreateStreamProcessor)
{-# DEPRECATED cspOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling 'DescribeStreamProcessor' . @Name@ is idempotent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspName :: Lens.Lens' CreateStreamProcessor Lude.Text
cspName = Lens.lens (name :: CreateStreamProcessor -> Lude.Text) (\s a -> s {name = a} :: CreateStreamProcessor)
{-# DEPRECATED cspName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | ARN of the IAM role that allows access to the stream processor.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspRoleARN :: Lens.Lens' CreateStreamProcessor Lude.Text
cspRoleARN = Lens.lens (roleARN :: CreateStreamProcessor -> Lude.Text) (\s a -> s {roleARN = a} :: CreateStreamProcessor)
{-# DEPRECATED cspRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateStreamProcessor where
  type Rs CreateStreamProcessor = CreateStreamProcessorResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStreamProcessorResponse'
            Lude.<$> (x Lude..?> "StreamProcessorArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStreamProcessor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.CreateStreamProcessor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStreamProcessor where
  toJSON CreateStreamProcessor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Settings" Lude..= settings),
            Lude.Just ("Input" Lude..= input),
            Lude.Just ("Output" Lude..= output),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateStreamProcessor where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStreamProcessor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { -- | ARN for the newly create stream processor.
    streamProcessorARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamProcessorResponse' with the minimum fields required to make a request.
--
-- * 'streamProcessorARN' - ARN for the newly create stream processor.
-- * 'responseStatus' - The response status code.
mkCreateStreamProcessorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamProcessorResponse
mkCreateStreamProcessorResponse pResponseStatus_ =
  CreateStreamProcessorResponse'
    { streamProcessorARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | ARN for the newly create stream processor.
--
-- /Note:/ Consider using 'streamProcessorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsStreamProcessorARN :: Lens.Lens' CreateStreamProcessorResponse (Lude.Maybe Lude.Text)
csprsStreamProcessorARN = Lens.lens (streamProcessorARN :: CreateStreamProcessorResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamProcessorARN = a} :: CreateStreamProcessorResponse)
{-# DEPRECATED csprsStreamProcessorARN "Use generic-lens or generic-optics with 'streamProcessorARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsResponseStatus :: Lens.Lens' CreateStreamProcessorResponse Lude.Int
csprsResponseStatus = Lens.lens (responseStatus :: CreateStreamProcessorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamProcessorResponse)
{-# DEPRECATED csprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

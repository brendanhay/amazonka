{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectModerationLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects unsafe content in a specified JPEG or PNG format image. Use @DetectModerationLabels@ to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.
--
-- To filter images, use the labels returned by @DetectModerationLabels@ to determine which types of content are appropriate.
-- For information about moderation labels, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
module Network.AWS.Rekognition.DetectModerationLabels
  ( -- * Creating a request
    DetectModerationLabels (..),
    mkDetectModerationLabels,

    -- ** Request lenses
    dmlImage,
    dmlHumanLoopConfig,
    dmlMinConfidence,

    -- * Destructuring the response
    DetectModerationLabelsResponse (..),
    mkDetectModerationLabelsResponse,

    -- ** Response lenses
    dmlrsHumanLoopActivationOutput,
    dmlrsModerationModelVersion,
    dmlrsModerationLabels,
    dmlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectModerationLabels' smart constructor.
data DetectModerationLabels = DetectModerationLabels'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Image,
    -- | Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
    humanLoopConfig :: Lude.Maybe HumanLoopConfig,
    -- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value.
    --
    -- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
    minConfidence :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectModerationLabels' with the minimum fields required to make a request.
--
-- * 'image' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
-- * 'humanLoopConfig' - Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
-- * 'minConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
mkDetectModerationLabels ::
  -- | 'image'
  Image ->
  DetectModerationLabels
mkDetectModerationLabels pImage_ =
  DetectModerationLabels'
    { image = pImage_,
      humanLoopConfig = Lude.Nothing,
      minConfidence = Lude.Nothing
    }

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlImage :: Lens.Lens' DetectModerationLabels Image
dmlImage = Lens.lens (image :: DetectModerationLabels -> Image) (\s a -> s {image = a} :: DetectModerationLabels)
{-# DEPRECATED dmlImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlHumanLoopConfig :: Lens.Lens' DetectModerationLabels (Lude.Maybe HumanLoopConfig)
dmlHumanLoopConfig = Lens.lens (humanLoopConfig :: DetectModerationLabels -> Lude.Maybe HumanLoopConfig) (\s a -> s {humanLoopConfig = a} :: DetectModerationLabels)
{-# DEPRECATED dmlHumanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead." #-}

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlMinConfidence :: Lens.Lens' DetectModerationLabels (Lude.Maybe Lude.Double)
dmlMinConfidence = Lens.lens (minConfidence :: DetectModerationLabels -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: DetectModerationLabels)
{-# DEPRECATED dmlMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

instance Lude.AWSRequest DetectModerationLabels where
  type Rs DetectModerationLabels = DetectModerationLabelsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectModerationLabelsResponse'
            Lude.<$> (x Lude..?> "HumanLoopActivationOutput")
            Lude.<*> (x Lude..?> "ModerationModelVersion")
            Lude.<*> (x Lude..?> "ModerationLabels" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectModerationLabels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DetectModerationLabels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectModerationLabels where
  toJSON DetectModerationLabels' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Image" Lude..= image),
            ("HumanLoopConfig" Lude..=) Lude.<$> humanLoopConfig,
            ("MinConfidence" Lude..=) Lude.<$> minConfidence
          ]
      )

instance Lude.ToPath DetectModerationLabels where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectModerationLabels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectModerationLabelsResponse' smart constructor.
data DetectModerationLabelsResponse = DetectModerationLabelsResponse'
  { -- | Shows the results of the human in the loop evaluation.
    humanLoopActivationOutput :: Lude.Maybe HumanLoopActivationOutput,
    -- | Version number of the moderation detection model that was used to detect unsafe content.
    moderationModelVersion :: Lude.Maybe Lude.Text,
    -- | Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
    moderationLabels :: Lude.Maybe [ModerationLabel],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectModerationLabelsResponse' with the minimum fields required to make a request.
--
-- * 'humanLoopActivationOutput' - Shows the results of the human in the loop evaluation.
-- * 'moderationModelVersion' - Version number of the moderation detection model that was used to detect unsafe content.
-- * 'moderationLabels' - Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
-- * 'responseStatus' - The response status code.
mkDetectModerationLabelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectModerationLabelsResponse
mkDetectModerationLabelsResponse pResponseStatus_ =
  DetectModerationLabelsResponse'
    { humanLoopActivationOutput =
        Lude.Nothing,
      moderationModelVersion = Lude.Nothing,
      moderationLabels = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Shows the results of the human in the loop evaluation.
--
-- /Note:/ Consider using 'humanLoopActivationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrsHumanLoopActivationOutput :: Lens.Lens' DetectModerationLabelsResponse (Lude.Maybe HumanLoopActivationOutput)
dmlrsHumanLoopActivationOutput = Lens.lens (humanLoopActivationOutput :: DetectModerationLabelsResponse -> Lude.Maybe HumanLoopActivationOutput) (\s a -> s {humanLoopActivationOutput = a} :: DetectModerationLabelsResponse)
{-# DEPRECATED dmlrsHumanLoopActivationOutput "Use generic-lens or generic-optics with 'humanLoopActivationOutput' instead." #-}

-- | Version number of the moderation detection model that was used to detect unsafe content.
--
-- /Note:/ Consider using 'moderationModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrsModerationModelVersion :: Lens.Lens' DetectModerationLabelsResponse (Lude.Maybe Lude.Text)
dmlrsModerationModelVersion = Lens.lens (moderationModelVersion :: DetectModerationLabelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {moderationModelVersion = a} :: DetectModerationLabelsResponse)
{-# DEPRECATED dmlrsModerationModelVersion "Use generic-lens or generic-optics with 'moderationModelVersion' instead." #-}

-- | Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
--
-- /Note:/ Consider using 'moderationLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrsModerationLabels :: Lens.Lens' DetectModerationLabelsResponse (Lude.Maybe [ModerationLabel])
dmlrsModerationLabels = Lens.lens (moderationLabels :: DetectModerationLabelsResponse -> Lude.Maybe [ModerationLabel]) (\s a -> s {moderationLabels = a} :: DetectModerationLabelsResponse)
{-# DEPRECATED dmlrsModerationLabels "Use generic-lens or generic-optics with 'moderationLabels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrsResponseStatus :: Lens.Lens' DetectModerationLabelsResponse Lude.Int
dmlrsResponseStatus = Lens.lens (responseStatus :: DetectModerationLabelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectModerationLabelsResponse)
{-# DEPRECATED dmlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

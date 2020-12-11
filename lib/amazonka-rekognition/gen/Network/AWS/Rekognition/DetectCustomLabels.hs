{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectCustomLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects custom labels in a supplied image by using an Amazon Rekognition Custom Labels model.
--
-- You specify which version of a model version to use by using the @ProjectVersionArn@ input parameter.
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- For each object that the model version detects on an image, the API returns a (@CustomLabel@ ) object in an array (@CustomLabels@ ). Each @CustomLabel@ object provides the label name (@Name@ ), the level of confidence that the image contains the object (@Confidence@ ), and object location information, if it exists, for the label on the image (@Geometry@ ).
-- During training model calculates a threshold value that determines if a prediction for a label is true. By default, @DetectCustomLabels@ doesn't return labels whose confidence value is below the model's calculated threshold value. To filter labels that are returned, specify a value for @MinConfidence@ that is higher than the model's calculated threshold. You can get the model's calculated threshold from the model's training results shown in the Amazon Rekognition Custom Labels console. To get all labels, regardless of confidence, specify a @MinConfidence@ value of 0.
-- You can also add the @MaxResults@ parameter to limit the number of labels returned.
-- This is a stateless API operation. That is, the operation does not persist any data.
-- This operation requires permissions to perform the @rekognition:DetectCustomLabels@ action.
module Network.AWS.Rekognition.DetectCustomLabels
  ( -- * Creating a request
    DetectCustomLabels (..),
    mkDetectCustomLabels,

    -- ** Request lenses
    dclMinConfidence,
    dclMaxResults,
    dclProjectVersionARN,
    dclImage,

    -- * Destructuring the response
    DetectCustomLabelsResponse (..),
    mkDetectCustomLabelsResponse,

    -- ** Response lenses
    dclrsCustomLabels,
    dclrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectCustomLabels' smart constructor.
data DetectCustomLabels = DetectCustomLabels'
  { minConfidence ::
      Lude.Maybe Lude.Double,
    maxResults :: Lude.Maybe Lude.Natural,
    projectVersionARN :: Lude.Text,
    image :: Image
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectCustomLabels' with the minimum fields required to make a request.
--
-- * 'image' - Undocumented field.
-- * 'maxResults' - Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
-- * 'minConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
-- * 'projectVersionARN' - The ARN of the model version that you want to use.
mkDetectCustomLabels ::
  -- | 'projectVersionARN'
  Lude.Text ->
  -- | 'image'
  Image ->
  DetectCustomLabels
mkDetectCustomLabels pProjectVersionARN_ pImage_ =
  DetectCustomLabels'
    { minConfidence = Lude.Nothing,
      maxResults = Lude.Nothing,
      projectVersionARN = pProjectVersionARN_,
      image = pImage_
    }

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclMinConfidence :: Lens.Lens' DetectCustomLabels (Lude.Maybe Lude.Double)
dclMinConfidence = Lens.lens (minConfidence :: DetectCustomLabels -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: DetectCustomLabels)
{-# DEPRECATED dclMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

-- | Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclMaxResults :: Lens.Lens' DetectCustomLabels (Lude.Maybe Lude.Natural)
dclMaxResults = Lens.lens (maxResults :: DetectCustomLabels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DetectCustomLabels)
{-# DEPRECATED dclMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the model version that you want to use.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclProjectVersionARN :: Lens.Lens' DetectCustomLabels Lude.Text
dclProjectVersionARN = Lens.lens (projectVersionARN :: DetectCustomLabels -> Lude.Text) (\s a -> s {projectVersionARN = a} :: DetectCustomLabels)
{-# DEPRECATED dclProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclImage :: Lens.Lens' DetectCustomLabels Image
dclImage = Lens.lens (image :: DetectCustomLabels -> Image) (\s a -> s {image = a} :: DetectCustomLabels)
{-# DEPRECATED dclImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.AWSRequest DetectCustomLabels where
  type Rs DetectCustomLabels = DetectCustomLabelsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectCustomLabelsResponse'
            Lude.<$> (x Lude..?> "CustomLabels" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectCustomLabels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DetectCustomLabels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectCustomLabels where
  toJSON DetectCustomLabels' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MinConfidence" Lude..=) Lude.<$> minConfidence,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ProjectVersionArn" Lude..= projectVersionARN),
            Lude.Just ("Image" Lude..= image)
          ]
      )

instance Lude.ToPath DetectCustomLabels where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectCustomLabels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectCustomLabelsResponse' smart constructor.
data DetectCustomLabelsResponse = DetectCustomLabelsResponse'
  { customLabels ::
      Lude.Maybe [CustomLabel],
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

-- | Creates a value of 'DetectCustomLabelsResponse' with the minimum fields required to make a request.
--
-- * 'customLabels' - An array of custom labels detected in the input image.
-- * 'responseStatus' - The response status code.
mkDetectCustomLabelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectCustomLabelsResponse
mkDetectCustomLabelsResponse pResponseStatus_ =
  DetectCustomLabelsResponse'
    { customLabels = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of custom labels detected in the input image.
--
-- /Note:/ Consider using 'customLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclrsCustomLabels :: Lens.Lens' DetectCustomLabelsResponse (Lude.Maybe [CustomLabel])
dclrsCustomLabels = Lens.lens (customLabels :: DetectCustomLabelsResponse -> Lude.Maybe [CustomLabel]) (\s a -> s {customLabels = a} :: DetectCustomLabelsResponse)
{-# DEPRECATED dclrsCustomLabels "Use generic-lens or generic-optics with 'customLabels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclrsResponseStatus :: Lens.Lens' DetectCustomLabelsResponse Lude.Int
dclrsResponseStatus = Lens.lens (responseStatus :: DetectCustomLabelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectCustomLabelsResponse)
{-# DEPRECATED dclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

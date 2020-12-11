{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature.
--
-- For an example, see Analyzing Images Stored in an Amazon S3 Bucket in the Amazon Rekognition Developer Guide.
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response includes all three labels, one for each object.
-- @{Name: lighthouse, Confidence: 98.4629}@
-- @{Name: rock,Confidence: 79.2097}@
-- @{Name: sea,Confidence: 75.061}@
-- In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels.
-- @{Name: flower,Confidence: 99.0562}@
-- @{Name: plant,Confidence: 99.0562}@
-- @{Name: tulip,Confidence: 99.0562}@
-- In this example, the detection algorithm more precisely identifies the flower as a tulip.
-- In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify @MinConfidence@ to control the confidence threshold for the labels returned. The default is 55%. You can also add the @MaxLabels@ parameter to limit the number of labels returned.
-- @DetectLabels@ returns bounding boxes for instances of common object labels in an array of 'Instance' objects. An @Instance@ object contains a 'BoundingBox' object, for the location of the label on the image. It also includes the confidence by which the bounding box was detected.
-- @DetectLabels@ also returns a hierarchical taxonomy of detected labels. For example, a detected car might be assigned the label /car/ . The label /car/ has two parent labels: /Vehicle/ (its parent) and /Transportation/ (its grandparent). The response returns the entire list of ancestors for a label. Each ancestor is a unique label in the response. In the previous example, /Car/ , /Vehicle/ , and /Transportation/ are returned as unique labels in the response.
-- This is a stateless API operation. That is, the operation does not persist any data.
-- This operation requires permissions to perform the @rekognition:DetectLabels@ action.
module Network.AWS.Rekognition.DetectLabels
  ( -- * Creating a request
    DetectLabels (..),
    mkDetectLabels,

    -- ** Request lenses
    dlMinConfidence,
    dlMaxLabels,
    dlImage,

    -- * Destructuring the response
    DetectLabelsResponse (..),
    mkDetectLabelsResponse,

    -- ** Response lenses
    dlrsLabels,
    dlrsOrientationCorrection,
    dlrsLabelModelVersion,
    dlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { minConfidence ::
      Lude.Maybe Lude.Double,
    maxLabels :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'DetectLabels' with the minimum fields required to make a request.
--
-- * 'image' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
-- * 'maxLabels' - Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels.
-- * 'minConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
mkDetectLabels ::
  -- | 'image'
  Image ->
  DetectLabels
mkDetectLabels pImage_ =
  DetectLabels'
    { minConfidence = Lude.Nothing,
      maxLabels = Lude.Nothing,
      image = pImage_
    }

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMinConfidence :: Lens.Lens' DetectLabels (Lude.Maybe Lude.Double)
dlMinConfidence = Lens.lens (minConfidence :: DetectLabels -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: DetectLabels)
{-# DEPRECATED dlMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

-- | Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels.
--
-- /Note:/ Consider using 'maxLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMaxLabels :: Lens.Lens' DetectLabels (Lude.Maybe Lude.Natural)
dlMaxLabels = Lens.lens (maxLabels :: DetectLabels -> Lude.Maybe Lude.Natural) (\s a -> s {maxLabels = a} :: DetectLabels)
{-# DEPRECATED dlMaxLabels "Use generic-lens or generic-optics with 'maxLabels' instead." #-}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlImage :: Lens.Lens' DetectLabels Image
dlImage = Lens.lens (image :: DetectLabels -> Image) (\s a -> s {image = a} :: DetectLabels)
{-# DEPRECATED dlImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.AWSRequest DetectLabels where
  type Rs DetectLabels = DetectLabelsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectLabelsResponse'
            Lude.<$> (x Lude..?> "Labels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "OrientationCorrection")
            Lude.<*> (x Lude..?> "LabelModelVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectLabels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DetectLabels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectLabels where
  toJSON DetectLabels' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MinConfidence" Lude..=) Lude.<$> minConfidence,
            ("MaxLabels" Lude..=) Lude.<$> maxLabels,
            Lude.Just ("Image" Lude..= image)
          ]
      )

instance Lude.ToPath DetectLabels where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectLabels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { labels ::
      Lude.Maybe [Label],
    orientationCorrection ::
      Lude.Maybe OrientationCorrection,
    labelModelVersion :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DetectLabelsResponse' with the minimum fields required to make a request.
--
-- * 'labelModelVersion' - Version number of the label detection model that was used to detect labels.
-- * 'labels' - An array of labels for the real-world objects detected.
-- * 'orientationCorrection' - The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
-- * 'responseStatus' - The response status code.
mkDetectLabelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectLabelsResponse
mkDetectLabelsResponse pResponseStatus_ =
  DetectLabelsResponse'
    { labels = Lude.Nothing,
      orientationCorrection = Lude.Nothing,
      labelModelVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of labels for the real-world objects detected.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsLabels :: Lens.Lens' DetectLabelsResponse (Lude.Maybe [Label])
dlrsLabels = Lens.lens (labels :: DetectLabelsResponse -> Lude.Maybe [Label]) (\s a -> s {labels = a} :: DetectLabelsResponse)
{-# DEPRECATED dlrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsOrientationCorrection :: Lens.Lens' DetectLabelsResponse (Lude.Maybe OrientationCorrection)
dlrsOrientationCorrection = Lens.lens (orientationCorrection :: DetectLabelsResponse -> Lude.Maybe OrientationCorrection) (\s a -> s {orientationCorrection = a} :: DetectLabelsResponse)
{-# DEPRECATED dlrsOrientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead." #-}

-- | Version number of the label detection model that was used to detect labels.
--
-- /Note:/ Consider using 'labelModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsLabelModelVersion :: Lens.Lens' DetectLabelsResponse (Lude.Maybe Lude.Text)
dlrsLabelModelVersion = Lens.lens (labelModelVersion :: DetectLabelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {labelModelVersion = a} :: DetectLabelsResponse)
{-# DEPRECATED dlrsLabelModelVersion "Use generic-lens or generic-optics with 'labelModelVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DetectLabelsResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DetectLabelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectLabelsResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

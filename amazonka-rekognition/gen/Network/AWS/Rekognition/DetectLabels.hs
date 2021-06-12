{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectLabels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects instances of real-world entities within an image (JPEG or PNG)
-- provided as input. This includes objects like flower, tree, and table;
-- events like wedding, graduation, and birthday party; and concepts like
-- landscape, evening, and nature.
--
-- For an example, see Analyzing Images Stored in an Amazon S3 Bucket in
-- the Amazon Rekognition Developer Guide.
--
-- @DetectLabels@ does not support the detection of activities. However,
-- activity detection is supported for label detection in videos. For more
-- information, see StartLabelDetection in the Amazon Rekognition Developer
-- Guide.
--
-- You pass the input image as base64-encoded image bytes or as a reference
-- to an image in an Amazon S3 bucket. If you use the AWS CLI to call
-- Amazon Rekognition operations, passing image bytes is not supported. The
-- image must be either a PNG or JPEG formatted file.
--
-- For each object, scene, and concept the API returns one or more labels.
-- Each label provides the object name, and the level of confidence that
-- the image contains the object. For example, suppose the input image has
-- a lighthouse, the sea, and a rock. The response includes all three
-- labels, one for each object.
--
-- @{Name: lighthouse, Confidence: 98.4629}@
--
-- @{Name: rock,Confidence: 79.2097}@
--
-- @ {Name: sea,Confidence: 75.061}@
--
-- In the preceding example, the operation returns one label for each of
-- the three objects. The operation can also return multiple labels for the
-- same object in the image. For example, if the input image shows a flower
-- (for example, a tulip), the operation might return the following three
-- labels.
--
-- @{Name: flower,Confidence: 99.0562}@
--
-- @{Name: plant,Confidence: 99.0562}@
--
-- @{Name: tulip,Confidence: 99.0562}@
--
-- In this example, the detection algorithm more precisely identifies the
-- flower as a tulip.
--
-- In response, the API returns an array of labels. In addition, the
-- response also includes the orientation correction. Optionally, you can
-- specify @MinConfidence@ to control the confidence threshold for the
-- labels returned. The default is 55%. You can also add the @MaxLabels@
-- parameter to limit the number of labels returned.
--
-- If the object detected is a person, the operation doesn\'t provide the
-- same facial details that the DetectFaces operation provides.
--
-- @DetectLabels@ returns bounding boxes for instances of common object
-- labels in an array of Instance objects. An @Instance@ object contains a
-- BoundingBox object, for the location of the label on the image. It also
-- includes the confidence by which the bounding box was detected.
--
-- @DetectLabels@ also returns a hierarchical taxonomy of detected labels.
-- For example, a detected car might be assigned the label /car/. The label
-- /car/ has two parent labels: /Vehicle/ (its parent) and /Transportation/
-- (its grandparent). The response returns the entire list of ancestors for
-- a label. Each ancestor is a unique label in the response. In the
-- previous example, /Car/, /Vehicle/, and /Transportation/ are returned as
-- unique labels in the response.
--
-- This is a stateless API operation. That is, the operation does not
-- persist any data.
--
-- This operation requires permissions to perform the
-- @rekognition:DetectLabels@ action.
module Network.AWS.Rekognition.DetectLabels
  ( -- * Creating a Request
    DetectLabels (..),
    newDetectLabels,

    -- * Request Lenses
    detectLabels_maxLabels,
    detectLabels_minConfidence,
    detectLabels_image,

    -- * Destructuring the Response
    DetectLabelsResponse (..),
    newDetectLabelsResponse,

    -- * Response Lenses
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_labels,
    detectLabelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { -- | Maximum number of labels you want the service to return in the response.
    -- The service returns the specified number of highest confidence labels.
    maxLabels :: Core.Maybe Core.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon
    -- Rekognition doesn\'t return any labels with confidence lower than this
    -- specified value.
    --
    -- If @MinConfidence@ is not specified, the operation returns labels with a
    -- confidence values greater than or equal to 55 percent.
    minConfidence :: Core.Maybe Core.Double,
    -- | The input image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing image bytes is
    -- not supported. Images stored in an S3 Bucket do not need to be
    -- base64-encoded.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    image :: Image
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLabels', 'detectLabels_maxLabels' - Maximum number of labels you want the service to return in the response.
-- The service returns the specified number of highest confidence labels.
--
-- 'minConfidence', 'detectLabels_minConfidence' - Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with confidence lower than this
-- specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a
-- confidence values greater than or equal to 55 percent.
--
-- 'image', 'detectLabels_image' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing image bytes is
-- not supported. Images stored in an S3 Bucket do not need to be
-- base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newDetectLabels ::
  -- | 'image'
  Image ->
  DetectLabels
newDetectLabels pImage_ =
  DetectLabels'
    { maxLabels = Core.Nothing,
      minConfidence = Core.Nothing,
      image = pImage_
    }

-- | Maximum number of labels you want the service to return in the response.
-- The service returns the specified number of highest confidence labels.
detectLabels_maxLabels :: Lens.Lens' DetectLabels (Core.Maybe Core.Natural)
detectLabels_maxLabels = Lens.lens (\DetectLabels' {maxLabels} -> maxLabels) (\s@DetectLabels' {} a -> s {maxLabels = a} :: DetectLabels)

-- | Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with confidence lower than this
-- specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a
-- confidence values greater than or equal to 55 percent.
detectLabels_minConfidence :: Lens.Lens' DetectLabels (Core.Maybe Core.Double)
detectLabels_minConfidence = Lens.lens (\DetectLabels' {minConfidence} -> minConfidence) (\s@DetectLabels' {} a -> s {minConfidence = a} :: DetectLabels)

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing image bytes is
-- not supported. Images stored in an S3 Bucket do not need to be
-- base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
detectLabels_image :: Lens.Lens' DetectLabels Image
detectLabels_image = Lens.lens (\DetectLabels' {image} -> image) (\s@DetectLabels' {} a -> s {image = a} :: DetectLabels)

instance Core.AWSRequest DetectLabels where
  type AWSResponse DetectLabels = DetectLabelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectLabelsResponse'
            Core.<$> (x Core..?> "LabelModelVersion")
            Core.<*> (x Core..?> "OrientationCorrection")
            Core.<*> (x Core..?> "Labels" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetectLabels

instance Core.NFData DetectLabels

instance Core.ToHeaders DetectLabels where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DetectLabels" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetectLabels where
  toJSON DetectLabels' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxLabels" Core..=) Core.<$> maxLabels,
            ("MinConfidence" Core..=) Core.<$> minConfidence,
            Core.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath DetectLabels where
  toPath = Core.const "/"

instance Core.ToQuery DetectLabels where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { -- | Version number of the label detection model that was used to detect
    -- labels.
    labelModelVersion :: Core.Maybe Core.Text,
    -- | The value of @OrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable
    -- image file format (Exif) metadata that includes the image\'s
    -- orientation. Amazon Rekognition uses this orientation information to
    -- perform image correction. The bounding box coordinates are translated to
    -- represent object locations after the orientation information in the Exif
    -- metadata is used to correct the image orientation. Images in .png format
    -- don\'t contain Exif metadata.
    --
    -- Amazon Rekognition doesn’t perform image correction for images in .png
    -- format and .jpeg images without orientation information in the image
    -- Exif metadata. The bounding box coordinates aren\'t translated and
    -- represent the object locations before the image is rotated.
    orientationCorrection :: Core.Maybe OrientationCorrection,
    -- | An array of labels for the real-world objects detected.
    labels :: Core.Maybe [Label],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelModelVersion', 'detectLabelsResponse_labelModelVersion' - Version number of the label detection model that was used to detect
-- labels.
--
-- 'orientationCorrection', 'detectLabelsResponse_orientationCorrection' - The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
--
-- 'labels', 'detectLabelsResponse_labels' - An array of labels for the real-world objects detected.
--
-- 'httpStatus', 'detectLabelsResponse_httpStatus' - The response's http status code.
newDetectLabelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetectLabelsResponse
newDetectLabelsResponse pHttpStatus_ =
  DetectLabelsResponse'
    { labelModelVersion =
        Core.Nothing,
      orientationCorrection = Core.Nothing,
      labels = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the label detection model that was used to detect
-- labels.
detectLabelsResponse_labelModelVersion :: Lens.Lens' DetectLabelsResponse (Core.Maybe Core.Text)
detectLabelsResponse_labelModelVersion = Lens.lens (\DetectLabelsResponse' {labelModelVersion} -> labelModelVersion) (\s@DetectLabelsResponse' {} a -> s {labelModelVersion = a} :: DetectLabelsResponse)

-- | The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
detectLabelsResponse_orientationCorrection :: Lens.Lens' DetectLabelsResponse (Core.Maybe OrientationCorrection)
detectLabelsResponse_orientationCorrection = Lens.lens (\DetectLabelsResponse' {orientationCorrection} -> orientationCorrection) (\s@DetectLabelsResponse' {} a -> s {orientationCorrection = a} :: DetectLabelsResponse)

-- | An array of labels for the real-world objects detected.
detectLabelsResponse_labels :: Lens.Lens' DetectLabelsResponse (Core.Maybe [Label])
detectLabelsResponse_labels = Lens.lens (\DetectLabelsResponse' {labels} -> labels) (\s@DetectLabelsResponse' {} a -> s {labels = a} :: DetectLabelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectLabelsResponse_httpStatus :: Lens.Lens' DetectLabelsResponse Core.Int
detectLabelsResponse_httpStatus = Lens.lens (\DetectLabelsResponse' {httpStatus} -> httpStatus) (\s@DetectLabelsResponse' {} a -> s {httpStatus = a} :: DetectLabelsResponse)

instance Core.NFData DetectLabelsResponse

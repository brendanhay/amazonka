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
-- Module      : Amazonka.Rekognition.DetectLabels
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- For an example, see Analyzing images stored in an Amazon S3 bucket in
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
module Amazonka.Rekognition.DetectLabels
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { -- | Maximum number of labels you want the service to return in the response.
    -- The service returns the specified number of highest confidence labels.
    maxLabels :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon
    -- Rekognition doesn\'t return any labels with confidence lower than this
    -- specified value.
    --
    -- If @MinConfidence@ is not specified, the operation returns labels with a
    -- confidence values greater than or equal to 55 percent.
    minConfidence :: Prelude.Maybe Prelude.Double,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { maxLabels = Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      image = pImage_
    }

-- | Maximum number of labels you want the service to return in the response.
-- The service returns the specified number of highest confidence labels.
detectLabels_maxLabels :: Lens.Lens' DetectLabels (Prelude.Maybe Prelude.Natural)
detectLabels_maxLabels = Lens.lens (\DetectLabels' {maxLabels} -> maxLabels) (\s@DetectLabels' {} a -> s {maxLabels = a} :: DetectLabels)

-- | Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with confidence lower than this
-- specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a
-- confidence values greater than or equal to 55 percent.
detectLabels_minConfidence :: Lens.Lens' DetectLabels (Prelude.Maybe Prelude.Double)
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
  service _ = defaultService
  request srv = Request.postJSON srv
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectLabelsResponse'
            Prelude.<$> (x Core..?> "LabelModelVersion")
            Prelude.<*> (x Core..?> "OrientationCorrection")
            Prelude.<*> (x Core..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectLabels where
  hashWithSalt _salt DetectLabels' {..} =
    _salt `Prelude.hashWithSalt` maxLabels
      `Prelude.hashWithSalt` minConfidence
      `Prelude.hashWithSalt` image

instance Prelude.NFData DetectLabels where
  rnf DetectLabels' {..} =
    Prelude.rnf maxLabels
      `Prelude.seq` Prelude.rnf minConfidence
      `Prelude.seq` Prelude.rnf image

instance Core.ToHeaders DetectLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DetectLabels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectLabels where
  toJSON DetectLabels' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxLabels" Core..=) Prelude.<$> maxLabels,
            ("MinConfidence" Core..=) Prelude.<$> minConfidence,
            Prelude.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath DetectLabels where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { -- | Version number of the label detection model that was used to detect
    -- labels.
    labelModelVersion :: Prelude.Maybe Prelude.Text,
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
    orientationCorrection :: Prelude.Maybe OrientationCorrection,
    -- | An array of labels for the real-world objects detected.
    labels :: Prelude.Maybe [Label],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DetectLabelsResponse
newDetectLabelsResponse pHttpStatus_ =
  DetectLabelsResponse'
    { labelModelVersion =
        Prelude.Nothing,
      orientationCorrection = Prelude.Nothing,
      labels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the label detection model that was used to detect
-- labels.
detectLabelsResponse_labelModelVersion :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe Prelude.Text)
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
detectLabelsResponse_orientationCorrection :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe OrientationCorrection)
detectLabelsResponse_orientationCorrection = Lens.lens (\DetectLabelsResponse' {orientationCorrection} -> orientationCorrection) (\s@DetectLabelsResponse' {} a -> s {orientationCorrection = a} :: DetectLabelsResponse)

-- | An array of labels for the real-world objects detected.
detectLabelsResponse_labels :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe [Label])
detectLabelsResponse_labels = Lens.lens (\DetectLabelsResponse' {labels} -> labels) (\s@DetectLabelsResponse' {} a -> s {labels = a} :: DetectLabelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectLabelsResponse_httpStatus :: Lens.Lens' DetectLabelsResponse Prelude.Int
detectLabelsResponse_httpStatus = Lens.lens (\DetectLabelsResponse' {httpStatus} -> httpStatus) (\s@DetectLabelsResponse' {} a -> s {httpStatus = a} :: DetectLabelsResponse)

instance Prelude.NFData DetectLabelsResponse where
  rnf DetectLabelsResponse' {..} =
    Prelude.rnf labelModelVersion
      `Prelude.seq` Prelude.rnf orientationCorrection
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- You pass the input image as base64-encoded image bytes or as a reference
-- to an image in an Amazon S3 bucket. If you use the AWS CLI to call
-- Amazon Rekognition operations, passing image bytes is not supported. The
-- image must be either a PNG or JPEG formatted file.
--
-- __Optional Parameters__
--
-- You can specify one or both of the @GENERAL_LABELS@ and
-- @IMAGE_PROPERTIES@ feature types when calling the DetectLabels API.
-- Including @GENERAL_LABELS@ will ensure the response includes the labels
-- detected in the input image, while including @IMAGE_PROPERTIES @will
-- ensure the response includes information about the image quality and
-- color.
--
-- When using @GENERAL_LABELS@ and\/or @IMAGE_PROPERTIES@ you can provide
-- filtering criteria to the Settings parameter. You can filter with sets
-- of individual labels or with label categories. You can specify inclusive
-- filters, exclusive filters, or a combination of inclusive and exclusive
-- filters. For more information on filtering see
-- <https://docs.aws.amazon.com/rekognition/latest/dg/labels-detect-labels-image.html Detecting Labels in an Image>.
--
-- You can specify @MinConfidence@ to control the confidence threshold for
-- the labels returned. The default is 55%. You can also add the
-- @MaxLabels@ parameter to limit the number of labels returned. The
-- default and upper limit is 1000 labels.
--
-- __Response Elements__
--
-- For each object, scene, and concept the API returns one or more labels.
-- The API returns the following types of information regarding labels:
--
-- -   Name - The name of the detected label.
--
-- -   Confidence - The level of confidence in the label assigned to a
--     detected object.
--
-- -   Parents - The ancestor labels for a detected label. DetectLabels
--     returns a hierarchical taxonomy of detected labels. For example, a
--     detected car might be assigned the label car. The label car has two
--     parent labels: Vehicle (its parent) and Transportation (its
--     grandparent). The response includes the all ancestors for a label,
--     where every ancestor is a unique label. In the previous example,
--     Car, Vehicle, and Transportation are returned as unique labels in
--     the response.
--
-- -   Aliases - Possible Aliases for the label.
--
-- -   Categories - The label categories that the detected label belongs
--     to.
--
-- -   BoundingBox — Bounding boxes are described for all instances of
--     detected common object labels, returned in an array of Instance
--     objects. An Instance object contains a BoundingBox object,
--     describing the location of the label on the input image. It also
--     includes the confidence for the accuracy of the detected bounding
--     box.
--
-- The API returns the following information regarding the image, as part
-- of the ImageProperties structure:
--
-- -   Quality - Information about the Sharpness, Brightness, and Contrast
--     of the input image, scored between 0 to 100. Image quality is
--     returned for the entire image, as well as the background and the
--     foreground.
--
-- -   Dominant Color - An array of the dominant colors in the image.
--
-- -   Foreground - Information about the sharpness, brightness, and
--     dominant colors of the input image’s foreground.
--
-- -   Background - Information about the sharpness, brightness, and
--     dominant colors of the input image’s background.
--
-- The list of returned labels will include at least one label for every
-- detected object, along with information about that label. In the
-- following example, suppose the input image has a lighthouse, the sea,
-- and a rock. The response includes all three labels, one for each object,
-- as well as the confidence in the label:
--
-- @{Name: lighthouse, Confidence: 98.4629}@
--
-- @{Name: rock,Confidence: 79.2097}@
--
-- @ {Name: sea,Confidence: 75.061}@
--
-- The list of labels can include multiple labels for the same object. For
-- example, if the input image shows a flower (for example, a tulip), the
-- operation might return the following three labels.
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
-- If the object detected is a person, the operation doesn\'t provide the
-- same facial details that the DetectFaces operation provides.
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
    detectLabels_features,
    detectLabels_maxLabels,
    detectLabels_minConfidence,
    detectLabels_settings,
    detectLabels_image,

    -- * Destructuring the Response
    DetectLabelsResponse (..),
    newDetectLabelsResponse,

    -- * Response Lenses
    detectLabelsResponse_imageProperties,
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_labels,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { -- | A list of the types of analysis to perform. Specifying GENERAL_LABELS
    -- uses the label detection feature, while specifying IMAGE_PROPERTIES
    -- returns information regarding image color and quality. If no option is
    -- specified GENERAL_LABELS is used by default.
    features :: Prelude.Maybe [DetectLabelsFeatureName],
    -- | Maximum number of labels you want the service to return in the response.
    -- The service returns the specified number of highest confidence labels.
    maxLabels :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon
    -- Rekognition doesn\'t return any labels with confidence lower than this
    -- specified value.
    --
    -- If @MinConfidence@ is not specified, the operation returns labels with a
    -- confidence values greater than or equal to 55 percent.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | A list of the filters to be applied to returned detected labels and
    -- image properties. Specified filters can be inclusive, exclusive, or a
    -- combination of both. Filters can be used for individual labels or label
    -- categories. The exact label names or label categories must be supplied.
    -- For a full list of labels and label categories, see LINK HERE.
    settings :: Prelude.Maybe DetectLabelsSettings,
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
-- 'features', 'detectLabels_features' - A list of the types of analysis to perform. Specifying GENERAL_LABELS
-- uses the label detection feature, while specifying IMAGE_PROPERTIES
-- returns information regarding image color and quality. If no option is
-- specified GENERAL_LABELS is used by default.
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
-- 'settings', 'detectLabels_settings' - A list of the filters to be applied to returned detected labels and
-- image properties. Specified filters can be inclusive, exclusive, or a
-- combination of both. Filters can be used for individual labels or label
-- categories. The exact label names or label categories must be supplied.
-- For a full list of labels and label categories, see LINK HERE.
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
    { features = Prelude.Nothing,
      maxLabels = Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      settings = Prelude.Nothing,
      image = pImage_
    }

-- | A list of the types of analysis to perform. Specifying GENERAL_LABELS
-- uses the label detection feature, while specifying IMAGE_PROPERTIES
-- returns information regarding image color and quality. If no option is
-- specified GENERAL_LABELS is used by default.
detectLabels_features :: Lens.Lens' DetectLabels (Prelude.Maybe [DetectLabelsFeatureName])
detectLabels_features = Lens.lens (\DetectLabels' {features} -> features) (\s@DetectLabels' {} a -> s {features = a} :: DetectLabels) Prelude.. Lens.mapping Lens.coerced

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

-- | A list of the filters to be applied to returned detected labels and
-- image properties. Specified filters can be inclusive, exclusive, or a
-- combination of both. Filters can be used for individual labels or label
-- categories. The exact label names or label categories must be supplied.
-- For a full list of labels and label categories, see LINK HERE.
detectLabels_settings :: Lens.Lens' DetectLabels (Prelude.Maybe DetectLabelsSettings)
detectLabels_settings = Lens.lens (\DetectLabels' {settings} -> settings) (\s@DetectLabels' {} a -> s {settings = a} :: DetectLabels)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectLabelsResponse'
            Prelude.<$> (x Data..?> "ImageProperties")
            Prelude.<*> (x Data..?> "LabelModelVersion")
            Prelude.<*> (x Data..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "OrientationCorrection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectLabels where
  hashWithSalt _salt DetectLabels' {..} =
    _salt `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` maxLabels
      `Prelude.hashWithSalt` minConfidence
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` image

instance Prelude.NFData DetectLabels where
  rnf DetectLabels' {..} =
    Prelude.rnf features
      `Prelude.seq` Prelude.rnf maxLabels
      `Prelude.seq` Prelude.rnf minConfidence
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf image

instance Data.ToHeaders DetectLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DetectLabels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectLabels where
  toJSON DetectLabels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Features" Data..=) Prelude.<$> features,
            ("MaxLabels" Data..=) Prelude.<$> maxLabels,
            ("MinConfidence" Data..=) Prelude.<$> minConfidence,
            ("Settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("Image" Data..= image)
          ]
      )

instance Data.ToPath DetectLabels where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { -- | Information about the properties of the input image, such as brightness,
    -- sharpness, contrast, and dominant colors.
    imageProperties :: Prelude.Maybe DetectLabelsImageProperties,
    -- | Version number of the label detection model that was used to detect
    -- labels.
    labelModelVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of labels for the real-world objects detected.
    labels :: Prelude.Maybe [Label],
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
-- 'imageProperties', 'detectLabelsResponse_imageProperties' - Information about the properties of the input image, such as brightness,
-- sharpness, contrast, and dominant colors.
--
-- 'labelModelVersion', 'detectLabelsResponse_labelModelVersion' - Version number of the label detection model that was used to detect
-- labels.
--
-- 'labels', 'detectLabelsResponse_labels' - An array of labels for the real-world objects detected.
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
-- 'httpStatus', 'detectLabelsResponse_httpStatus' - The response's http status code.
newDetectLabelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectLabelsResponse
newDetectLabelsResponse pHttpStatus_ =
  DetectLabelsResponse'
    { imageProperties =
        Prelude.Nothing,
      labelModelVersion = Prelude.Nothing,
      labels = Prelude.Nothing,
      orientationCorrection = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the properties of the input image, such as brightness,
-- sharpness, contrast, and dominant colors.
detectLabelsResponse_imageProperties :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe DetectLabelsImageProperties)
detectLabelsResponse_imageProperties = Lens.lens (\DetectLabelsResponse' {imageProperties} -> imageProperties) (\s@DetectLabelsResponse' {} a -> s {imageProperties = a} :: DetectLabelsResponse)

-- | Version number of the label detection model that was used to detect
-- labels.
detectLabelsResponse_labelModelVersion :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe Prelude.Text)
detectLabelsResponse_labelModelVersion = Lens.lens (\DetectLabelsResponse' {labelModelVersion} -> labelModelVersion) (\s@DetectLabelsResponse' {} a -> s {labelModelVersion = a} :: DetectLabelsResponse)

-- | An array of labels for the real-world objects detected.
detectLabelsResponse_labels :: Lens.Lens' DetectLabelsResponse (Prelude.Maybe [Label])
detectLabelsResponse_labels = Lens.lens (\DetectLabelsResponse' {labels} -> labels) (\s@DetectLabelsResponse' {} a -> s {labels = a} :: DetectLabelsResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The response's http status code.
detectLabelsResponse_httpStatus :: Lens.Lens' DetectLabelsResponse Prelude.Int
detectLabelsResponse_httpStatus = Lens.lens (\DetectLabelsResponse' {httpStatus} -> httpStatus) (\s@DetectLabelsResponse' {} a -> s {httpStatus = a} :: DetectLabelsResponse)

instance Prelude.NFData DetectLabelsResponse where
  rnf DetectLabelsResponse' {..} =
    Prelude.rnf imageProperties
      `Prelude.seq` Prelude.rnf labelModelVersion
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf orientationCorrection
      `Prelude.seq` Prelude.rnf httpStatus

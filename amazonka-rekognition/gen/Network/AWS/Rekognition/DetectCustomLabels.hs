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
-- Module      : Network.AWS.Rekognition.DetectCustomLabels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects custom labels in a supplied image by using an Amazon Rekognition
-- Custom Labels model.
--
-- You specify which version of a model version to use by using the
-- @ProjectVersionArn@ input parameter.
--
-- You pass the input image as base64-encoded image bytes or as a reference
-- to an image in an Amazon S3 bucket. If you use the AWS CLI to call
-- Amazon Rekognition operations, passing image bytes is not supported. The
-- image must be either a PNG or JPEG formatted file.
--
-- For each object that the model version detects on an image, the API
-- returns a (@CustomLabel@) object in an array (@CustomLabels@). Each
-- @CustomLabel@ object provides the label name (@Name@), the level of
-- confidence that the image contains the object (@Confidence@), and object
-- location information, if it exists, for the label on the image
-- (@Geometry@).
--
-- During training model calculates a threshold value that determines if a
-- prediction for a label is true. By default, @DetectCustomLabels@
-- doesn\'t return labels whose confidence value is below the model\'s
-- calculated threshold value. To filter labels that are returned, specify
-- a value for @MinConfidence@ that is higher than the model\'s calculated
-- threshold. You can get the model\'s calculated threshold from the
-- model\'s training results shown in the Amazon Rekognition Custom Labels
-- console. To get all labels, regardless of confidence, specify a
-- @MinConfidence@ value of 0.
--
-- You can also add the @MaxResults@ parameter to limit the number of
-- labels returned.
--
-- This is a stateless API operation. That is, the operation does not
-- persist any data.
--
-- This operation requires permissions to perform the
-- @rekognition:DetectCustomLabels@ action.
module Network.AWS.Rekognition.DetectCustomLabels
  ( -- * Creating a Request
    DetectCustomLabels (..),
    newDetectCustomLabels,

    -- * Request Lenses
    detectCustomLabels_maxResults,
    detectCustomLabels_minConfidence,
    detectCustomLabels_projectVersionArn,
    detectCustomLabels_image,

    -- * Destructuring the Response
    DetectCustomLabelsResponse (..),
    newDetectCustomLabelsResponse,

    -- * Response Lenses
    detectCustomLabelsResponse_customLabels,
    detectCustomLabelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectCustomLabels' smart constructor.
data DetectCustomLabels = DetectCustomLabels'
  { -- | Maximum number of results you want the service to return in the
    -- response. The service returns the specified number of highest confidence
    -- labels ranked from highest confidence to lowest.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon
    -- Rekognition doesn\'t return any labels with a confidence lower than this
    -- specified value. If you specify a value of 0, all labels are return,
    -- regardless of the default thresholds that the model version applies.
    minConfidence :: Core.Maybe Core.Double,
    -- | The ARN of the model version that you want to use.
    projectVersionArn :: Core.Text,
    image :: Image
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectCustomLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'detectCustomLabels_maxResults' - Maximum number of results you want the service to return in the
-- response. The service returns the specified number of highest confidence
-- labels ranked from highest confidence to lowest.
--
-- 'minConfidence', 'detectCustomLabels_minConfidence' - Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with a confidence lower than this
-- specified value. If you specify a value of 0, all labels are return,
-- regardless of the default thresholds that the model version applies.
--
-- 'projectVersionArn', 'detectCustomLabels_projectVersionArn' - The ARN of the model version that you want to use.
--
-- 'image', 'detectCustomLabels_image' - Undocumented member.
newDetectCustomLabels ::
  -- | 'projectVersionArn'
  Core.Text ->
  -- | 'image'
  Image ->
  DetectCustomLabels
newDetectCustomLabels pProjectVersionArn_ pImage_ =
  DetectCustomLabels'
    { maxResults = Core.Nothing,
      minConfidence = Core.Nothing,
      projectVersionArn = pProjectVersionArn_,
      image = pImage_
    }

-- | Maximum number of results you want the service to return in the
-- response. The service returns the specified number of highest confidence
-- labels ranked from highest confidence to lowest.
detectCustomLabels_maxResults :: Lens.Lens' DetectCustomLabels (Core.Maybe Core.Natural)
detectCustomLabels_maxResults = Lens.lens (\DetectCustomLabels' {maxResults} -> maxResults) (\s@DetectCustomLabels' {} a -> s {maxResults = a} :: DetectCustomLabels)

-- | Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with a confidence lower than this
-- specified value. If you specify a value of 0, all labels are return,
-- regardless of the default thresholds that the model version applies.
detectCustomLabels_minConfidence :: Lens.Lens' DetectCustomLabels (Core.Maybe Core.Double)
detectCustomLabels_minConfidence = Lens.lens (\DetectCustomLabels' {minConfidence} -> minConfidence) (\s@DetectCustomLabels' {} a -> s {minConfidence = a} :: DetectCustomLabels)

-- | The ARN of the model version that you want to use.
detectCustomLabels_projectVersionArn :: Lens.Lens' DetectCustomLabels Core.Text
detectCustomLabels_projectVersionArn = Lens.lens (\DetectCustomLabels' {projectVersionArn} -> projectVersionArn) (\s@DetectCustomLabels' {} a -> s {projectVersionArn = a} :: DetectCustomLabels)

-- | Undocumented member.
detectCustomLabels_image :: Lens.Lens' DetectCustomLabels Image
detectCustomLabels_image = Lens.lens (\DetectCustomLabels' {image} -> image) (\s@DetectCustomLabels' {} a -> s {image = a} :: DetectCustomLabels)

instance Core.AWSRequest DetectCustomLabels where
  type
    AWSResponse DetectCustomLabels =
      DetectCustomLabelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectCustomLabelsResponse'
            Core.<$> (x Core..?> "CustomLabels" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetectCustomLabels

instance Core.NFData DetectCustomLabels

instance Core.ToHeaders DetectCustomLabels where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DetectCustomLabels" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetectCustomLabels where
  toJSON DetectCustomLabels' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("MinConfidence" Core..=) Core.<$> minConfidence,
            Core.Just
              ("ProjectVersionArn" Core..= projectVersionArn),
            Core.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath DetectCustomLabels where
  toPath = Core.const "/"

instance Core.ToQuery DetectCustomLabels where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetectCustomLabelsResponse' smart constructor.
data DetectCustomLabelsResponse = DetectCustomLabelsResponse'
  { -- | An array of custom labels detected in the input image.
    customLabels :: Core.Maybe [CustomLabel],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectCustomLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabels', 'detectCustomLabelsResponse_customLabels' - An array of custom labels detected in the input image.
--
-- 'httpStatus', 'detectCustomLabelsResponse_httpStatus' - The response's http status code.
newDetectCustomLabelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetectCustomLabelsResponse
newDetectCustomLabelsResponse pHttpStatus_ =
  DetectCustomLabelsResponse'
    { customLabels =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of custom labels detected in the input image.
detectCustomLabelsResponse_customLabels :: Lens.Lens' DetectCustomLabelsResponse (Core.Maybe [CustomLabel])
detectCustomLabelsResponse_customLabels = Lens.lens (\DetectCustomLabelsResponse' {customLabels} -> customLabels) (\s@DetectCustomLabelsResponse' {} a -> s {customLabels = a} :: DetectCustomLabelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectCustomLabelsResponse_httpStatus :: Lens.Lens' DetectCustomLabelsResponse Core.Int
detectCustomLabelsResponse_httpStatus = Lens.lens (\DetectCustomLabelsResponse' {httpStatus} -> httpStatus) (\s@DetectCustomLabelsResponse' {} a -> s {httpStatus = a} :: DetectCustomLabelsResponse)

instance Core.NFData DetectCustomLabelsResponse

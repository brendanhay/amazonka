{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectCustomLabels' smart constructor.
data DetectCustomLabels = DetectCustomLabels'
  { -- | Maximum number of results you want the service to return in the
    -- response. The service returns the specified number of highest confidence
    -- labels ranked from highest confidence to lowest.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon
    -- Rekognition doesn\'t return any labels with a confidence lower than this
    -- specified value. If you specify a value of 0, all labels are return,
    -- regardless of the default thresholds that the model version applies.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | The ARN of the model version that you want to use.
    projectVersionArn :: Prelude.Text,
    image :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'image'
  Image ->
  DetectCustomLabels
newDetectCustomLabels pProjectVersionArn_ pImage_ =
  DetectCustomLabels'
    { maxResults = Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      projectVersionArn = pProjectVersionArn_,
      image = pImage_
    }

-- | Maximum number of results you want the service to return in the
-- response. The service returns the specified number of highest confidence
-- labels ranked from highest confidence to lowest.
detectCustomLabels_maxResults :: Lens.Lens' DetectCustomLabels (Prelude.Maybe Prelude.Natural)
detectCustomLabels_maxResults = Lens.lens (\DetectCustomLabels' {maxResults} -> maxResults) (\s@DetectCustomLabels' {} a -> s {maxResults = a} :: DetectCustomLabels)

-- | Specifies the minimum confidence level for the labels to return. Amazon
-- Rekognition doesn\'t return any labels with a confidence lower than this
-- specified value. If you specify a value of 0, all labels are return,
-- regardless of the default thresholds that the model version applies.
detectCustomLabels_minConfidence :: Lens.Lens' DetectCustomLabels (Prelude.Maybe Prelude.Double)
detectCustomLabels_minConfidence = Lens.lens (\DetectCustomLabels' {minConfidence} -> minConfidence) (\s@DetectCustomLabels' {} a -> s {minConfidence = a} :: DetectCustomLabels)

-- | The ARN of the model version that you want to use.
detectCustomLabels_projectVersionArn :: Lens.Lens' DetectCustomLabels Prelude.Text
detectCustomLabels_projectVersionArn = Lens.lens (\DetectCustomLabels' {projectVersionArn} -> projectVersionArn) (\s@DetectCustomLabels' {} a -> s {projectVersionArn = a} :: DetectCustomLabels)

-- | Undocumented member.
detectCustomLabels_image :: Lens.Lens' DetectCustomLabels Image
detectCustomLabels_image = Lens.lens (\DetectCustomLabels' {image} -> image) (\s@DetectCustomLabels' {} a -> s {image = a} :: DetectCustomLabels)

instance Prelude.AWSRequest DetectCustomLabels where
  type
    Rs DetectCustomLabels =
      DetectCustomLabelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectCustomLabelsResponse'
            Prelude.<$> ( x Prelude..?> "CustomLabels"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectCustomLabels

instance Prelude.NFData DetectCustomLabels

instance Prelude.ToHeaders DetectCustomLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.DetectCustomLabels" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DetectCustomLabels where
  toJSON DetectCustomLabels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("MinConfidence" Prelude..=)
              Prelude.<$> minConfidence,
            Prelude.Just
              ("ProjectVersionArn" Prelude..= projectVersionArn),
            Prelude.Just ("Image" Prelude..= image)
          ]
      )

instance Prelude.ToPath DetectCustomLabels where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetectCustomLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectCustomLabelsResponse' smart constructor.
data DetectCustomLabelsResponse = DetectCustomLabelsResponse'
  { -- | An array of custom labels detected in the input image.
    customLabels :: Prelude.Maybe [CustomLabel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DetectCustomLabelsResponse
newDetectCustomLabelsResponse pHttpStatus_ =
  DetectCustomLabelsResponse'
    { customLabels =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of custom labels detected in the input image.
detectCustomLabelsResponse_customLabels :: Lens.Lens' DetectCustomLabelsResponse (Prelude.Maybe [CustomLabel])
detectCustomLabelsResponse_customLabels = Lens.lens (\DetectCustomLabelsResponse' {customLabels} -> customLabels) (\s@DetectCustomLabelsResponse' {} a -> s {customLabels = a} :: DetectCustomLabelsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
detectCustomLabelsResponse_httpStatus :: Lens.Lens' DetectCustomLabelsResponse Prelude.Int
detectCustomLabelsResponse_httpStatus = Lens.lens (\DetectCustomLabelsResponse' {httpStatus} -> httpStatus) (\s@DetectCustomLabelsResponse' {} a -> s {httpStatus = a} :: DetectCustomLabelsResponse)

instance Prelude.NFData DetectCustomLabelsResponse

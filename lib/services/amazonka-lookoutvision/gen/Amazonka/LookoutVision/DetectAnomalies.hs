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
-- Module      : Amazonka.LookoutVision.DetectAnomalies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects anomalies in an image that you supply.
--
-- The response from @DetectAnomalies@ includes a boolean prediction that
-- the image contains one or more anomalies and a confidence value for the
-- prediction. If the model is an image segmentation model, the response
-- also includes segmentation information for each type of anomaly found in
-- the image.
--
-- Before calling @DetectAnomalies@, you must first start your model with
-- the StartModel operation. You are charged for the amount of time, in
-- minutes, that a model runs and for the number of anomaly detection units
-- that your model uses. If you are not using a model, use the StopModel
-- operation to stop your model.
--
-- For more information, see /Detecting anomalies in an image/ in the
-- Amazon Lookout for Vision developer guide.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DetectAnomalies@ operation.
module Amazonka.LookoutVision.DetectAnomalies
  ( -- * Creating a Request
    DetectAnomalies (..),
    newDetectAnomalies,

    -- * Request Lenses
    detectAnomalies_projectName,
    detectAnomalies_modelVersion,
    detectAnomalies_contentType,
    detectAnomalies_body,

    -- * Destructuring the Response
    DetectAnomaliesResponse (..),
    newDetectAnomaliesResponse,

    -- * Response Lenses
    detectAnomaliesResponse_detectAnomalyResult,
    detectAnomaliesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectAnomalies' smart constructor.
data DetectAnomalies = DetectAnomalies'
  { -- | The name of the project that contains the model version that you want to
    -- use.
    projectName :: Prelude.Text,
    -- | The version of the model that you want to use.
    modelVersion :: Prelude.Text,
    -- | The type of the image passed in @Body@. Valid values are @image\/png@
    -- (PNG format images) and @image\/jpeg@ (JPG format images).
    contentType :: Prelude.Text,
    -- | The unencrypted image bytes that you want to analyze.
    body :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectAnomalies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'detectAnomalies_projectName' - The name of the project that contains the model version that you want to
-- use.
--
-- 'modelVersion', 'detectAnomalies_modelVersion' - The version of the model that you want to use.
--
-- 'contentType', 'detectAnomalies_contentType' - The type of the image passed in @Body@. Valid values are @image\/png@
-- (PNG format images) and @image\/jpeg@ (JPG format images).
--
-- 'body', 'detectAnomalies_body' - The unencrypted image bytes that you want to analyze.
newDetectAnomalies ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  -- | 'contentType'
  Prelude.Text ->
  -- | 'body'
  Data.HashedBody ->
  DetectAnomalies
newDetectAnomalies
  pProjectName_
  pModelVersion_
  pContentType_
  pBody_ =
    DetectAnomalies'
      { projectName = pProjectName_,
        modelVersion = pModelVersion_,
        contentType = pContentType_,
        body = pBody_
      }

-- | The name of the project that contains the model version that you want to
-- use.
detectAnomalies_projectName :: Lens.Lens' DetectAnomalies Prelude.Text
detectAnomalies_projectName = Lens.lens (\DetectAnomalies' {projectName} -> projectName) (\s@DetectAnomalies' {} a -> s {projectName = a} :: DetectAnomalies)

-- | The version of the model that you want to use.
detectAnomalies_modelVersion :: Lens.Lens' DetectAnomalies Prelude.Text
detectAnomalies_modelVersion = Lens.lens (\DetectAnomalies' {modelVersion} -> modelVersion) (\s@DetectAnomalies' {} a -> s {modelVersion = a} :: DetectAnomalies)

-- | The type of the image passed in @Body@. Valid values are @image\/png@
-- (PNG format images) and @image\/jpeg@ (JPG format images).
detectAnomalies_contentType :: Lens.Lens' DetectAnomalies Prelude.Text
detectAnomalies_contentType = Lens.lens (\DetectAnomalies' {contentType} -> contentType) (\s@DetectAnomalies' {} a -> s {contentType = a} :: DetectAnomalies)

-- | The unencrypted image bytes that you want to analyze.
detectAnomalies_body :: Lens.Lens' DetectAnomalies Data.HashedBody
detectAnomalies_body = Lens.lens (\DetectAnomalies' {body} -> body) (\s@DetectAnomalies' {} a -> s {body = a} :: DetectAnomalies)

instance Core.AWSRequest DetectAnomalies where
  type
    AWSResponse DetectAnomalies =
      DetectAnomaliesResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectAnomaliesResponse'
            Prelude.<$> (x Data..?> "DetectAnomalyResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Data.ToBody DetectAnomalies where
  toBody DetectAnomalies' {..} = Data.toBody body

instance Data.ToHeaders DetectAnomalies where
  toHeaders DetectAnomalies' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance Data.ToPath DetectAnomalies where
  toPath DetectAnomalies' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/models/",
        Data.toBS modelVersion,
        "/detect"
      ]

instance Data.ToQuery DetectAnomalies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectAnomaliesResponse' smart constructor.
data DetectAnomaliesResponse = DetectAnomaliesResponse'
  { -- | The results of the @DetectAnomalies@ operation.
    detectAnomalyResult :: Prelude.Maybe DetectAnomalyResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectAnomaliesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectAnomalyResult', 'detectAnomaliesResponse_detectAnomalyResult' - The results of the @DetectAnomalies@ operation.
--
-- 'httpStatus', 'detectAnomaliesResponse_httpStatus' - The response's http status code.
newDetectAnomaliesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectAnomaliesResponse
newDetectAnomaliesResponse pHttpStatus_ =
  DetectAnomaliesResponse'
    { detectAnomalyResult =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The results of the @DetectAnomalies@ operation.
detectAnomaliesResponse_detectAnomalyResult :: Lens.Lens' DetectAnomaliesResponse (Prelude.Maybe DetectAnomalyResult)
detectAnomaliesResponse_detectAnomalyResult = Lens.lens (\DetectAnomaliesResponse' {detectAnomalyResult} -> detectAnomalyResult) (\s@DetectAnomaliesResponse' {} a -> s {detectAnomalyResult = a} :: DetectAnomaliesResponse)

-- | The response's http status code.
detectAnomaliesResponse_httpStatus :: Lens.Lens' DetectAnomaliesResponse Prelude.Int
detectAnomaliesResponse_httpStatus = Lens.lens (\DetectAnomaliesResponse' {httpStatus} -> httpStatus) (\s@DetectAnomaliesResponse' {} a -> s {httpStatus = a} :: DetectAnomaliesResponse)

instance Prelude.NFData DetectAnomaliesResponse where
  rnf DetectAnomaliesResponse' {..} =
    Prelude.rnf detectAnomalyResult
      `Prelude.seq` Prelude.rnf httpStatus

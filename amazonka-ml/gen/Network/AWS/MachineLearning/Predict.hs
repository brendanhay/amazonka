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
-- Module      : Network.AWS.MachineLearning.Predict
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a prediction for the observation using the specified
-- @ML Model@.
--
-- Note
--
-- Not all response parameters will be populated. Whether a response
-- parameter is populated depends on the type of model requested.
module Network.AWS.MachineLearning.Predict
  ( -- * Creating a Request
    Predict (..),
    newPredict,

    -- * Request Lenses
    predict_mLModelId,
    predict_record,
    predict_predictEndpoint,

    -- * Destructuring the Response
    PredictResponse (..),
    newPredictResponse,

    -- * Response Lenses
    predictResponse_prediction,
    predictResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPredict' smart constructor.
data Predict = Predict'
  { -- | A unique identifier of the @MLModel@.
    mLModelId :: Core.Text,
    record :: Core.HashMap Core.Text Core.Text,
    predictEndpoint :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Predict' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'predict_mLModelId' - A unique identifier of the @MLModel@.
--
-- 'record', 'predict_record' - Undocumented member.
--
-- 'predictEndpoint', 'predict_predictEndpoint' - Undocumented member.
newPredict ::
  -- | 'mLModelId'
  Core.Text ->
  -- | 'predictEndpoint'
  Core.Text ->
  Predict
newPredict pMLModelId_ pPredictEndpoint_ =
  Predict'
    { mLModelId = pMLModelId_,
      record = Core.mempty,
      predictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the @MLModel@.
predict_mLModelId :: Lens.Lens' Predict Core.Text
predict_mLModelId = Lens.lens (\Predict' {mLModelId} -> mLModelId) (\s@Predict' {} a -> s {mLModelId = a} :: Predict)

-- | Undocumented member.
predict_record :: Lens.Lens' Predict (Core.HashMap Core.Text Core.Text)
predict_record = Lens.lens (\Predict' {record} -> record) (\s@Predict' {} a -> s {record = a} :: Predict) Core.. Lens._Coerce

-- | Undocumented member.
predict_predictEndpoint :: Lens.Lens' Predict Core.Text
predict_predictEndpoint = Lens.lens (\Predict' {predictEndpoint} -> predictEndpoint) (\s@Predict' {} a -> s {predictEndpoint = a} :: Predict)

instance Core.AWSRequest Predict where
  type AWSResponse Predict = PredictResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PredictResponse'
            Core.<$> (x Core..?> "Prediction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable Predict

instance Core.NFData Predict

instance Core.ToHeaders Predict where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonML_20141212.Predict" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON Predict where
  toJSON Predict' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MLModelId" Core..= mLModelId),
            Core.Just ("Record" Core..= record),
            Core.Just
              ("PredictEndpoint" Core..= predictEndpoint)
          ]
      )

instance Core.ToPath Predict where
  toPath = Core.const "/"

instance Core.ToQuery Predict where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPredictResponse' smart constructor.
data PredictResponse = PredictResponse'
  { prediction :: Core.Maybe Prediction,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PredictResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prediction', 'predictResponse_prediction' - Undocumented member.
--
-- 'httpStatus', 'predictResponse_httpStatus' - The response's http status code.
newPredictResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PredictResponse
newPredictResponse pHttpStatus_ =
  PredictResponse'
    { prediction = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
predictResponse_prediction :: Lens.Lens' PredictResponse (Core.Maybe Prediction)
predictResponse_prediction = Lens.lens (\PredictResponse' {prediction} -> prediction) (\s@PredictResponse' {} a -> s {prediction = a} :: PredictResponse)

-- | The response's http status code.
predictResponse_httpStatus :: Lens.Lens' PredictResponse Core.Int
predictResponse_httpStatus = Lens.lens (\PredictResponse' {httpStatus} -> httpStatus) (\s@PredictResponse' {} a -> s {httpStatus = a} :: PredictResponse)

instance Core.NFData PredictResponse

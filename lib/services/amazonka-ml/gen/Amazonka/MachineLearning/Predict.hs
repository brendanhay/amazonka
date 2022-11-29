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
-- Module      : Amazonka.MachineLearning.Predict
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a prediction for the observation using the specified
-- @ML Model@.
--
-- __Note:__ Not all response parameters will be populated. Whether a
-- response parameter is populated depends on the type of model requested.
module Amazonka.MachineLearning.Predict
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPredict' smart constructor.
data Predict = Predict'
  { -- | A unique identifier of the @MLModel@.
    mLModelId :: Prelude.Text,
    record :: Prelude.HashMap Prelude.Text Prelude.Text,
    predictEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'predictEndpoint'
  Prelude.Text ->
  Predict
newPredict pMLModelId_ pPredictEndpoint_ =
  Predict'
    { mLModelId = pMLModelId_,
      record = Prelude.mempty,
      predictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the @MLModel@.
predict_mLModelId :: Lens.Lens' Predict Prelude.Text
predict_mLModelId = Lens.lens (\Predict' {mLModelId} -> mLModelId) (\s@Predict' {} a -> s {mLModelId = a} :: Predict)

-- | Undocumented member.
predict_record :: Lens.Lens' Predict (Prelude.HashMap Prelude.Text Prelude.Text)
predict_record = Lens.lens (\Predict' {record} -> record) (\s@Predict' {} a -> s {record = a} :: Predict) Prelude.. Lens.coerced

-- | Undocumented member.
predict_predictEndpoint :: Lens.Lens' Predict Prelude.Text
predict_predictEndpoint = Lens.lens (\Predict' {predictEndpoint} -> predictEndpoint) (\s@Predict' {} a -> s {predictEndpoint = a} :: Predict)

instance Core.AWSRequest Predict where
  type AWSResponse Predict = PredictResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PredictResponse'
            Prelude.<$> (x Core..?> "Prediction")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Predict where
  hashWithSalt _salt Predict' {..} =
    _salt `Prelude.hashWithSalt` mLModelId
      `Prelude.hashWithSalt` record
      `Prelude.hashWithSalt` predictEndpoint

instance Prelude.NFData Predict where
  rnf Predict' {..} =
    Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf record
      `Prelude.seq` Prelude.rnf predictEndpoint

instance Core.ToHeaders Predict where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonML_20141212.Predict" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON Predict where
  toJSON Predict' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MLModelId" Core..= mLModelId),
            Prelude.Just ("Record" Core..= record),
            Prelude.Just
              ("PredictEndpoint" Core..= predictEndpoint)
          ]
      )

instance Core.ToPath Predict where
  toPath = Prelude.const "/"

instance Core.ToQuery Predict where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPredictResponse' smart constructor.
data PredictResponse = PredictResponse'
  { prediction :: Prelude.Maybe Prediction,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PredictResponse
newPredictResponse pHttpStatus_ =
  PredictResponse'
    { prediction = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
predictResponse_prediction :: Lens.Lens' PredictResponse (Prelude.Maybe Prediction)
predictResponse_prediction = Lens.lens (\PredictResponse' {prediction} -> prediction) (\s@PredictResponse' {} a -> s {prediction = a} :: PredictResponse)

-- | The response's http status code.
predictResponse_httpStatus :: Lens.Lens' PredictResponse Prelude.Int
predictResponse_httpStatus = Lens.lens (\PredictResponse' {httpStatus} -> httpStatus) (\s@PredictResponse' {} a -> s {httpStatus = a} :: PredictResponse)

instance Prelude.NFData PredictResponse where
  rnf PredictResponse' {..} =
    Prelude.rnf prediction
      `Prelude.seq` Prelude.rnf httpStatus

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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPredict' smart constructor.
data Predict = Predict'
  { -- | A unique identifier of the @MLModel@.
    mLModelId :: Prelude.Text,
    record :: Prelude.HashMap Prelude.Text Prelude.Text,
    predictEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
predict_record = Lens.lens (\Predict' {record} -> record) (\s@Predict' {} a -> s {record = a} :: Predict) Prelude.. Prelude._Coerce

-- | Undocumented member.
predict_predictEndpoint :: Lens.Lens' Predict Prelude.Text
predict_predictEndpoint = Lens.lens (\Predict' {predictEndpoint} -> predictEndpoint) (\s@Predict' {} a -> s {predictEndpoint = a} :: Predict)

instance Prelude.AWSRequest Predict where
  type Rs Predict = PredictResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PredictResponse'
            Prelude.<$> (x Prelude..?> "Prediction")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Predict

instance Prelude.NFData Predict

instance Prelude.ToHeaders Predict where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonML_20141212.Predict" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON Predict where
  toJSON Predict' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MLModelId" Prelude..= mLModelId),
            Prelude.Just ("Record" Prelude..= record),
            Prelude.Just
              ("PredictEndpoint" Prelude..= predictEndpoint)
          ]
      )

instance Prelude.ToPath Predict where
  toPath = Prelude.const "/"

instance Prelude.ToQuery Predict where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPredictResponse' smart constructor.
data PredictResponse = PredictResponse'
  { prediction :: Prelude.Maybe Prediction,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PredictResponse

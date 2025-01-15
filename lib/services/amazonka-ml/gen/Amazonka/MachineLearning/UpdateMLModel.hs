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
-- Module      : Amazonka.MachineLearning.UpdateMLModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @MLModelName@ and the @ScoreThreshold@ of an @MLModel@.
--
-- You can use the @GetMLModel@ operation to view the contents of the
-- updated data element.
module Amazonka.MachineLearning.UpdateMLModel
  ( -- * Creating a Request
    UpdateMLModel (..),
    newUpdateMLModel,

    -- * Request Lenses
    updateMLModel_mLModelName,
    updateMLModel_scoreThreshold,
    updateMLModel_mLModelId,

    -- * Destructuring the Response
    UpdateMLModelResponse (..),
    newUpdateMLModelResponse,

    -- * Response Lenses
    updateMLModelResponse_mLModelId,
    updateMLModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMLModel' smart constructor.
data UpdateMLModel = UpdateMLModel'
  { -- | A user-supplied name or description of the @MLModel@.
    mLModelName :: Prelude.Maybe Prelude.Text,
    -- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
    -- the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the @ScoreThreshold@ receive a
    -- positive result from the @MLModel@, such as @true@. Output values less
    -- than the @ScoreThreshold@ receive a negative response from the
    -- @MLModel@, such as @false@.
    scoreThreshold :: Prelude.Maybe Prelude.Double,
    -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelName', 'updateMLModel_mLModelName' - A user-supplied name or description of the @MLModel@.
--
-- 'scoreThreshold', 'updateMLModel_scoreThreshold' - The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
--
-- 'mLModelId', 'updateMLModel_mLModelId' - The ID assigned to the @MLModel@ during creation.
newUpdateMLModel ::
  -- | 'mLModelId'
  Prelude.Text ->
  UpdateMLModel
newUpdateMLModel pMLModelId_ =
  UpdateMLModel'
    { mLModelName = Prelude.Nothing,
      scoreThreshold = Prelude.Nothing,
      mLModelId = pMLModelId_
    }

-- | A user-supplied name or description of the @MLModel@.
updateMLModel_mLModelName :: Lens.Lens' UpdateMLModel (Prelude.Maybe Prelude.Text)
updateMLModel_mLModelName = Lens.lens (\UpdateMLModel' {mLModelName} -> mLModelName) (\s@UpdateMLModel' {} a -> s {mLModelName = a} :: UpdateMLModel)

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
updateMLModel_scoreThreshold :: Lens.Lens' UpdateMLModel (Prelude.Maybe Prelude.Double)
updateMLModel_scoreThreshold = Lens.lens (\UpdateMLModel' {scoreThreshold} -> scoreThreshold) (\s@UpdateMLModel' {} a -> s {scoreThreshold = a} :: UpdateMLModel)

-- | The ID assigned to the @MLModel@ during creation.
updateMLModel_mLModelId :: Lens.Lens' UpdateMLModel Prelude.Text
updateMLModel_mLModelId = Lens.lens (\UpdateMLModel' {mLModelId} -> mLModelId) (\s@UpdateMLModel' {} a -> s {mLModelId = a} :: UpdateMLModel)

instance Core.AWSRequest UpdateMLModel where
  type
    AWSResponse UpdateMLModel =
      UpdateMLModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLModelResponse'
            Prelude.<$> (x Data..?> "MLModelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMLModel where
  hashWithSalt _salt UpdateMLModel' {..} =
    _salt
      `Prelude.hashWithSalt` mLModelName
      `Prelude.hashWithSalt` scoreThreshold
      `Prelude.hashWithSalt` mLModelId

instance Prelude.NFData UpdateMLModel where
  rnf UpdateMLModel' {..} =
    Prelude.rnf mLModelName `Prelude.seq`
      Prelude.rnf scoreThreshold `Prelude.seq`
        Prelude.rnf mLModelId

instance Data.ToHeaders UpdateMLModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.UpdateMLModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMLModel where
  toJSON UpdateMLModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MLModelName" Data..=) Prelude.<$> mLModelName,
            ("ScoreThreshold" Data..=)
              Prelude.<$> scoreThreshold,
            Prelude.Just ("MLModelId" Data..= mLModelId)
          ]
      )

instance Data.ToPath UpdateMLModel where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMLModel where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdateMLModel@ operation.
--
-- You can see the updated content by using the @GetMLModel@ operation.
--
-- /See:/ 'newUpdateMLModelResponse' smart constructor.
data UpdateMLModelResponse = UpdateMLModelResponse'
  { -- | The ID assigned to the @MLModel@ during creation. This value should be
    -- identical to the value of the @MLModelID@ in the request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMLModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'updateMLModelResponse_mLModelId' - The ID assigned to the @MLModel@ during creation. This value should be
-- identical to the value of the @MLModelID@ in the request.
--
-- 'httpStatus', 'updateMLModelResponse_httpStatus' - The response's http status code.
newUpdateMLModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMLModelResponse
newUpdateMLModelResponse pHttpStatus_ =
  UpdateMLModelResponse'
    { mLModelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID assigned to the @MLModel@ during creation. This value should be
-- identical to the value of the @MLModelID@ in the request.
updateMLModelResponse_mLModelId :: Lens.Lens' UpdateMLModelResponse (Prelude.Maybe Prelude.Text)
updateMLModelResponse_mLModelId = Lens.lens (\UpdateMLModelResponse' {mLModelId} -> mLModelId) (\s@UpdateMLModelResponse' {} a -> s {mLModelId = a} :: UpdateMLModelResponse)

-- | The response's http status code.
updateMLModelResponse_httpStatus :: Lens.Lens' UpdateMLModelResponse Prelude.Int
updateMLModelResponse_httpStatus = Lens.lens (\UpdateMLModelResponse' {httpStatus} -> httpStatus) (\s@UpdateMLModelResponse' {} a -> s {httpStatus = a} :: UpdateMLModelResponse)

instance Prelude.NFData UpdateMLModelResponse where
  rnf UpdateMLModelResponse' {..} =
    Prelude.rnf mLModelId `Prelude.seq`
      Prelude.rnf httpStatus

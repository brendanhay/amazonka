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
-- Module      : Network.AWS.MachineLearning.UpdateMLModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @MLModelName@ and the @ScoreThreshold@ of an @MLModel@.
--
-- You can use the @GetMLModel@ operation to view the contents of the
-- updated data element.
module Network.AWS.MachineLearning.UpdateMLModel
  ( -- * Creating a Request
    UpdateMLModel (..),
    newUpdateMLModel,

    -- * Request Lenses
    updateMLModel_scoreThreshold,
    updateMLModel_mLModelName,
    updateMLModel_mLModelId,

    -- * Destructuring the Response
    UpdateMLModelResponse (..),
    newUpdateMLModelResponse,

    -- * Response Lenses
    updateMLModelResponse_mLModelId,
    updateMLModelResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMLModel' smart constructor.
data UpdateMLModel = UpdateMLModel'
  { -- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
    -- the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the @ScoreThreshold@ receive a
    -- positive result from the @MLModel@, such as @true@. Output values less
    -- than the @ScoreThreshold@ receive a negative response from the
    -- @MLModel@, such as @false@.
    scoreThreshold :: Prelude.Maybe Prelude.Double,
    -- | A user-supplied name or description of the @MLModel@.
    mLModelName :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateMLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scoreThreshold', 'updateMLModel_scoreThreshold' - The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
--
-- 'mLModelName', 'updateMLModel_mLModelName' - A user-supplied name or description of the @MLModel@.
--
-- 'mLModelId', 'updateMLModel_mLModelId' - The ID assigned to the @MLModel@ during creation.
newUpdateMLModel ::
  -- | 'mLModelId'
  Prelude.Text ->
  UpdateMLModel
newUpdateMLModel pMLModelId_ =
  UpdateMLModel'
    { scoreThreshold = Prelude.Nothing,
      mLModelName = Prelude.Nothing,
      mLModelId = pMLModelId_
    }

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
updateMLModel_scoreThreshold :: Lens.Lens' UpdateMLModel (Prelude.Maybe Prelude.Double)
updateMLModel_scoreThreshold = Lens.lens (\UpdateMLModel' {scoreThreshold} -> scoreThreshold) (\s@UpdateMLModel' {} a -> s {scoreThreshold = a} :: UpdateMLModel)

-- | A user-supplied name or description of the @MLModel@.
updateMLModel_mLModelName :: Lens.Lens' UpdateMLModel (Prelude.Maybe Prelude.Text)
updateMLModel_mLModelName = Lens.lens (\UpdateMLModel' {mLModelName} -> mLModelName) (\s@UpdateMLModel' {} a -> s {mLModelName = a} :: UpdateMLModel)

-- | The ID assigned to the @MLModel@ during creation.
updateMLModel_mLModelId :: Lens.Lens' UpdateMLModel Prelude.Text
updateMLModel_mLModelId = Lens.lens (\UpdateMLModel' {mLModelId} -> mLModelId) (\s@UpdateMLModel' {} a -> s {mLModelId = a} :: UpdateMLModel)

instance Prelude.AWSRequest UpdateMLModel where
  type Rs UpdateMLModel = UpdateMLModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLModelResponse'
            Prelude.<$> (x Prelude..?> "MLModelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMLModel

instance Prelude.NFData UpdateMLModel

instance Prelude.ToHeaders UpdateMLModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.UpdateMLModel" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateMLModel where
  toJSON UpdateMLModel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ScoreThreshold" Prelude..=)
              Prelude.<$> scoreThreshold,
            ("MLModelName" Prelude..=) Prelude.<$> mLModelName,
            Prelude.Just ("MLModelId" Prelude..= mLModelId)
          ]
      )

instance Prelude.ToPath UpdateMLModel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateMLModel where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateMLModelResponse

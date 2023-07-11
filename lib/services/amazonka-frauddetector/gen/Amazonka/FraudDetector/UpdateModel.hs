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
-- Module      : Amazonka.FraudDetector.UpdateModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates model description.
module Amazonka.FraudDetector.UpdateModel
  ( -- * Creating a Request
    UpdateModel (..),
    newUpdateModel,

    -- * Request Lenses
    updateModel_description,
    updateModel_modelId,
    updateModel_modelType,

    -- * Destructuring the Response
    UpdateModelResponse (..),
    newUpdateModelResponse,

    -- * Response Lenses
    updateModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { -- | The new model description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateModel_description' - The new model description.
--
-- 'modelId', 'updateModel_modelId' - The model ID.
--
-- 'modelType', 'updateModel_modelType' - The model type.
newUpdateModel ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  UpdateModel
newUpdateModel pModelId_ pModelType_ =
  UpdateModel'
    { description = Prelude.Nothing,
      modelId = pModelId_,
      modelType = pModelType_
    }

-- | The new model description.
updateModel_description :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_description = Lens.lens (\UpdateModel' {description} -> description) (\s@UpdateModel' {} a -> s {description = a} :: UpdateModel)

-- | The model ID.
updateModel_modelId :: Lens.Lens' UpdateModel Prelude.Text
updateModel_modelId = Lens.lens (\UpdateModel' {modelId} -> modelId) (\s@UpdateModel' {} a -> s {modelId = a} :: UpdateModel)

-- | The model type.
updateModel_modelType :: Lens.Lens' UpdateModel ModelTypeEnum
updateModel_modelType = Lens.lens (\UpdateModel' {modelType} -> modelType) (\s@UpdateModel' {} a -> s {modelType = a} :: UpdateModel)

instance Core.AWSRequest UpdateModel where
  type AWSResponse UpdateModel = UpdateModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateModel where
  hashWithSalt _salt UpdateModel' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType

instance Prelude.NFData UpdateModel where
  rnf UpdateModel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType

instance Data.ToHeaders UpdateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModel where
  toJSON UpdateModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType)
          ]
      )

instance Data.ToPath UpdateModel where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelResponse' smart constructor.
data UpdateModelResponse = UpdateModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateModelResponse_httpStatus' - The response's http status code.
newUpdateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateModelResponse
newUpdateModelResponse pHttpStatus_ =
  UpdateModelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateModelResponse_httpStatus :: Lens.Lens' UpdateModelResponse Prelude.Int
updateModelResponse_httpStatus = Lens.lens (\UpdateModelResponse' {httpStatus} -> httpStatus) (\s@UpdateModelResponse' {} a -> s {httpStatus = a} :: UpdateModelResponse)

instance Prelude.NFData UpdateModelResponse where
  rnf UpdateModelResponse' {..} = Prelude.rnf httpStatus

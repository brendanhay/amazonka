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
-- Module      : Amazonka.FraudDetector.UpdateModelVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a model version.
--
-- You can perform the following status updates:
--
-- 1.  Change the @TRAINING_IN_PROGRESS@ status to @TRAINING_CANCELLED@.
--
-- 2.  Change the @TRAINING_COMPLETE@ status to @ACTIVE@.
--
-- 3.  Change @ACTIVE@ to @INACTIVE@.
module Amazonka.FraudDetector.UpdateModelVersionStatus
  ( -- * Creating a Request
    UpdateModelVersionStatus (..),
    newUpdateModelVersionStatus,

    -- * Request Lenses
    updateModelVersionStatus_modelId,
    updateModelVersionStatus_modelType,
    updateModelVersionStatus_modelVersionNumber,
    updateModelVersionStatus_status,

    -- * Destructuring the Response
    UpdateModelVersionStatusResponse (..),
    newUpdateModelVersionStatusResponse,

    -- * Response Lenses
    updateModelVersionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateModelVersionStatus' smart constructor.
data UpdateModelVersionStatus = UpdateModelVersionStatus'
  { -- | The model ID of the model version to update.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum,
    -- | The model version number.
    modelVersionNumber :: Prelude.Text,
    -- | The model version status.
    status :: ModelVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelVersionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelId', 'updateModelVersionStatus_modelId' - The model ID of the model version to update.
--
-- 'modelType', 'updateModelVersionStatus_modelType' - The model type.
--
-- 'modelVersionNumber', 'updateModelVersionStatus_modelVersionNumber' - The model version number.
--
-- 'status', 'updateModelVersionStatus_status' - The model version status.
newUpdateModelVersionStatus ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'modelVersionNumber'
  Prelude.Text ->
  -- | 'status'
  ModelVersionStatus ->
  UpdateModelVersionStatus
newUpdateModelVersionStatus
  pModelId_
  pModelType_
  pModelVersionNumber_
  pStatus_ =
    UpdateModelVersionStatus'
      { modelId = pModelId_,
        modelType = pModelType_,
        modelVersionNumber = pModelVersionNumber_,
        status = pStatus_
      }

-- | The model ID of the model version to update.
updateModelVersionStatus_modelId :: Lens.Lens' UpdateModelVersionStatus Prelude.Text
updateModelVersionStatus_modelId = Lens.lens (\UpdateModelVersionStatus' {modelId} -> modelId) (\s@UpdateModelVersionStatus' {} a -> s {modelId = a} :: UpdateModelVersionStatus)

-- | The model type.
updateModelVersionStatus_modelType :: Lens.Lens' UpdateModelVersionStatus ModelTypeEnum
updateModelVersionStatus_modelType = Lens.lens (\UpdateModelVersionStatus' {modelType} -> modelType) (\s@UpdateModelVersionStatus' {} a -> s {modelType = a} :: UpdateModelVersionStatus)

-- | The model version number.
updateModelVersionStatus_modelVersionNumber :: Lens.Lens' UpdateModelVersionStatus Prelude.Text
updateModelVersionStatus_modelVersionNumber = Lens.lens (\UpdateModelVersionStatus' {modelVersionNumber} -> modelVersionNumber) (\s@UpdateModelVersionStatus' {} a -> s {modelVersionNumber = a} :: UpdateModelVersionStatus)

-- | The model version status.
updateModelVersionStatus_status :: Lens.Lens' UpdateModelVersionStatus ModelVersionStatus
updateModelVersionStatus_status = Lens.lens (\UpdateModelVersionStatus' {status} -> status) (\s@UpdateModelVersionStatus' {} a -> s {status = a} :: UpdateModelVersionStatus)

instance Core.AWSRequest UpdateModelVersionStatus where
  type
    AWSResponse UpdateModelVersionStatus =
      UpdateModelVersionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateModelVersionStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateModelVersionStatus where
  hashWithSalt _salt UpdateModelVersionStatus' {..} =
    _salt `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateModelVersionStatus where
  rnf UpdateModelVersionStatus' {..} =
    Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdateModelVersionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateModelVersionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModelVersionStatus where
  toJSON UpdateModelVersionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("modelVersionNumber" Data..= modelVersionNumber),
            Prelude.Just ("status" Data..= status)
          ]
      )

instance Data.ToPath UpdateModelVersionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateModelVersionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelVersionStatusResponse' smart constructor.
data UpdateModelVersionStatusResponse = UpdateModelVersionStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelVersionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateModelVersionStatusResponse_httpStatus' - The response's http status code.
newUpdateModelVersionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateModelVersionStatusResponse
newUpdateModelVersionStatusResponse pHttpStatus_ =
  UpdateModelVersionStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateModelVersionStatusResponse_httpStatus :: Lens.Lens' UpdateModelVersionStatusResponse Prelude.Int
updateModelVersionStatusResponse_httpStatus = Lens.lens (\UpdateModelVersionStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateModelVersionStatusResponse' {} a -> s {httpStatus = a} :: UpdateModelVersionStatusResponse)

instance
  Prelude.NFData
    UpdateModelVersionStatusResponse
  where
  rnf UpdateModelVersionStatusResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.FraudDetector.DeleteModelVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model version.
--
-- You can delete models and model versions in Amazon Fraud Detector,
-- provided that they are not associated with a detector version.
--
-- When you delete a model version, Amazon Fraud Detector permanently
-- deletes that model version and the data is no longer stored in Amazon
-- Fraud Detector.
module Amazonka.FraudDetector.DeleteModelVersion
  ( -- * Creating a Request
    DeleteModelVersion (..),
    newDeleteModelVersion,

    -- * Request Lenses
    deleteModelVersion_modelId,
    deleteModelVersion_modelType,
    deleteModelVersion_modelVersionNumber,

    -- * Destructuring the Response
    DeleteModelVersionResponse (..),
    newDeleteModelVersionResponse,

    -- * Response Lenses
    deleteModelVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteModelVersion' smart constructor.
data DeleteModelVersion = DeleteModelVersion'
  { -- | The model ID of the model version to delete.
    modelId :: Prelude.Text,
    -- | The model type of the model version to delete.
    modelType :: ModelTypeEnum,
    -- | The model version number of the model version to delete.
    modelVersionNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelId', 'deleteModelVersion_modelId' - The model ID of the model version to delete.
--
-- 'modelType', 'deleteModelVersion_modelType' - The model type of the model version to delete.
--
-- 'modelVersionNumber', 'deleteModelVersion_modelVersionNumber' - The model version number of the model version to delete.
newDeleteModelVersion ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'modelVersionNumber'
  Prelude.Text ->
  DeleteModelVersion
newDeleteModelVersion
  pModelId_
  pModelType_
  pModelVersionNumber_ =
    DeleteModelVersion'
      { modelId = pModelId_,
        modelType = pModelType_,
        modelVersionNumber = pModelVersionNumber_
      }

-- | The model ID of the model version to delete.
deleteModelVersion_modelId :: Lens.Lens' DeleteModelVersion Prelude.Text
deleteModelVersion_modelId = Lens.lens (\DeleteModelVersion' {modelId} -> modelId) (\s@DeleteModelVersion' {} a -> s {modelId = a} :: DeleteModelVersion)

-- | The model type of the model version to delete.
deleteModelVersion_modelType :: Lens.Lens' DeleteModelVersion ModelTypeEnum
deleteModelVersion_modelType = Lens.lens (\DeleteModelVersion' {modelType} -> modelType) (\s@DeleteModelVersion' {} a -> s {modelType = a} :: DeleteModelVersion)

-- | The model version number of the model version to delete.
deleteModelVersion_modelVersionNumber :: Lens.Lens' DeleteModelVersion Prelude.Text
deleteModelVersion_modelVersionNumber = Lens.lens (\DeleteModelVersion' {modelVersionNumber} -> modelVersionNumber) (\s@DeleteModelVersion' {} a -> s {modelVersionNumber = a} :: DeleteModelVersion)

instance Core.AWSRequest DeleteModelVersion where
  type
    AWSResponse DeleteModelVersion =
      DeleteModelVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteModelVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteModelVersion where
  hashWithSalt _salt DeleteModelVersion' {..} =
    _salt `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber

instance Prelude.NFData DeleteModelVersion where
  rnf DeleteModelVersion' {..} =
    Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber

instance Data.ToHeaders DeleteModelVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteModelVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteModelVersion where
  toJSON DeleteModelVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("modelVersionNumber" Data..= modelVersionNumber)
          ]
      )

instance Data.ToPath DeleteModelVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteModelVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelVersionResponse' smart constructor.
data DeleteModelVersionResponse = DeleteModelVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteModelVersionResponse_httpStatus' - The response's http status code.
newDeleteModelVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteModelVersionResponse
newDeleteModelVersionResponse pHttpStatus_ =
  DeleteModelVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteModelVersionResponse_httpStatus :: Lens.Lens' DeleteModelVersionResponse Prelude.Int
deleteModelVersionResponse_httpStatus = Lens.lens (\DeleteModelVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteModelVersionResponse' {} a -> s {httpStatus = a} :: DeleteModelVersionResponse)

instance Prelude.NFData DeleteModelVersionResponse where
  rnf DeleteModelVersionResponse' {..} =
    Prelude.rnf httpStatus

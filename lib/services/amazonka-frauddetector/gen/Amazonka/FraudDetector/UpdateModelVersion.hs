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
-- Module      : Amazonka.FraudDetector.UpdateModelVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a model version. Updating a model version retrains an existing
-- model version using updated training data and produces a new minor
-- version of the model. You can update the training data set location and
-- data access role attributes using this action. This action creates and
-- trains a new minor version of the model, for example version 1.01, 1.02,
-- 1.03.
module Amazonka.FraudDetector.UpdateModelVersion
  ( -- * Creating a Request
    UpdateModelVersion (..),
    newUpdateModelVersion,

    -- * Request Lenses
    updateModelVersion_tags,
    updateModelVersion_ingestedEventsDetail,
    updateModelVersion_externalEventsDetail,
    updateModelVersion_modelId,
    updateModelVersion_modelType,
    updateModelVersion_majorVersionNumber,

    -- * Destructuring the Response
    UpdateModelVersionResponse (..),
    newUpdateModelVersionResponse,

    -- * Response Lenses
    updateModelVersionResponse_modelVersionNumber,
    updateModelVersionResponse_status,
    updateModelVersionResponse_modelType,
    updateModelVersionResponse_modelId,
    updateModelVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateModelVersion' smart constructor.
data UpdateModelVersion = UpdateModelVersion'
  { -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The details of the ingested event used for training the model version.
    -- Required if your @trainingDataSource@ is @INGESTED_EVENTS@.
    ingestedEventsDetail :: Prelude.Maybe IngestedEventsDetail,
    -- | The details of the external events data used for training the model
    -- version. Required if @trainingDataSource@ is @EXTERNAL_EVENTS@.
    externalEventsDetail :: Prelude.Maybe ExternalEventsDetail,
    -- | The model ID.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum,
    -- | The major version number.
    majorVersionNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateModelVersion_tags' - A collection of key and value pairs.
--
-- 'ingestedEventsDetail', 'updateModelVersion_ingestedEventsDetail' - The details of the ingested event used for training the model version.
-- Required if your @trainingDataSource@ is @INGESTED_EVENTS@.
--
-- 'externalEventsDetail', 'updateModelVersion_externalEventsDetail' - The details of the external events data used for training the model
-- version. Required if @trainingDataSource@ is @EXTERNAL_EVENTS@.
--
-- 'modelId', 'updateModelVersion_modelId' - The model ID.
--
-- 'modelType', 'updateModelVersion_modelType' - The model type.
--
-- 'majorVersionNumber', 'updateModelVersion_majorVersionNumber' - The major version number.
newUpdateModelVersion ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'majorVersionNumber'
  Prelude.Text ->
  UpdateModelVersion
newUpdateModelVersion
  pModelId_
  pModelType_
  pMajorVersionNumber_ =
    UpdateModelVersion'
      { tags = Prelude.Nothing,
        ingestedEventsDetail = Prelude.Nothing,
        externalEventsDetail = Prelude.Nothing,
        modelId = pModelId_,
        modelType = pModelType_,
        majorVersionNumber = pMajorVersionNumber_
      }

-- | A collection of key and value pairs.
updateModelVersion_tags :: Lens.Lens' UpdateModelVersion (Prelude.Maybe [Tag])
updateModelVersion_tags = Lens.lens (\UpdateModelVersion' {tags} -> tags) (\s@UpdateModelVersion' {} a -> s {tags = a} :: UpdateModelVersion) Prelude.. Lens.mapping Lens.coerced

-- | The details of the ingested event used for training the model version.
-- Required if your @trainingDataSource@ is @INGESTED_EVENTS@.
updateModelVersion_ingestedEventsDetail :: Lens.Lens' UpdateModelVersion (Prelude.Maybe IngestedEventsDetail)
updateModelVersion_ingestedEventsDetail = Lens.lens (\UpdateModelVersion' {ingestedEventsDetail} -> ingestedEventsDetail) (\s@UpdateModelVersion' {} a -> s {ingestedEventsDetail = a} :: UpdateModelVersion)

-- | The details of the external events data used for training the model
-- version. Required if @trainingDataSource@ is @EXTERNAL_EVENTS@.
updateModelVersion_externalEventsDetail :: Lens.Lens' UpdateModelVersion (Prelude.Maybe ExternalEventsDetail)
updateModelVersion_externalEventsDetail = Lens.lens (\UpdateModelVersion' {externalEventsDetail} -> externalEventsDetail) (\s@UpdateModelVersion' {} a -> s {externalEventsDetail = a} :: UpdateModelVersion)

-- | The model ID.
updateModelVersion_modelId :: Lens.Lens' UpdateModelVersion Prelude.Text
updateModelVersion_modelId = Lens.lens (\UpdateModelVersion' {modelId} -> modelId) (\s@UpdateModelVersion' {} a -> s {modelId = a} :: UpdateModelVersion)

-- | The model type.
updateModelVersion_modelType :: Lens.Lens' UpdateModelVersion ModelTypeEnum
updateModelVersion_modelType = Lens.lens (\UpdateModelVersion' {modelType} -> modelType) (\s@UpdateModelVersion' {} a -> s {modelType = a} :: UpdateModelVersion)

-- | The major version number.
updateModelVersion_majorVersionNumber :: Lens.Lens' UpdateModelVersion Prelude.Text
updateModelVersion_majorVersionNumber = Lens.lens (\UpdateModelVersion' {majorVersionNumber} -> majorVersionNumber) (\s@UpdateModelVersion' {} a -> s {majorVersionNumber = a} :: UpdateModelVersion)

instance Core.AWSRequest UpdateModelVersion where
  type
    AWSResponse UpdateModelVersion =
      UpdateModelVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelVersionResponse'
            Prelude.<$> (x Data..?> "modelVersionNumber")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "modelType")
            Prelude.<*> (x Data..?> "modelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateModelVersion where
  hashWithSalt _salt UpdateModelVersion' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ingestedEventsDetail
      `Prelude.hashWithSalt` externalEventsDetail
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` majorVersionNumber

instance Prelude.NFData UpdateModelVersion where
  rnf UpdateModelVersion' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ingestedEventsDetail
      `Prelude.seq` Prelude.rnf externalEventsDetail
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf majorVersionNumber

instance Data.ToHeaders UpdateModelVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateModelVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModelVersion where
  toJSON UpdateModelVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("ingestedEventsDetail" Data..=)
              Prelude.<$> ingestedEventsDetail,
            ("externalEventsDetail" Data..=)
              Prelude.<$> externalEventsDetail,
            Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("majorVersionNumber" Data..= majorVersionNumber)
          ]
      )

instance Data.ToPath UpdateModelVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateModelVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelVersionResponse' smart constructor.
data UpdateModelVersionResponse = UpdateModelVersionResponse'
  { -- | The model version number of the model version updated.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The status of the updated model version.
    status :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersionNumber', 'updateModelVersionResponse_modelVersionNumber' - The model version number of the model version updated.
--
-- 'status', 'updateModelVersionResponse_status' - The status of the updated model version.
--
-- 'modelType', 'updateModelVersionResponse_modelType' - The model type.
--
-- 'modelId', 'updateModelVersionResponse_modelId' - The model ID.
--
-- 'httpStatus', 'updateModelVersionResponse_httpStatus' - The response's http status code.
newUpdateModelVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateModelVersionResponse
newUpdateModelVersionResponse pHttpStatus_ =
  UpdateModelVersionResponse'
    { modelVersionNumber =
        Prelude.Nothing,
      status = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The model version number of the model version updated.
updateModelVersionResponse_modelVersionNumber :: Lens.Lens' UpdateModelVersionResponse (Prelude.Maybe Prelude.Text)
updateModelVersionResponse_modelVersionNumber = Lens.lens (\UpdateModelVersionResponse' {modelVersionNumber} -> modelVersionNumber) (\s@UpdateModelVersionResponse' {} a -> s {modelVersionNumber = a} :: UpdateModelVersionResponse)

-- | The status of the updated model version.
updateModelVersionResponse_status :: Lens.Lens' UpdateModelVersionResponse (Prelude.Maybe Prelude.Text)
updateModelVersionResponse_status = Lens.lens (\UpdateModelVersionResponse' {status} -> status) (\s@UpdateModelVersionResponse' {} a -> s {status = a} :: UpdateModelVersionResponse)

-- | The model type.
updateModelVersionResponse_modelType :: Lens.Lens' UpdateModelVersionResponse (Prelude.Maybe ModelTypeEnum)
updateModelVersionResponse_modelType = Lens.lens (\UpdateModelVersionResponse' {modelType} -> modelType) (\s@UpdateModelVersionResponse' {} a -> s {modelType = a} :: UpdateModelVersionResponse)

-- | The model ID.
updateModelVersionResponse_modelId :: Lens.Lens' UpdateModelVersionResponse (Prelude.Maybe Prelude.Text)
updateModelVersionResponse_modelId = Lens.lens (\UpdateModelVersionResponse' {modelId} -> modelId) (\s@UpdateModelVersionResponse' {} a -> s {modelId = a} :: UpdateModelVersionResponse)

-- | The response's http status code.
updateModelVersionResponse_httpStatus :: Lens.Lens' UpdateModelVersionResponse Prelude.Int
updateModelVersionResponse_httpStatus = Lens.lens (\UpdateModelVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateModelVersionResponse' {} a -> s {httpStatus = a} :: UpdateModelVersionResponse)

instance Prelude.NFData UpdateModelVersionResponse where
  rnf UpdateModelVersionResponse' {..} =
    Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf httpStatus

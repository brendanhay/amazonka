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
-- Module      : Amazonka.FraudDetector.GetModelVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of the specified model version.
module Amazonka.FraudDetector.GetModelVersion
  ( -- * Creating a Request
    GetModelVersion (..),
    newGetModelVersion,

    -- * Request Lenses
    getModelVersion_modelId,
    getModelVersion_modelType,
    getModelVersion_modelVersionNumber,

    -- * Destructuring the Response
    GetModelVersionResponse (..),
    newGetModelVersionResponse,

    -- * Response Lenses
    getModelVersionResponse_ingestedEventsDetail,
    getModelVersionResponse_modelVersionNumber,
    getModelVersionResponse_arn,
    getModelVersionResponse_status,
    getModelVersionResponse_modelType,
    getModelVersionResponse_trainingDataSchema,
    getModelVersionResponse_externalEventsDetail,
    getModelVersionResponse_trainingDataSource,
    getModelVersionResponse_modelId,
    getModelVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetModelVersion' smart constructor.
data GetModelVersion = GetModelVersion'
  { -- | The model ID.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum,
    -- | The model version number.
    modelVersionNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelId', 'getModelVersion_modelId' - The model ID.
--
-- 'modelType', 'getModelVersion_modelType' - The model type.
--
-- 'modelVersionNumber', 'getModelVersion_modelVersionNumber' - The model version number.
newGetModelVersion ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'modelVersionNumber'
  Prelude.Text ->
  GetModelVersion
newGetModelVersion
  pModelId_
  pModelType_
  pModelVersionNumber_ =
    GetModelVersion'
      { modelId = pModelId_,
        modelType = pModelType_,
        modelVersionNumber = pModelVersionNumber_
      }

-- | The model ID.
getModelVersion_modelId :: Lens.Lens' GetModelVersion Prelude.Text
getModelVersion_modelId = Lens.lens (\GetModelVersion' {modelId} -> modelId) (\s@GetModelVersion' {} a -> s {modelId = a} :: GetModelVersion)

-- | The model type.
getModelVersion_modelType :: Lens.Lens' GetModelVersion ModelTypeEnum
getModelVersion_modelType = Lens.lens (\GetModelVersion' {modelType} -> modelType) (\s@GetModelVersion' {} a -> s {modelType = a} :: GetModelVersion)

-- | The model version number.
getModelVersion_modelVersionNumber :: Lens.Lens' GetModelVersion Prelude.Text
getModelVersion_modelVersionNumber = Lens.lens (\GetModelVersion' {modelVersionNumber} -> modelVersionNumber) (\s@GetModelVersion' {} a -> s {modelVersionNumber = a} :: GetModelVersion)

instance Core.AWSRequest GetModelVersion where
  type
    AWSResponse GetModelVersion =
      GetModelVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelVersionResponse'
            Prelude.<$> (x Data..?> "ingestedEventsDetail")
            Prelude.<*> (x Data..?> "modelVersionNumber")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "modelType")
            Prelude.<*> (x Data..?> "trainingDataSchema")
            Prelude.<*> (x Data..?> "externalEventsDetail")
            Prelude.<*> (x Data..?> "trainingDataSource")
            Prelude.<*> (x Data..?> "modelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModelVersion where
  hashWithSalt _salt GetModelVersion' {..} =
    _salt `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber

instance Prelude.NFData GetModelVersion where
  rnf GetModelVersion' {..} =
    Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber

instance Data.ToHeaders GetModelVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetModelVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetModelVersion where
  toJSON GetModelVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("modelVersionNumber" Data..= modelVersionNumber)
          ]
      )

instance Data.ToPath GetModelVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetModelVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetModelVersionResponse' smart constructor.
data GetModelVersionResponse = GetModelVersionResponse'
  { -- | The details of the ingested events data used for training the model
    -- version. This will be populated if the @trainingDataSource@ is
    -- @INGESTED_EVENTS@.
    ingestedEventsDetail :: Prelude.Maybe IngestedEventsDetail,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The model version status.
    --
    -- Possible values are:
    --
    -- -   @TRAINING_IN_PROGRESS@
    --
    -- -   @TRAINING_COMPLETE@
    --
    -- -   @ACTIVATE_REQUESTED@
    --
    -- -   @ACTIVATE_IN_PROGRESS@
    --
    -- -   @ACTIVE@
    --
    -- -   @INACTIVATE_REQUESTED@
    --
    -- -   @INACTIVATE_IN_PROGRESS@
    --
    -- -   @INACTIVE@
    --
    -- -   @ERROR@
    status :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The training data schema.
    trainingDataSchema :: Prelude.Maybe TrainingDataSchema,
    -- | The details of the external events data used for training the model
    -- version. This will be populated if the @trainingDataSource@ is
    -- @EXTERNAL_EVENTS@
    externalEventsDetail :: Prelude.Maybe ExternalEventsDetail,
    -- | The training data source.
    trainingDataSource :: Prelude.Maybe TrainingDataSourceEnum,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestedEventsDetail', 'getModelVersionResponse_ingestedEventsDetail' - The details of the ingested events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @INGESTED_EVENTS@.
--
-- 'modelVersionNumber', 'getModelVersionResponse_modelVersionNumber' - The model version number.
--
-- 'arn', 'getModelVersionResponse_arn' - The model version ARN.
--
-- 'status', 'getModelVersionResponse_status' - The model version status.
--
-- Possible values are:
--
-- -   @TRAINING_IN_PROGRESS@
--
-- -   @TRAINING_COMPLETE@
--
-- -   @ACTIVATE_REQUESTED@
--
-- -   @ACTIVATE_IN_PROGRESS@
--
-- -   @ACTIVE@
--
-- -   @INACTIVATE_REQUESTED@
--
-- -   @INACTIVATE_IN_PROGRESS@
--
-- -   @INACTIVE@
--
-- -   @ERROR@
--
-- 'modelType', 'getModelVersionResponse_modelType' - The model type.
--
-- 'trainingDataSchema', 'getModelVersionResponse_trainingDataSchema' - The training data schema.
--
-- 'externalEventsDetail', 'getModelVersionResponse_externalEventsDetail' - The details of the external events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @EXTERNAL_EVENTS@
--
-- 'trainingDataSource', 'getModelVersionResponse_trainingDataSource' - The training data source.
--
-- 'modelId', 'getModelVersionResponse_modelId' - The model ID.
--
-- 'httpStatus', 'getModelVersionResponse_httpStatus' - The response's http status code.
newGetModelVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetModelVersionResponse
newGetModelVersionResponse pHttpStatus_ =
  GetModelVersionResponse'
    { ingestedEventsDetail =
        Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      modelType = Prelude.Nothing,
      trainingDataSchema = Prelude.Nothing,
      externalEventsDetail = Prelude.Nothing,
      trainingDataSource = Prelude.Nothing,
      modelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the ingested events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @INGESTED_EVENTS@.
getModelVersionResponse_ingestedEventsDetail :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe IngestedEventsDetail)
getModelVersionResponse_ingestedEventsDetail = Lens.lens (\GetModelVersionResponse' {ingestedEventsDetail} -> ingestedEventsDetail) (\s@GetModelVersionResponse' {} a -> s {ingestedEventsDetail = a} :: GetModelVersionResponse)

-- | The model version number.
getModelVersionResponse_modelVersionNumber :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_modelVersionNumber = Lens.lens (\GetModelVersionResponse' {modelVersionNumber} -> modelVersionNumber) (\s@GetModelVersionResponse' {} a -> s {modelVersionNumber = a} :: GetModelVersionResponse)

-- | The model version ARN.
getModelVersionResponse_arn :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_arn = Lens.lens (\GetModelVersionResponse' {arn} -> arn) (\s@GetModelVersionResponse' {} a -> s {arn = a} :: GetModelVersionResponse)

-- | The model version status.
--
-- Possible values are:
--
-- -   @TRAINING_IN_PROGRESS@
--
-- -   @TRAINING_COMPLETE@
--
-- -   @ACTIVATE_REQUESTED@
--
-- -   @ACTIVATE_IN_PROGRESS@
--
-- -   @ACTIVE@
--
-- -   @INACTIVATE_REQUESTED@
--
-- -   @INACTIVATE_IN_PROGRESS@
--
-- -   @INACTIVE@
--
-- -   @ERROR@
getModelVersionResponse_status :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_status = Lens.lens (\GetModelVersionResponse' {status} -> status) (\s@GetModelVersionResponse' {} a -> s {status = a} :: GetModelVersionResponse)

-- | The model type.
getModelVersionResponse_modelType :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe ModelTypeEnum)
getModelVersionResponse_modelType = Lens.lens (\GetModelVersionResponse' {modelType} -> modelType) (\s@GetModelVersionResponse' {} a -> s {modelType = a} :: GetModelVersionResponse)

-- | The training data schema.
getModelVersionResponse_trainingDataSchema :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe TrainingDataSchema)
getModelVersionResponse_trainingDataSchema = Lens.lens (\GetModelVersionResponse' {trainingDataSchema} -> trainingDataSchema) (\s@GetModelVersionResponse' {} a -> s {trainingDataSchema = a} :: GetModelVersionResponse)

-- | The details of the external events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @EXTERNAL_EVENTS@
getModelVersionResponse_externalEventsDetail :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe ExternalEventsDetail)
getModelVersionResponse_externalEventsDetail = Lens.lens (\GetModelVersionResponse' {externalEventsDetail} -> externalEventsDetail) (\s@GetModelVersionResponse' {} a -> s {externalEventsDetail = a} :: GetModelVersionResponse)

-- | The training data source.
getModelVersionResponse_trainingDataSource :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe TrainingDataSourceEnum)
getModelVersionResponse_trainingDataSource = Lens.lens (\GetModelVersionResponse' {trainingDataSource} -> trainingDataSource) (\s@GetModelVersionResponse' {} a -> s {trainingDataSource = a} :: GetModelVersionResponse)

-- | The model ID.
getModelVersionResponse_modelId :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_modelId = Lens.lens (\GetModelVersionResponse' {modelId} -> modelId) (\s@GetModelVersionResponse' {} a -> s {modelId = a} :: GetModelVersionResponse)

-- | The response's http status code.
getModelVersionResponse_httpStatus :: Lens.Lens' GetModelVersionResponse Prelude.Int
getModelVersionResponse_httpStatus = Lens.lens (\GetModelVersionResponse' {httpStatus} -> httpStatus) (\s@GetModelVersionResponse' {} a -> s {httpStatus = a} :: GetModelVersionResponse)

instance Prelude.NFData GetModelVersionResponse where
  rnf GetModelVersionResponse' {..} =
    Prelude.rnf ingestedEventsDetail
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf trainingDataSchema
      `Prelude.seq` Prelude.rnf externalEventsDetail
      `Prelude.seq` Prelude.rnf trainingDataSource
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf httpStatus

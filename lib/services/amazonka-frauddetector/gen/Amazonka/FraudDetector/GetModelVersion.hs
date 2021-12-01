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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getModelVersionResponse_status,
    getModelVersionResponse_modelType,
    getModelVersionResponse_modelId,
    getModelVersionResponse_arn,
    getModelVersionResponse_trainingDataSource,
    getModelVersionResponse_externalEventsDetail,
    getModelVersionResponse_ingestedEventsDetail,
    getModelVersionResponse_modelVersionNumber,
    getModelVersionResponse_trainingDataSchema,
    getModelVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelVersionResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "modelType")
            Prelude.<*> (x Core..?> "modelId")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "trainingDataSource")
            Prelude.<*> (x Core..?> "externalEventsDetail")
            Prelude.<*> (x Core..?> "ingestedEventsDetail")
            Prelude.<*> (x Core..?> "modelVersionNumber")
            Prelude.<*> (x Core..?> "trainingDataSchema")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModelVersion where
  hashWithSalt salt' GetModelVersion' {..} =
    salt' `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelId

instance Prelude.NFData GetModelVersion where
  rnf GetModelVersion' {..} =
    Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf modelType

instance Core.ToHeaders GetModelVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetModelVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetModelVersion where
  toJSON GetModelVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("modelId" Core..= modelId),
            Prelude.Just ("modelType" Core..= modelType),
            Prelude.Just
              ("modelVersionNumber" Core..= modelVersionNumber)
          ]
      )

instance Core.ToPath GetModelVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery GetModelVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetModelVersionResponse' smart constructor.
data GetModelVersionResponse = GetModelVersionResponse'
  { -- | The model version status.
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
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The training data source.
    trainingDataSource :: Prelude.Maybe TrainingDataSourceEnum,
    -- | The details of the external events data used for training the model
    -- version. This will be populated if the @trainingDataSource@ is
    -- @EXTERNAL_EVENTS@
    externalEventsDetail :: Prelude.Maybe ExternalEventsDetail,
    -- | The details of the ingested events data used for training the model
    -- version. This will be populated if the @trainingDataSource@ is
    -- @INGESTED_EVENTS@.
    ingestedEventsDetail :: Prelude.Maybe IngestedEventsDetail,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The training data schema.
    trainingDataSchema :: Prelude.Maybe TrainingDataSchema,
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
-- 'modelId', 'getModelVersionResponse_modelId' - The model ID.
--
-- 'arn', 'getModelVersionResponse_arn' - The model version ARN.
--
-- 'trainingDataSource', 'getModelVersionResponse_trainingDataSource' - The training data source.
--
-- 'externalEventsDetail', 'getModelVersionResponse_externalEventsDetail' - The details of the external events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @EXTERNAL_EVENTS@
--
-- 'ingestedEventsDetail', 'getModelVersionResponse_ingestedEventsDetail' - The details of the ingested events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @INGESTED_EVENTS@.
--
-- 'modelVersionNumber', 'getModelVersionResponse_modelVersionNumber' - The model version number.
--
-- 'trainingDataSchema', 'getModelVersionResponse_trainingDataSchema' - The training data schema.
--
-- 'httpStatus', 'getModelVersionResponse_httpStatus' - The response's http status code.
newGetModelVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetModelVersionResponse
newGetModelVersionResponse pHttpStatus_ =
  GetModelVersionResponse'
    { status = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelId = Prelude.Nothing,
      arn = Prelude.Nothing,
      trainingDataSource = Prelude.Nothing,
      externalEventsDetail = Prelude.Nothing,
      ingestedEventsDetail = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      trainingDataSchema = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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

-- | The model ID.
getModelVersionResponse_modelId :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_modelId = Lens.lens (\GetModelVersionResponse' {modelId} -> modelId) (\s@GetModelVersionResponse' {} a -> s {modelId = a} :: GetModelVersionResponse)

-- | The model version ARN.
getModelVersionResponse_arn :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_arn = Lens.lens (\GetModelVersionResponse' {arn} -> arn) (\s@GetModelVersionResponse' {} a -> s {arn = a} :: GetModelVersionResponse)

-- | The training data source.
getModelVersionResponse_trainingDataSource :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe TrainingDataSourceEnum)
getModelVersionResponse_trainingDataSource = Lens.lens (\GetModelVersionResponse' {trainingDataSource} -> trainingDataSource) (\s@GetModelVersionResponse' {} a -> s {trainingDataSource = a} :: GetModelVersionResponse)

-- | The details of the external events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @EXTERNAL_EVENTS@
getModelVersionResponse_externalEventsDetail :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe ExternalEventsDetail)
getModelVersionResponse_externalEventsDetail = Lens.lens (\GetModelVersionResponse' {externalEventsDetail} -> externalEventsDetail) (\s@GetModelVersionResponse' {} a -> s {externalEventsDetail = a} :: GetModelVersionResponse)

-- | The details of the ingested events data used for training the model
-- version. This will be populated if the @trainingDataSource@ is
-- @INGESTED_EVENTS@.
getModelVersionResponse_ingestedEventsDetail :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe IngestedEventsDetail)
getModelVersionResponse_ingestedEventsDetail = Lens.lens (\GetModelVersionResponse' {ingestedEventsDetail} -> ingestedEventsDetail) (\s@GetModelVersionResponse' {} a -> s {ingestedEventsDetail = a} :: GetModelVersionResponse)

-- | The model version number.
getModelVersionResponse_modelVersionNumber :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe Prelude.Text)
getModelVersionResponse_modelVersionNumber = Lens.lens (\GetModelVersionResponse' {modelVersionNumber} -> modelVersionNumber) (\s@GetModelVersionResponse' {} a -> s {modelVersionNumber = a} :: GetModelVersionResponse)

-- | The training data schema.
getModelVersionResponse_trainingDataSchema :: Lens.Lens' GetModelVersionResponse (Prelude.Maybe TrainingDataSchema)
getModelVersionResponse_trainingDataSchema = Lens.lens (\GetModelVersionResponse' {trainingDataSchema} -> trainingDataSchema) (\s@GetModelVersionResponse' {} a -> s {trainingDataSchema = a} :: GetModelVersionResponse)

-- | The response's http status code.
getModelVersionResponse_httpStatus :: Lens.Lens' GetModelVersionResponse Prelude.Int
getModelVersionResponse_httpStatus = Lens.lens (\GetModelVersionResponse' {httpStatus} -> httpStatus) (\s@GetModelVersionResponse' {} a -> s {httpStatus = a} :: GetModelVersionResponse)

instance Prelude.NFData GetModelVersionResponse where
  rnf GetModelVersionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trainingDataSchema
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf ingestedEventsDetail
      `Prelude.seq` Prelude.rnf externalEventsDetail
      `Prelude.seq` Prelude.rnf trainingDataSource
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType

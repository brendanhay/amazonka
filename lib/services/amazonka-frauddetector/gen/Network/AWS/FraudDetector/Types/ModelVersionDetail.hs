{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Types.ModelVersionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelVersionDetail where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.ExternalEventsDetail
import Amazonka.FraudDetector.Types.IngestedEventsDetail
import Amazonka.FraudDetector.Types.ModelTypeEnum
import Amazonka.FraudDetector.Types.TrainingDataSchema
import Amazonka.FraudDetector.Types.TrainingDataSourceEnum
import Amazonka.FraudDetector.Types.TrainingResult
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the model version.
--
-- /See:/ 'newModelVersionDetail' smart constructor.
data ModelVersionDetail = ModelVersionDetail'
  { -- | The status of the model version.
    status :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The timestamp when the model was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The model version training data source.
    trainingDataSource :: Prelude.Maybe TrainingDataSourceEnum,
    -- | The timestamp when the model was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The external events data details. This will be populated if the
    -- @trainingDataSource@ for the model version is specified as
    -- @EXTERNAL_EVENTS@.
    externalEventsDetail :: Prelude.Maybe ExternalEventsDetail,
    -- | The ingested events data details. This will be populated if the
    -- @trainingDataSource@ for the model version is specified as
    -- @INGESTED_EVENTS@.
    ingestedEventsDetail :: Prelude.Maybe IngestedEventsDetail,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The training results.
    trainingResult :: Prelude.Maybe TrainingResult,
    -- | The training data schema.
    trainingDataSchema :: Prelude.Maybe TrainingDataSchema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelVersionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'modelVersionDetail_status' - The status of the model version.
--
-- 'modelType', 'modelVersionDetail_modelType' - The model type.
--
-- 'lastUpdatedTime', 'modelVersionDetail_lastUpdatedTime' - The timestamp when the model was last updated.
--
-- 'modelId', 'modelVersionDetail_modelId' - The model ID.
--
-- 'arn', 'modelVersionDetail_arn' - The model version ARN.
--
-- 'trainingDataSource', 'modelVersionDetail_trainingDataSource' - The model version training data source.
--
-- 'createdTime', 'modelVersionDetail_createdTime' - The timestamp when the model was created.
--
-- 'externalEventsDetail', 'modelVersionDetail_externalEventsDetail' - The external events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @EXTERNAL_EVENTS@.
--
-- 'ingestedEventsDetail', 'modelVersionDetail_ingestedEventsDetail' - The ingested events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @INGESTED_EVENTS@.
--
-- 'modelVersionNumber', 'modelVersionDetail_modelVersionNumber' - The model version number.
--
-- 'trainingResult', 'modelVersionDetail_trainingResult' - The training results.
--
-- 'trainingDataSchema', 'modelVersionDetail_trainingDataSchema' - The training data schema.
newModelVersionDetail ::
  ModelVersionDetail
newModelVersionDetail =
  ModelVersionDetail'
    { status = Prelude.Nothing,
      modelType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelId = Prelude.Nothing,
      arn = Prelude.Nothing,
      trainingDataSource = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      externalEventsDetail = Prelude.Nothing,
      ingestedEventsDetail = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      trainingResult = Prelude.Nothing,
      trainingDataSchema = Prelude.Nothing
    }

-- | The status of the model version.
modelVersionDetail_status :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_status = Lens.lens (\ModelVersionDetail' {status} -> status) (\s@ModelVersionDetail' {} a -> s {status = a} :: ModelVersionDetail)

-- | The model type.
modelVersionDetail_modelType :: Lens.Lens' ModelVersionDetail (Prelude.Maybe ModelTypeEnum)
modelVersionDetail_modelType = Lens.lens (\ModelVersionDetail' {modelType} -> modelType) (\s@ModelVersionDetail' {} a -> s {modelType = a} :: ModelVersionDetail)

-- | The timestamp when the model was last updated.
modelVersionDetail_lastUpdatedTime :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_lastUpdatedTime = Lens.lens (\ModelVersionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ModelVersionDetail' {} a -> s {lastUpdatedTime = a} :: ModelVersionDetail)

-- | The model ID.
modelVersionDetail_modelId :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelId = Lens.lens (\ModelVersionDetail' {modelId} -> modelId) (\s@ModelVersionDetail' {} a -> s {modelId = a} :: ModelVersionDetail)

-- | The model version ARN.
modelVersionDetail_arn :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_arn = Lens.lens (\ModelVersionDetail' {arn} -> arn) (\s@ModelVersionDetail' {} a -> s {arn = a} :: ModelVersionDetail)

-- | The model version training data source.
modelVersionDetail_trainingDataSource :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSourceEnum)
modelVersionDetail_trainingDataSource = Lens.lens (\ModelVersionDetail' {trainingDataSource} -> trainingDataSource) (\s@ModelVersionDetail' {} a -> s {trainingDataSource = a} :: ModelVersionDetail)

-- | The timestamp when the model was created.
modelVersionDetail_createdTime :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_createdTime = Lens.lens (\ModelVersionDetail' {createdTime} -> createdTime) (\s@ModelVersionDetail' {} a -> s {createdTime = a} :: ModelVersionDetail)

-- | The external events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @EXTERNAL_EVENTS@.
modelVersionDetail_externalEventsDetail :: Lens.Lens' ModelVersionDetail (Prelude.Maybe ExternalEventsDetail)
modelVersionDetail_externalEventsDetail = Lens.lens (\ModelVersionDetail' {externalEventsDetail} -> externalEventsDetail) (\s@ModelVersionDetail' {} a -> s {externalEventsDetail = a} :: ModelVersionDetail)

-- | The ingested events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @INGESTED_EVENTS@.
modelVersionDetail_ingestedEventsDetail :: Lens.Lens' ModelVersionDetail (Prelude.Maybe IngestedEventsDetail)
modelVersionDetail_ingestedEventsDetail = Lens.lens (\ModelVersionDetail' {ingestedEventsDetail} -> ingestedEventsDetail) (\s@ModelVersionDetail' {} a -> s {ingestedEventsDetail = a} :: ModelVersionDetail)

-- | The model version number.
modelVersionDetail_modelVersionNumber :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelVersionNumber = Lens.lens (\ModelVersionDetail' {modelVersionNumber} -> modelVersionNumber) (\s@ModelVersionDetail' {} a -> s {modelVersionNumber = a} :: ModelVersionDetail)

-- | The training results.
modelVersionDetail_trainingResult :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingResult)
modelVersionDetail_trainingResult = Lens.lens (\ModelVersionDetail' {trainingResult} -> trainingResult) (\s@ModelVersionDetail' {} a -> s {trainingResult = a} :: ModelVersionDetail)

-- | The training data schema.
modelVersionDetail_trainingDataSchema :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSchema)
modelVersionDetail_trainingDataSchema = Lens.lens (\ModelVersionDetail' {trainingDataSchema} -> trainingDataSchema) (\s@ModelVersionDetail' {} a -> s {trainingDataSchema = a} :: ModelVersionDetail)

instance Core.FromJSON ModelVersionDetail where
  parseJSON =
    Core.withObject
      "ModelVersionDetail"
      ( \x ->
          ModelVersionDetail'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "modelType")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "modelId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "trainingDataSource")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "externalEventsDetail")
            Prelude.<*> (x Core..:? "ingestedEventsDetail")
            Prelude.<*> (x Core..:? "modelVersionNumber")
            Prelude.<*> (x Core..:? "trainingResult")
            Prelude.<*> (x Core..:? "trainingDataSchema")
      )

instance Prelude.Hashable ModelVersionDetail

instance Prelude.NFData ModelVersionDetail

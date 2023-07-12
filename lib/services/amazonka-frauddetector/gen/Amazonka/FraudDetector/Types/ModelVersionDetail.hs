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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelVersionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ExternalEventsDetail
import Amazonka.FraudDetector.Types.IngestedEventsDetail
import Amazonka.FraudDetector.Types.ModelTypeEnum
import Amazonka.FraudDetector.Types.TrainingDataSchema
import Amazonka.FraudDetector.Types.TrainingDataSourceEnum
import Amazonka.FraudDetector.Types.TrainingResult
import Amazonka.FraudDetector.Types.TrainingResultV2
import qualified Amazonka.Prelude as Prelude

-- | The details of the model version.
--
-- /See:/ 'newModelVersionDetail' smart constructor.
data ModelVersionDetail = ModelVersionDetail'
  { -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
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
    -- | The timestamp when the model was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The status of the model version.
    status :: Prelude.Maybe Prelude.Text,
    -- | The training data schema.
    trainingDataSchema :: Prelude.Maybe TrainingDataSchema,
    -- | The model version training data source.
    trainingDataSource :: Prelude.Maybe TrainingDataSourceEnum,
    -- | The training results.
    trainingResult :: Prelude.Maybe TrainingResult,
    -- | The training result details. The details include the relative importance
    -- of the variables.
    trainingResultV2 :: Prelude.Maybe TrainingResultV2
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
-- 'arn', 'modelVersionDetail_arn' - The model version ARN.
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
-- 'lastUpdatedTime', 'modelVersionDetail_lastUpdatedTime' - The timestamp when the model was last updated.
--
-- 'modelId', 'modelVersionDetail_modelId' - The model ID.
--
-- 'modelType', 'modelVersionDetail_modelType' - The model type.
--
-- 'modelVersionNumber', 'modelVersionDetail_modelVersionNumber' - The model version number.
--
-- 'status', 'modelVersionDetail_status' - The status of the model version.
--
-- 'trainingDataSchema', 'modelVersionDetail_trainingDataSchema' - The training data schema.
--
-- 'trainingDataSource', 'modelVersionDetail_trainingDataSource' - The model version training data source.
--
-- 'trainingResult', 'modelVersionDetail_trainingResult' - The training results.
--
-- 'trainingResultV2', 'modelVersionDetail_trainingResultV2' - The training result details. The details include the relative importance
-- of the variables.
newModelVersionDetail ::
  ModelVersionDetail
newModelVersionDetail =
  ModelVersionDetail'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      externalEventsDetail = Prelude.Nothing,
      ingestedEventsDetail = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelId = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      status = Prelude.Nothing,
      trainingDataSchema = Prelude.Nothing,
      trainingDataSource = Prelude.Nothing,
      trainingResult = Prelude.Nothing,
      trainingResultV2 = Prelude.Nothing
    }

-- | The model version ARN.
modelVersionDetail_arn :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_arn = Lens.lens (\ModelVersionDetail' {arn} -> arn) (\s@ModelVersionDetail' {} a -> s {arn = a} :: ModelVersionDetail)

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

-- | The timestamp when the model was last updated.
modelVersionDetail_lastUpdatedTime :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_lastUpdatedTime = Lens.lens (\ModelVersionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ModelVersionDetail' {} a -> s {lastUpdatedTime = a} :: ModelVersionDetail)

-- | The model ID.
modelVersionDetail_modelId :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelId = Lens.lens (\ModelVersionDetail' {modelId} -> modelId) (\s@ModelVersionDetail' {} a -> s {modelId = a} :: ModelVersionDetail)

-- | The model type.
modelVersionDetail_modelType :: Lens.Lens' ModelVersionDetail (Prelude.Maybe ModelTypeEnum)
modelVersionDetail_modelType = Lens.lens (\ModelVersionDetail' {modelType} -> modelType) (\s@ModelVersionDetail' {} a -> s {modelType = a} :: ModelVersionDetail)

-- | The model version number.
modelVersionDetail_modelVersionNumber :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelVersionNumber = Lens.lens (\ModelVersionDetail' {modelVersionNumber} -> modelVersionNumber) (\s@ModelVersionDetail' {} a -> s {modelVersionNumber = a} :: ModelVersionDetail)

-- | The status of the model version.
modelVersionDetail_status :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_status = Lens.lens (\ModelVersionDetail' {status} -> status) (\s@ModelVersionDetail' {} a -> s {status = a} :: ModelVersionDetail)

-- | The training data schema.
modelVersionDetail_trainingDataSchema :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSchema)
modelVersionDetail_trainingDataSchema = Lens.lens (\ModelVersionDetail' {trainingDataSchema} -> trainingDataSchema) (\s@ModelVersionDetail' {} a -> s {trainingDataSchema = a} :: ModelVersionDetail)

-- | The model version training data source.
modelVersionDetail_trainingDataSource :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSourceEnum)
modelVersionDetail_trainingDataSource = Lens.lens (\ModelVersionDetail' {trainingDataSource} -> trainingDataSource) (\s@ModelVersionDetail' {} a -> s {trainingDataSource = a} :: ModelVersionDetail)

-- | The training results.
modelVersionDetail_trainingResult :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingResult)
modelVersionDetail_trainingResult = Lens.lens (\ModelVersionDetail' {trainingResult} -> trainingResult) (\s@ModelVersionDetail' {} a -> s {trainingResult = a} :: ModelVersionDetail)

-- | The training result details. The details include the relative importance
-- of the variables.
modelVersionDetail_trainingResultV2 :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingResultV2)
modelVersionDetail_trainingResultV2 = Lens.lens (\ModelVersionDetail' {trainingResultV2} -> trainingResultV2) (\s@ModelVersionDetail' {} a -> s {trainingResultV2 = a} :: ModelVersionDetail)

instance Data.FromJSON ModelVersionDetail where
  parseJSON =
    Data.withObject
      "ModelVersionDetail"
      ( \x ->
          ModelVersionDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "externalEventsDetail")
            Prelude.<*> (x Data..:? "ingestedEventsDetail")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "modelId")
            Prelude.<*> (x Data..:? "modelType")
            Prelude.<*> (x Data..:? "modelVersionNumber")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "trainingDataSchema")
            Prelude.<*> (x Data..:? "trainingDataSource")
            Prelude.<*> (x Data..:? "trainingResult")
            Prelude.<*> (x Data..:? "trainingResultV2")
      )

instance Prelude.Hashable ModelVersionDetail where
  hashWithSalt _salt ModelVersionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` externalEventsDetail
      `Prelude.hashWithSalt` ingestedEventsDetail
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trainingDataSchema
      `Prelude.hashWithSalt` trainingDataSource
      `Prelude.hashWithSalt` trainingResult
      `Prelude.hashWithSalt` trainingResultV2

instance Prelude.NFData ModelVersionDetail where
  rnf ModelVersionDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf externalEventsDetail
      `Prelude.seq` Prelude.rnf ingestedEventsDetail
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf trainingDataSchema
      `Prelude.seq` Prelude.rnf trainingDataSource
      `Prelude.seq` Prelude.rnf trainingResult
      `Prelude.seq` Prelude.rnf trainingResultV2

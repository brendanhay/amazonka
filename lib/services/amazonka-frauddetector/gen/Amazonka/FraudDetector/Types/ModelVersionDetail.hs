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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelVersionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The timestamp when the model was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The ingested events data details. This will be populated if the
    -- @trainingDataSource@ for the model version is specified as
    -- @INGESTED_EVENTS@.
    ingestedEventsDetail :: Prelude.Maybe IngestedEventsDetail,
    -- | The training result details. The details include the relative importance
    -- of the variables.
    trainingResultV2 :: Prelude.Maybe TrainingResultV2,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the model version.
    status :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the model was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The training data schema.
    trainingDataSchema :: Prelude.Maybe TrainingDataSchema,
    -- | The training results.
    trainingResult :: Prelude.Maybe TrainingResult,
    -- | The external events data details. This will be populated if the
    -- @trainingDataSource@ for the model version is specified as
    -- @EXTERNAL_EVENTS@.
    externalEventsDetail :: Prelude.Maybe ExternalEventsDetail,
    -- | The model version training data source.
    trainingDataSource :: Prelude.Maybe TrainingDataSourceEnum,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text
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
-- 'createdTime', 'modelVersionDetail_createdTime' - The timestamp when the model was created.
--
-- 'ingestedEventsDetail', 'modelVersionDetail_ingestedEventsDetail' - The ingested events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @INGESTED_EVENTS@.
--
-- 'trainingResultV2', 'modelVersionDetail_trainingResultV2' - The training result details. The details include the relative importance
-- of the variables.
--
-- 'modelVersionNumber', 'modelVersionDetail_modelVersionNumber' - The model version number.
--
-- 'arn', 'modelVersionDetail_arn' - The model version ARN.
--
-- 'status', 'modelVersionDetail_status' - The status of the model version.
--
-- 'lastUpdatedTime', 'modelVersionDetail_lastUpdatedTime' - The timestamp when the model was last updated.
--
-- 'modelType', 'modelVersionDetail_modelType' - The model type.
--
-- 'trainingDataSchema', 'modelVersionDetail_trainingDataSchema' - The training data schema.
--
-- 'trainingResult', 'modelVersionDetail_trainingResult' - The training results.
--
-- 'externalEventsDetail', 'modelVersionDetail_externalEventsDetail' - The external events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @EXTERNAL_EVENTS@.
--
-- 'trainingDataSource', 'modelVersionDetail_trainingDataSource' - The model version training data source.
--
-- 'modelId', 'modelVersionDetail_modelId' - The model ID.
newModelVersionDetail ::
  ModelVersionDetail
newModelVersionDetail =
  ModelVersionDetail'
    { createdTime = Prelude.Nothing,
      ingestedEventsDetail = Prelude.Nothing,
      trainingResultV2 = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelType = Prelude.Nothing,
      trainingDataSchema = Prelude.Nothing,
      trainingResult = Prelude.Nothing,
      externalEventsDetail = Prelude.Nothing,
      trainingDataSource = Prelude.Nothing,
      modelId = Prelude.Nothing
    }

-- | The timestamp when the model was created.
modelVersionDetail_createdTime :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_createdTime = Lens.lens (\ModelVersionDetail' {createdTime} -> createdTime) (\s@ModelVersionDetail' {} a -> s {createdTime = a} :: ModelVersionDetail)

-- | The ingested events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @INGESTED_EVENTS@.
modelVersionDetail_ingestedEventsDetail :: Lens.Lens' ModelVersionDetail (Prelude.Maybe IngestedEventsDetail)
modelVersionDetail_ingestedEventsDetail = Lens.lens (\ModelVersionDetail' {ingestedEventsDetail} -> ingestedEventsDetail) (\s@ModelVersionDetail' {} a -> s {ingestedEventsDetail = a} :: ModelVersionDetail)

-- | The training result details. The details include the relative importance
-- of the variables.
modelVersionDetail_trainingResultV2 :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingResultV2)
modelVersionDetail_trainingResultV2 = Lens.lens (\ModelVersionDetail' {trainingResultV2} -> trainingResultV2) (\s@ModelVersionDetail' {} a -> s {trainingResultV2 = a} :: ModelVersionDetail)

-- | The model version number.
modelVersionDetail_modelVersionNumber :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelVersionNumber = Lens.lens (\ModelVersionDetail' {modelVersionNumber} -> modelVersionNumber) (\s@ModelVersionDetail' {} a -> s {modelVersionNumber = a} :: ModelVersionDetail)

-- | The model version ARN.
modelVersionDetail_arn :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_arn = Lens.lens (\ModelVersionDetail' {arn} -> arn) (\s@ModelVersionDetail' {} a -> s {arn = a} :: ModelVersionDetail)

-- | The status of the model version.
modelVersionDetail_status :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_status = Lens.lens (\ModelVersionDetail' {status} -> status) (\s@ModelVersionDetail' {} a -> s {status = a} :: ModelVersionDetail)

-- | The timestamp when the model was last updated.
modelVersionDetail_lastUpdatedTime :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_lastUpdatedTime = Lens.lens (\ModelVersionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ModelVersionDetail' {} a -> s {lastUpdatedTime = a} :: ModelVersionDetail)

-- | The model type.
modelVersionDetail_modelType :: Lens.Lens' ModelVersionDetail (Prelude.Maybe ModelTypeEnum)
modelVersionDetail_modelType = Lens.lens (\ModelVersionDetail' {modelType} -> modelType) (\s@ModelVersionDetail' {} a -> s {modelType = a} :: ModelVersionDetail)

-- | The training data schema.
modelVersionDetail_trainingDataSchema :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSchema)
modelVersionDetail_trainingDataSchema = Lens.lens (\ModelVersionDetail' {trainingDataSchema} -> trainingDataSchema) (\s@ModelVersionDetail' {} a -> s {trainingDataSchema = a} :: ModelVersionDetail)

-- | The training results.
modelVersionDetail_trainingResult :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingResult)
modelVersionDetail_trainingResult = Lens.lens (\ModelVersionDetail' {trainingResult} -> trainingResult) (\s@ModelVersionDetail' {} a -> s {trainingResult = a} :: ModelVersionDetail)

-- | The external events data details. This will be populated if the
-- @trainingDataSource@ for the model version is specified as
-- @EXTERNAL_EVENTS@.
modelVersionDetail_externalEventsDetail :: Lens.Lens' ModelVersionDetail (Prelude.Maybe ExternalEventsDetail)
modelVersionDetail_externalEventsDetail = Lens.lens (\ModelVersionDetail' {externalEventsDetail} -> externalEventsDetail) (\s@ModelVersionDetail' {} a -> s {externalEventsDetail = a} :: ModelVersionDetail)

-- | The model version training data source.
modelVersionDetail_trainingDataSource :: Lens.Lens' ModelVersionDetail (Prelude.Maybe TrainingDataSourceEnum)
modelVersionDetail_trainingDataSource = Lens.lens (\ModelVersionDetail' {trainingDataSource} -> trainingDataSource) (\s@ModelVersionDetail' {} a -> s {trainingDataSource = a} :: ModelVersionDetail)

-- | The model ID.
modelVersionDetail_modelId :: Lens.Lens' ModelVersionDetail (Prelude.Maybe Prelude.Text)
modelVersionDetail_modelId = Lens.lens (\ModelVersionDetail' {modelId} -> modelId) (\s@ModelVersionDetail' {} a -> s {modelId = a} :: ModelVersionDetail)

instance Core.FromJSON ModelVersionDetail where
  parseJSON =
    Core.withObject
      "ModelVersionDetail"
      ( \x ->
          ModelVersionDetail'
            Prelude.<$> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "ingestedEventsDetail")
            Prelude.<*> (x Core..:? "trainingResultV2")
            Prelude.<*> (x Core..:? "modelVersionNumber")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "modelType")
            Prelude.<*> (x Core..:? "trainingDataSchema")
            Prelude.<*> (x Core..:? "trainingResult")
            Prelude.<*> (x Core..:? "externalEventsDetail")
            Prelude.<*> (x Core..:? "trainingDataSource")
            Prelude.<*> (x Core..:? "modelId")
      )

instance Prelude.Hashable ModelVersionDetail where
  hashWithSalt _salt ModelVersionDetail' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` ingestedEventsDetail
      `Prelude.hashWithSalt` trainingResultV2
      `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` trainingDataSchema
      `Prelude.hashWithSalt` trainingResult
      `Prelude.hashWithSalt` externalEventsDetail
      `Prelude.hashWithSalt` trainingDataSource
      `Prelude.hashWithSalt` modelId

instance Prelude.NFData ModelVersionDetail where
  rnf ModelVersionDetail' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf ingestedEventsDetail
      `Prelude.seq` Prelude.rnf trainingResultV2
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf trainingDataSchema
      `Prelude.seq` Prelude.rnf trainingResult
      `Prelude.seq` Prelude.rnf externalEventsDetail
      `Prelude.seq` Prelude.rnf trainingDataSource
      `Prelude.seq` Prelude.rnf modelId

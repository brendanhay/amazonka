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
-- Module      : Amazonka.Forecast.DescribeAutoPredictor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a predictor created using the CreateAutoPredictor operation.
module Amazonka.Forecast.DescribeAutoPredictor
  ( -- * Creating a Request
    DescribeAutoPredictor (..),
    newDescribeAutoPredictor,

    -- * Request Lenses
    describeAutoPredictor_predictorArn,

    -- * Destructuring the Response
    DescribeAutoPredictorResponse (..),
    newDescribeAutoPredictorResponse,

    -- * Response Lenses
    describeAutoPredictorResponse_lastModificationTime,
    describeAutoPredictorResponse_encryptionConfig,
    describeAutoPredictorResponse_message,
    describeAutoPredictorResponse_explainabilityInfo,
    describeAutoPredictorResponse_forecastDimensions,
    describeAutoPredictorResponse_optimizationMetric,
    describeAutoPredictorResponse_monitorInfo,
    describeAutoPredictorResponse_forecastTypes,
    describeAutoPredictorResponse_predictorName,
    describeAutoPredictorResponse_status,
    describeAutoPredictorResponse_estimatedTimeRemainingInMinutes,
    describeAutoPredictorResponse_forecastHorizon,
    describeAutoPredictorResponse_predictorArn,
    describeAutoPredictorResponse_datasetImportJobArns,
    describeAutoPredictorResponse_dataConfig,
    describeAutoPredictorResponse_creationTime,
    describeAutoPredictorResponse_timeAlignmentBoundary,
    describeAutoPredictorResponse_referencePredictorSummary,
    describeAutoPredictorResponse_forecastFrequency,
    describeAutoPredictorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAutoPredictor' smart constructor.
data DescribeAutoPredictor = DescribeAutoPredictor'
  { -- | The Amazon Resource Name (ARN) of the predictor.
    predictorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoPredictor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorArn', 'describeAutoPredictor_predictorArn' - The Amazon Resource Name (ARN) of the predictor.
newDescribeAutoPredictor ::
  -- | 'predictorArn'
  Prelude.Text ->
  DescribeAutoPredictor
newDescribeAutoPredictor pPredictorArn_ =
  DescribeAutoPredictor'
    { predictorArn =
        pPredictorArn_
    }

-- | The Amazon Resource Name (ARN) of the predictor.
describeAutoPredictor_predictorArn :: Lens.Lens' DescribeAutoPredictor Prelude.Text
describeAutoPredictor_predictorArn = Lens.lens (\DescribeAutoPredictor' {predictorArn} -> predictorArn) (\s@DescribeAutoPredictor' {} a -> s {predictorArn = a} :: DescribeAutoPredictor)

instance Core.AWSRequest DescribeAutoPredictor where
  type
    AWSResponse DescribeAutoPredictor =
      DescribeAutoPredictorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutoPredictorResponse'
            Prelude.<$> (x Core..?> "LastModificationTime")
            Prelude.<*> (x Core..?> "EncryptionConfig")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "ExplainabilityInfo")
            Prelude.<*> (x Core..?> "ForecastDimensions")
            Prelude.<*> (x Core..?> "OptimizationMetric")
            Prelude.<*> (x Core..?> "MonitorInfo")
            Prelude.<*> (x Core..?> "ForecastTypes")
            Prelude.<*> (x Core..?> "PredictorName")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Core..?> "ForecastHorizon")
            Prelude.<*> (x Core..?> "PredictorArn")
            Prelude.<*> ( x Core..?> "DatasetImportJobArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "DataConfig")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "TimeAlignmentBoundary")
            Prelude.<*> (x Core..?> "ReferencePredictorSummary")
            Prelude.<*> (x Core..?> "ForecastFrequency")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAutoPredictor where
  hashWithSalt _salt DescribeAutoPredictor' {..} =
    _salt `Prelude.hashWithSalt` predictorArn

instance Prelude.NFData DescribeAutoPredictor where
  rnf DescribeAutoPredictor' {..} =
    Prelude.rnf predictorArn

instance Core.ToHeaders DescribeAutoPredictor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DescribeAutoPredictor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAutoPredictor where
  toJSON DescribeAutoPredictor' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PredictorArn" Core..= predictorArn)]
      )

instance Core.ToPath DescribeAutoPredictor where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAutoPredictor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutoPredictorResponse' smart constructor.
data DescribeAutoPredictorResponse = DescribeAutoPredictorResponse'
  { -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | In the event of an error, a message detailing the cause of the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | Provides the status and ARN of the Predictor Explainability.
    explainabilityInfo :: Prelude.Maybe ExplainabilityInfo,
    -- | An array of dimension (field) names that specify the attributes used to
    -- group your time series.
    forecastDimensions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The accuracy metric used to optimize the predictor.
    optimizationMetric :: Prelude.Maybe OptimizationMetric,
    -- | A object with the Amazon Resource Name (ARN) and status of the monitor
    -- resource.
    monitorInfo :: Prelude.Maybe MonitorInfo,
    -- | The forecast types used during predictor training. Default value is
    -- [\"0.1\",\"0.5\",\"0.9\"].
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the predictor.
    predictorName :: Prelude.Maybe Prelude.Text,
    -- | The status of the predictor. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The estimated time remaining in minutes for the predictor training job
    -- to complete.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The number of time-steps that the model predicts. The forecast horizon
    -- is also called the prediction length.
    forecastHorizon :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the predictor
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | An array of the ARNs of the dataset import jobs used to import training
    -- data for the predictor.
    datasetImportJobArns :: Prelude.Maybe [Prelude.Text],
    -- | The data configuration for your dataset group and any additional
    -- datasets.
    dataConfig :: Prelude.Maybe DataConfig,
    -- | The timestamp of the CreateAutoPredictor request.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The time boundary Forecast uses when aggregating data.
    timeAlignmentBoundary :: Prelude.Maybe TimeAlignmentBoundary,
    -- | The ARN and state of the reference predictor. This parameter is only
    -- valid for retrained or upgraded predictors.
    referencePredictorSummary :: Prelude.Maybe ReferencePredictorSummary,
    -- | The frequency of predictions in a forecast.
    --
    -- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
    -- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
    -- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
    -- and \"5min\" indicates every five minutes.
    forecastFrequency :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoPredictorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeAutoPredictorResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'encryptionConfig', 'describeAutoPredictorResponse_encryptionConfig' - Undocumented member.
--
-- 'message', 'describeAutoPredictorResponse_message' - In the event of an error, a message detailing the cause of the error.
--
-- 'explainabilityInfo', 'describeAutoPredictorResponse_explainabilityInfo' - Provides the status and ARN of the Predictor Explainability.
--
-- 'forecastDimensions', 'describeAutoPredictorResponse_forecastDimensions' - An array of dimension (field) names that specify the attributes used to
-- group your time series.
--
-- 'optimizationMetric', 'describeAutoPredictorResponse_optimizationMetric' - The accuracy metric used to optimize the predictor.
--
-- 'monitorInfo', 'describeAutoPredictorResponse_monitorInfo' - A object with the Amazon Resource Name (ARN) and status of the monitor
-- resource.
--
-- 'forecastTypes', 'describeAutoPredictorResponse_forecastTypes' - The forecast types used during predictor training. Default value is
-- [\"0.1\",\"0.5\",\"0.9\"].
--
-- 'predictorName', 'describeAutoPredictorResponse_predictorName' - The name of the predictor.
--
-- 'status', 'describeAutoPredictorResponse_status' - The status of the predictor. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'estimatedTimeRemainingInMinutes', 'describeAutoPredictorResponse_estimatedTimeRemainingInMinutes' - The estimated time remaining in minutes for the predictor training job
-- to complete.
--
-- 'forecastHorizon', 'describeAutoPredictorResponse_forecastHorizon' - The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
--
-- 'predictorArn', 'describeAutoPredictorResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor
--
-- 'datasetImportJobArns', 'describeAutoPredictorResponse_datasetImportJobArns' - An array of the ARNs of the dataset import jobs used to import training
-- data for the predictor.
--
-- 'dataConfig', 'describeAutoPredictorResponse_dataConfig' - The data configuration for your dataset group and any additional
-- datasets.
--
-- 'creationTime', 'describeAutoPredictorResponse_creationTime' - The timestamp of the CreateAutoPredictor request.
--
-- 'timeAlignmentBoundary', 'describeAutoPredictorResponse_timeAlignmentBoundary' - The time boundary Forecast uses when aggregating data.
--
-- 'referencePredictorSummary', 'describeAutoPredictorResponse_referencePredictorSummary' - The ARN and state of the reference predictor. This parameter is only
-- valid for retrained or upgraded predictors.
--
-- 'forecastFrequency', 'describeAutoPredictorResponse_forecastFrequency' - The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
--
-- 'httpStatus', 'describeAutoPredictorResponse_httpStatus' - The response's http status code.
newDescribeAutoPredictorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAutoPredictorResponse
newDescribeAutoPredictorResponse pHttpStatus_ =
  DescribeAutoPredictorResponse'
    { lastModificationTime =
        Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      explainabilityInfo = Prelude.Nothing,
      forecastDimensions = Prelude.Nothing,
      optimizationMetric = Prelude.Nothing,
      monitorInfo = Prelude.Nothing,
      forecastTypes = Prelude.Nothing,
      predictorName = Prelude.Nothing,
      status = Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      forecastHorizon = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      datasetImportJobArns = Prelude.Nothing,
      dataConfig = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      timeAlignmentBoundary = Prelude.Nothing,
      referencePredictorSummary = Prelude.Nothing,
      forecastFrequency = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
describeAutoPredictorResponse_lastModificationTime :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.UTCTime)
describeAutoPredictorResponse_lastModificationTime = Lens.lens (\DescribeAutoPredictorResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeAutoPredictorResponse' {} a -> s {lastModificationTime = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeAutoPredictorResponse_encryptionConfig :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe EncryptionConfig)
describeAutoPredictorResponse_encryptionConfig = Lens.lens (\DescribeAutoPredictorResponse' {encryptionConfig} -> encryptionConfig) (\s@DescribeAutoPredictorResponse' {} a -> s {encryptionConfig = a} :: DescribeAutoPredictorResponse)

-- | In the event of an error, a message detailing the cause of the error.
describeAutoPredictorResponse_message :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_message = Lens.lens (\DescribeAutoPredictorResponse' {message} -> message) (\s@DescribeAutoPredictorResponse' {} a -> s {message = a} :: DescribeAutoPredictorResponse)

-- | Provides the status and ARN of the Predictor Explainability.
describeAutoPredictorResponse_explainabilityInfo :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe ExplainabilityInfo)
describeAutoPredictorResponse_explainabilityInfo = Lens.lens (\DescribeAutoPredictorResponse' {explainabilityInfo} -> explainabilityInfo) (\s@DescribeAutoPredictorResponse' {} a -> s {explainabilityInfo = a} :: DescribeAutoPredictorResponse)

-- | An array of dimension (field) names that specify the attributes used to
-- group your time series.
describeAutoPredictorResponse_forecastDimensions :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeAutoPredictorResponse_forecastDimensions = Lens.lens (\DescribeAutoPredictorResponse' {forecastDimensions} -> forecastDimensions) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastDimensions = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The accuracy metric used to optimize the predictor.
describeAutoPredictorResponse_optimizationMetric :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe OptimizationMetric)
describeAutoPredictorResponse_optimizationMetric = Lens.lens (\DescribeAutoPredictorResponse' {optimizationMetric} -> optimizationMetric) (\s@DescribeAutoPredictorResponse' {} a -> s {optimizationMetric = a} :: DescribeAutoPredictorResponse)

-- | A object with the Amazon Resource Name (ARN) and status of the monitor
-- resource.
describeAutoPredictorResponse_monitorInfo :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe MonitorInfo)
describeAutoPredictorResponse_monitorInfo = Lens.lens (\DescribeAutoPredictorResponse' {monitorInfo} -> monitorInfo) (\s@DescribeAutoPredictorResponse' {} a -> s {monitorInfo = a} :: DescribeAutoPredictorResponse)

-- | The forecast types used during predictor training. Default value is
-- [\"0.1\",\"0.5\",\"0.9\"].
describeAutoPredictorResponse_forecastTypes :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeAutoPredictorResponse_forecastTypes = Lens.lens (\DescribeAutoPredictorResponse' {forecastTypes} -> forecastTypes) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastTypes = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the predictor.
describeAutoPredictorResponse_predictorName :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_predictorName = Lens.lens (\DescribeAutoPredictorResponse' {predictorName} -> predictorName) (\s@DescribeAutoPredictorResponse' {} a -> s {predictorName = a} :: DescribeAutoPredictorResponse)

-- | The status of the predictor. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
describeAutoPredictorResponse_status :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_status = Lens.lens (\DescribeAutoPredictorResponse' {status} -> status) (\s@DescribeAutoPredictorResponse' {} a -> s {status = a} :: DescribeAutoPredictorResponse)

-- | The estimated time remaining in minutes for the predictor training job
-- to complete.
describeAutoPredictorResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Integer)
describeAutoPredictorResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeAutoPredictorResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeAutoPredictorResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeAutoPredictorResponse)

-- | The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
describeAutoPredictorResponse_forecastHorizon :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Int)
describeAutoPredictorResponse_forecastHorizon = Lens.lens (\DescribeAutoPredictorResponse' {forecastHorizon} -> forecastHorizon) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastHorizon = a} :: DescribeAutoPredictorResponse)

-- | The Amazon Resource Name (ARN) of the predictor
describeAutoPredictorResponse_predictorArn :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_predictorArn = Lens.lens (\DescribeAutoPredictorResponse' {predictorArn} -> predictorArn) (\s@DescribeAutoPredictorResponse' {} a -> s {predictorArn = a} :: DescribeAutoPredictorResponse)

-- | An array of the ARNs of the dataset import jobs used to import training
-- data for the predictor.
describeAutoPredictorResponse_datasetImportJobArns :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe [Prelude.Text])
describeAutoPredictorResponse_datasetImportJobArns = Lens.lens (\DescribeAutoPredictorResponse' {datasetImportJobArns} -> datasetImportJobArns) (\s@DescribeAutoPredictorResponse' {} a -> s {datasetImportJobArns = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The data configuration for your dataset group and any additional
-- datasets.
describeAutoPredictorResponse_dataConfig :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe DataConfig)
describeAutoPredictorResponse_dataConfig = Lens.lens (\DescribeAutoPredictorResponse' {dataConfig} -> dataConfig) (\s@DescribeAutoPredictorResponse' {} a -> s {dataConfig = a} :: DescribeAutoPredictorResponse)

-- | The timestamp of the CreateAutoPredictor request.
describeAutoPredictorResponse_creationTime :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.UTCTime)
describeAutoPredictorResponse_creationTime = Lens.lens (\DescribeAutoPredictorResponse' {creationTime} -> creationTime) (\s@DescribeAutoPredictorResponse' {} a -> s {creationTime = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Core._Time

-- | The time boundary Forecast uses when aggregating data.
describeAutoPredictorResponse_timeAlignmentBoundary :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe TimeAlignmentBoundary)
describeAutoPredictorResponse_timeAlignmentBoundary = Lens.lens (\DescribeAutoPredictorResponse' {timeAlignmentBoundary} -> timeAlignmentBoundary) (\s@DescribeAutoPredictorResponse' {} a -> s {timeAlignmentBoundary = a} :: DescribeAutoPredictorResponse)

-- | The ARN and state of the reference predictor. This parameter is only
-- valid for retrained or upgraded predictors.
describeAutoPredictorResponse_referencePredictorSummary :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe ReferencePredictorSummary)
describeAutoPredictorResponse_referencePredictorSummary = Lens.lens (\DescribeAutoPredictorResponse' {referencePredictorSummary} -> referencePredictorSummary) (\s@DescribeAutoPredictorResponse' {} a -> s {referencePredictorSummary = a} :: DescribeAutoPredictorResponse)

-- | The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
describeAutoPredictorResponse_forecastFrequency :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_forecastFrequency = Lens.lens (\DescribeAutoPredictorResponse' {forecastFrequency} -> forecastFrequency) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastFrequency = a} :: DescribeAutoPredictorResponse)

-- | The response's http status code.
describeAutoPredictorResponse_httpStatus :: Lens.Lens' DescribeAutoPredictorResponse Prelude.Int
describeAutoPredictorResponse_httpStatus = Lens.lens (\DescribeAutoPredictorResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoPredictorResponse' {} a -> s {httpStatus = a} :: DescribeAutoPredictorResponse)

instance Prelude.NFData DescribeAutoPredictorResponse where
  rnf DescribeAutoPredictorResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf explainabilityInfo
      `Prelude.seq` Prelude.rnf forecastDimensions
      `Prelude.seq` Prelude.rnf optimizationMetric
      `Prelude.seq` Prelude.rnf monitorInfo
      `Prelude.seq` Prelude.rnf forecastTypes
      `Prelude.seq` Prelude.rnf predictorName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf forecastHorizon
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf datasetImportJobArns
      `Prelude.seq` Prelude.rnf dataConfig
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf timeAlignmentBoundary
      `Prelude.seq` Prelude.rnf
        referencePredictorSummary
      `Prelude.seq` Prelude.rnf forecastFrequency
      `Prelude.seq` Prelude.rnf httpStatus

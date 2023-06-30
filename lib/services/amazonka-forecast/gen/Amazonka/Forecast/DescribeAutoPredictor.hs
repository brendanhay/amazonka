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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeAutoPredictorResponse_creationTime,
    describeAutoPredictorResponse_dataConfig,
    describeAutoPredictorResponse_datasetImportJobArns,
    describeAutoPredictorResponse_encryptionConfig,
    describeAutoPredictorResponse_estimatedTimeRemainingInMinutes,
    describeAutoPredictorResponse_explainabilityInfo,
    describeAutoPredictorResponse_forecastDimensions,
    describeAutoPredictorResponse_forecastFrequency,
    describeAutoPredictorResponse_forecastHorizon,
    describeAutoPredictorResponse_forecastTypes,
    describeAutoPredictorResponse_lastModificationTime,
    describeAutoPredictorResponse_message,
    describeAutoPredictorResponse_monitorInfo,
    describeAutoPredictorResponse_optimizationMetric,
    describeAutoPredictorResponse_predictorArn,
    describeAutoPredictorResponse_predictorName,
    describeAutoPredictorResponse_referencePredictorSummary,
    describeAutoPredictorResponse_status,
    describeAutoPredictorResponse_timeAlignmentBoundary,
    describeAutoPredictorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DataConfig")
            Prelude.<*> ( x
                            Data..?> "DatasetImportJobArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "EncryptionConfig")
            Prelude.<*> (x Data..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Data..?> "ExplainabilityInfo")
            Prelude.<*> (x Data..?> "ForecastDimensions")
            Prelude.<*> (x Data..?> "ForecastFrequency")
            Prelude.<*> (x Data..?> "ForecastHorizon")
            Prelude.<*> (x Data..?> "ForecastTypes")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "MonitorInfo")
            Prelude.<*> (x Data..?> "OptimizationMetric")
            Prelude.<*> (x Data..?> "PredictorArn")
            Prelude.<*> (x Data..?> "PredictorName")
            Prelude.<*> (x Data..?> "ReferencePredictorSummary")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TimeAlignmentBoundary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAutoPredictor where
  hashWithSalt _salt DescribeAutoPredictor' {..} =
    _salt `Prelude.hashWithSalt` predictorArn

instance Prelude.NFData DescribeAutoPredictor where
  rnf DescribeAutoPredictor' {..} =
    Prelude.rnf predictorArn

instance Data.ToHeaders DescribeAutoPredictor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeAutoPredictor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAutoPredictor where
  toJSON DescribeAutoPredictor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PredictorArn" Data..= predictorArn)]
      )

instance Data.ToPath DescribeAutoPredictor where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAutoPredictor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutoPredictorResponse' smart constructor.
data DescribeAutoPredictorResponse = DescribeAutoPredictorResponse'
  { -- | The timestamp of the CreateAutoPredictor request.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The data configuration for your dataset group and any additional
    -- datasets.
    dataConfig :: Prelude.Maybe DataConfig,
    -- | An array of the ARNs of the dataset import jobs used to import training
    -- data for the predictor.
    datasetImportJobArns :: Prelude.Maybe [Prelude.Text],
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | The estimated time remaining in minutes for the predictor training job
    -- to complete.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | Provides the status and ARN of the Predictor Explainability.
    explainabilityInfo :: Prelude.Maybe ExplainabilityInfo,
    -- | An array of dimension (field) names that specify the attributes used to
    -- group your time series.
    forecastDimensions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The frequency of predictions in a forecast.
    --
    -- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
    -- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
    -- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
    -- and \"5min\" indicates every five minutes.
    forecastFrequency :: Prelude.Maybe Prelude.Text,
    -- | The number of time-steps that the model predicts. The forecast horizon
    -- is also called the prediction length.
    forecastHorizon :: Prelude.Maybe Prelude.Int,
    -- | The forecast types used during predictor training. Default value is
    -- [\"0.1\",\"0.5\",\"0.9\"].
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | In the event of an error, a message detailing the cause of the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | A object with the Amazon Resource Name (ARN) and status of the monitor
    -- resource.
    monitorInfo :: Prelude.Maybe MonitorInfo,
    -- | The accuracy metric used to optimize the predictor.
    optimizationMetric :: Prelude.Maybe OptimizationMetric,
    -- | The Amazon Resource Name (ARN) of the predictor
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the predictor.
    predictorName :: Prelude.Maybe Prelude.Text,
    -- | The ARN and state of the reference predictor. This parameter is only
    -- valid for retrained or upgraded predictors.
    referencePredictorSummary :: Prelude.Maybe ReferencePredictorSummary,
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
    -- | The time boundary Forecast uses when aggregating data.
    timeAlignmentBoundary :: Prelude.Maybe TimeAlignmentBoundary,
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
-- 'creationTime', 'describeAutoPredictorResponse_creationTime' - The timestamp of the CreateAutoPredictor request.
--
-- 'dataConfig', 'describeAutoPredictorResponse_dataConfig' - The data configuration for your dataset group and any additional
-- datasets.
--
-- 'datasetImportJobArns', 'describeAutoPredictorResponse_datasetImportJobArns' - An array of the ARNs of the dataset import jobs used to import training
-- data for the predictor.
--
-- 'encryptionConfig', 'describeAutoPredictorResponse_encryptionConfig' - Undocumented member.
--
-- 'estimatedTimeRemainingInMinutes', 'describeAutoPredictorResponse_estimatedTimeRemainingInMinutes' - The estimated time remaining in minutes for the predictor training job
-- to complete.
--
-- 'explainabilityInfo', 'describeAutoPredictorResponse_explainabilityInfo' - Provides the status and ARN of the Predictor Explainability.
--
-- 'forecastDimensions', 'describeAutoPredictorResponse_forecastDimensions' - An array of dimension (field) names that specify the attributes used to
-- group your time series.
--
-- 'forecastFrequency', 'describeAutoPredictorResponse_forecastFrequency' - The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
--
-- 'forecastHorizon', 'describeAutoPredictorResponse_forecastHorizon' - The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
--
-- 'forecastTypes', 'describeAutoPredictorResponse_forecastTypes' - The forecast types used during predictor training. Default value is
-- [\"0.1\",\"0.5\",\"0.9\"].
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
-- 'message', 'describeAutoPredictorResponse_message' - In the event of an error, a message detailing the cause of the error.
--
-- 'monitorInfo', 'describeAutoPredictorResponse_monitorInfo' - A object with the Amazon Resource Name (ARN) and status of the monitor
-- resource.
--
-- 'optimizationMetric', 'describeAutoPredictorResponse_optimizationMetric' - The accuracy metric used to optimize the predictor.
--
-- 'predictorArn', 'describeAutoPredictorResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor
--
-- 'predictorName', 'describeAutoPredictorResponse_predictorName' - The name of the predictor.
--
-- 'referencePredictorSummary', 'describeAutoPredictorResponse_referencePredictorSummary' - The ARN and state of the reference predictor. This parameter is only
-- valid for retrained or upgraded predictors.
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
-- 'timeAlignmentBoundary', 'describeAutoPredictorResponse_timeAlignmentBoundary' - The time boundary Forecast uses when aggregating data.
--
-- 'httpStatus', 'describeAutoPredictorResponse_httpStatus' - The response's http status code.
newDescribeAutoPredictorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAutoPredictorResponse
newDescribeAutoPredictorResponse pHttpStatus_ =
  DescribeAutoPredictorResponse'
    { creationTime =
        Prelude.Nothing,
      dataConfig = Prelude.Nothing,
      datasetImportJobArns = Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      explainabilityInfo = Prelude.Nothing,
      forecastDimensions = Prelude.Nothing,
      forecastFrequency = Prelude.Nothing,
      forecastHorizon = Prelude.Nothing,
      forecastTypes = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      monitorInfo = Prelude.Nothing,
      optimizationMetric = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      predictorName = Prelude.Nothing,
      referencePredictorSummary = Prelude.Nothing,
      status = Prelude.Nothing,
      timeAlignmentBoundary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp of the CreateAutoPredictor request.
describeAutoPredictorResponse_creationTime :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.UTCTime)
describeAutoPredictorResponse_creationTime = Lens.lens (\DescribeAutoPredictorResponse' {creationTime} -> creationTime) (\s@DescribeAutoPredictorResponse' {} a -> s {creationTime = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Data._Time

-- | The data configuration for your dataset group and any additional
-- datasets.
describeAutoPredictorResponse_dataConfig :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe DataConfig)
describeAutoPredictorResponse_dataConfig = Lens.lens (\DescribeAutoPredictorResponse' {dataConfig} -> dataConfig) (\s@DescribeAutoPredictorResponse' {} a -> s {dataConfig = a} :: DescribeAutoPredictorResponse)

-- | An array of the ARNs of the dataset import jobs used to import training
-- data for the predictor.
describeAutoPredictorResponse_datasetImportJobArns :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe [Prelude.Text])
describeAutoPredictorResponse_datasetImportJobArns = Lens.lens (\DescribeAutoPredictorResponse' {datasetImportJobArns} -> datasetImportJobArns) (\s@DescribeAutoPredictorResponse' {} a -> s {datasetImportJobArns = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeAutoPredictorResponse_encryptionConfig :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe EncryptionConfig)
describeAutoPredictorResponse_encryptionConfig = Lens.lens (\DescribeAutoPredictorResponse' {encryptionConfig} -> encryptionConfig) (\s@DescribeAutoPredictorResponse' {} a -> s {encryptionConfig = a} :: DescribeAutoPredictorResponse)

-- | The estimated time remaining in minutes for the predictor training job
-- to complete.
describeAutoPredictorResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Integer)
describeAutoPredictorResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeAutoPredictorResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeAutoPredictorResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeAutoPredictorResponse)

-- | Provides the status and ARN of the Predictor Explainability.
describeAutoPredictorResponse_explainabilityInfo :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe ExplainabilityInfo)
describeAutoPredictorResponse_explainabilityInfo = Lens.lens (\DescribeAutoPredictorResponse' {explainabilityInfo} -> explainabilityInfo) (\s@DescribeAutoPredictorResponse' {} a -> s {explainabilityInfo = a} :: DescribeAutoPredictorResponse)

-- | An array of dimension (field) names that specify the attributes used to
-- group your time series.
describeAutoPredictorResponse_forecastDimensions :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeAutoPredictorResponse_forecastDimensions = Lens.lens (\DescribeAutoPredictorResponse' {forecastDimensions} -> forecastDimensions) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastDimensions = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
describeAutoPredictorResponse_forecastFrequency :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_forecastFrequency = Lens.lens (\DescribeAutoPredictorResponse' {forecastFrequency} -> forecastFrequency) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastFrequency = a} :: DescribeAutoPredictorResponse)

-- | The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
describeAutoPredictorResponse_forecastHorizon :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Int)
describeAutoPredictorResponse_forecastHorizon = Lens.lens (\DescribeAutoPredictorResponse' {forecastHorizon} -> forecastHorizon) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastHorizon = a} :: DescribeAutoPredictorResponse)

-- | The forecast types used during predictor training. Default value is
-- [\"0.1\",\"0.5\",\"0.9\"].
describeAutoPredictorResponse_forecastTypes :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeAutoPredictorResponse_forecastTypes = Lens.lens (\DescribeAutoPredictorResponse' {forecastTypes} -> forecastTypes) (\s@DescribeAutoPredictorResponse' {} a -> s {forecastTypes = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Lens.coerced

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
describeAutoPredictorResponse_lastModificationTime = Lens.lens (\DescribeAutoPredictorResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeAutoPredictorResponse' {} a -> s {lastModificationTime = a} :: DescribeAutoPredictorResponse) Prelude.. Lens.mapping Data._Time

-- | In the event of an error, a message detailing the cause of the error.
describeAutoPredictorResponse_message :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_message = Lens.lens (\DescribeAutoPredictorResponse' {message} -> message) (\s@DescribeAutoPredictorResponse' {} a -> s {message = a} :: DescribeAutoPredictorResponse)

-- | A object with the Amazon Resource Name (ARN) and status of the monitor
-- resource.
describeAutoPredictorResponse_monitorInfo :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe MonitorInfo)
describeAutoPredictorResponse_monitorInfo = Lens.lens (\DescribeAutoPredictorResponse' {monitorInfo} -> monitorInfo) (\s@DescribeAutoPredictorResponse' {} a -> s {monitorInfo = a} :: DescribeAutoPredictorResponse)

-- | The accuracy metric used to optimize the predictor.
describeAutoPredictorResponse_optimizationMetric :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe OptimizationMetric)
describeAutoPredictorResponse_optimizationMetric = Lens.lens (\DescribeAutoPredictorResponse' {optimizationMetric} -> optimizationMetric) (\s@DescribeAutoPredictorResponse' {} a -> s {optimizationMetric = a} :: DescribeAutoPredictorResponse)

-- | The Amazon Resource Name (ARN) of the predictor
describeAutoPredictorResponse_predictorArn :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_predictorArn = Lens.lens (\DescribeAutoPredictorResponse' {predictorArn} -> predictorArn) (\s@DescribeAutoPredictorResponse' {} a -> s {predictorArn = a} :: DescribeAutoPredictorResponse)

-- | The name of the predictor.
describeAutoPredictorResponse_predictorName :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe Prelude.Text)
describeAutoPredictorResponse_predictorName = Lens.lens (\DescribeAutoPredictorResponse' {predictorName} -> predictorName) (\s@DescribeAutoPredictorResponse' {} a -> s {predictorName = a} :: DescribeAutoPredictorResponse)

-- | The ARN and state of the reference predictor. This parameter is only
-- valid for retrained or upgraded predictors.
describeAutoPredictorResponse_referencePredictorSummary :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe ReferencePredictorSummary)
describeAutoPredictorResponse_referencePredictorSummary = Lens.lens (\DescribeAutoPredictorResponse' {referencePredictorSummary} -> referencePredictorSummary) (\s@DescribeAutoPredictorResponse' {} a -> s {referencePredictorSummary = a} :: DescribeAutoPredictorResponse)

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

-- | The time boundary Forecast uses when aggregating data.
describeAutoPredictorResponse_timeAlignmentBoundary :: Lens.Lens' DescribeAutoPredictorResponse (Prelude.Maybe TimeAlignmentBoundary)
describeAutoPredictorResponse_timeAlignmentBoundary = Lens.lens (\DescribeAutoPredictorResponse' {timeAlignmentBoundary} -> timeAlignmentBoundary) (\s@DescribeAutoPredictorResponse' {} a -> s {timeAlignmentBoundary = a} :: DescribeAutoPredictorResponse)

-- | The response's http status code.
describeAutoPredictorResponse_httpStatus :: Lens.Lens' DescribeAutoPredictorResponse Prelude.Int
describeAutoPredictorResponse_httpStatus = Lens.lens (\DescribeAutoPredictorResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoPredictorResponse' {} a -> s {httpStatus = a} :: DescribeAutoPredictorResponse)

instance Prelude.NFData DescribeAutoPredictorResponse where
  rnf DescribeAutoPredictorResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataConfig
      `Prelude.seq` Prelude.rnf datasetImportJobArns
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf explainabilityInfo
      `Prelude.seq` Prelude.rnf forecastDimensions
      `Prelude.seq` Prelude.rnf forecastFrequency
      `Prelude.seq` Prelude.rnf forecastHorizon
      `Prelude.seq` Prelude.rnf forecastTypes
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf monitorInfo
      `Prelude.seq` Prelude.rnf optimizationMetric
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf predictorName
      `Prelude.seq` Prelude.rnf
        referencePredictorSummary
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        timeAlignmentBoundary
      `Prelude.seq` Prelude.rnf httpStatus

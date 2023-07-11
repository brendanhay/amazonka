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
-- Module      : Amazonka.Rum.BatchCreateRumMetricDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the extended metrics that you want a CloudWatch RUM app
-- monitor to send to a destination. Valid destinations include CloudWatch
-- and Evidently.
--
-- By default, RUM app monitors send some metrics to CloudWatch. These
-- default metrics are listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-metrics.html CloudWatch metrics that you can collect with CloudWatch RUM>.
--
-- If you also send extended metrics, you can send metrics to Evidently as
-- well as CloudWatch, and you can also optionally send the metrics with
-- additional dimensions. The valid dimension names for the additional
-- dimensions are @BrowserName@, @CountryCode@, @DeviceType@, @FileType@,
-- @OSName@, and @PageId@. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-vended-metrics.html Extended metrics that you can send to CloudWatch and CloudWatch Evidently>.
--
-- The maximum number of metric definitions that you can specify in one
-- @BatchCreateRumMetricDefinitions@ operation is 200.
--
-- The maximum number of metric definitions that one destination can
-- contain is 2000.
--
-- Extended metrics sent are charged as CloudWatch custom metrics. Each
-- combination of additional dimension name and dimension value counts as a
-- custom metric. For more information, see
-- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- You must have already created a destination for the metrics before you
-- send them. For more information, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
--
-- If some metric definitions specified in a
-- @BatchCreateRumMetricDefinitions@ operations are not valid, those metric
-- definitions fail and return errors, but all valid metric definitions in
-- the same operation still succeed.
module Amazonka.Rum.BatchCreateRumMetricDefinitions
  ( -- * Creating a Request
    BatchCreateRumMetricDefinitions (..),
    newBatchCreateRumMetricDefinitions,

    -- * Request Lenses
    batchCreateRumMetricDefinitions_destinationArn,
    batchCreateRumMetricDefinitions_appMonitorName,
    batchCreateRumMetricDefinitions_destination,
    batchCreateRumMetricDefinitions_metricDefinitions,

    -- * Destructuring the Response
    BatchCreateRumMetricDefinitionsResponse (..),
    newBatchCreateRumMetricDefinitionsResponse,

    -- * Response Lenses
    batchCreateRumMetricDefinitionsResponse_metricDefinitions,
    batchCreateRumMetricDefinitionsResponse_httpStatus,
    batchCreateRumMetricDefinitionsResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newBatchCreateRumMetricDefinitions' smart constructor.
data BatchCreateRumMetricDefinitions = BatchCreateRumMetricDefinitions'
  { -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter.
    --
    -- This parameter specifies the ARN of the Evidently experiment that is to
    -- receive the metrics. You must have already defined this experiment as a
    -- valid destination. For more information, see
    -- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch RUM app monitor that is to send the metrics.
    appMonitorName :: Prelude.Text,
    -- | The destination to send the metrics to. Valid values are @CloudWatch@
    -- and @Evidently@. If you specify @Evidently@, you must also specify the
    -- ARN of the CloudWatchEvidently experiment that will receive the metrics
    -- and an IAM role that has permission to write to the experiment.
    destination :: MetricDestination,
    -- | An array of structures which define the metrics that you want to send.
    metricDefinitions :: [MetricDefinitionRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateRumMetricDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'batchCreateRumMetricDefinitions_destinationArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that is to
-- receive the metrics. You must have already defined this experiment as a
-- valid destination. For more information, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
--
-- 'appMonitorName', 'batchCreateRumMetricDefinitions_appMonitorName' - The name of the CloudWatch RUM app monitor that is to send the metrics.
--
-- 'destination', 'batchCreateRumMetricDefinitions_destination' - The destination to send the metrics to. Valid values are @CloudWatch@
-- and @Evidently@. If you specify @Evidently@, you must also specify the
-- ARN of the CloudWatchEvidently experiment that will receive the metrics
-- and an IAM role that has permission to write to the experiment.
--
-- 'metricDefinitions', 'batchCreateRumMetricDefinitions_metricDefinitions' - An array of structures which define the metrics that you want to send.
newBatchCreateRumMetricDefinitions ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  BatchCreateRumMetricDefinitions
newBatchCreateRumMetricDefinitions
  pAppMonitorName_
  pDestination_ =
    BatchCreateRumMetricDefinitions'
      { destinationArn =
          Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_,
        metricDefinitions = Prelude.mempty
      }

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that is to
-- receive the metrics. You must have already defined this experiment as a
-- valid destination. For more information, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
batchCreateRumMetricDefinitions_destinationArn :: Lens.Lens' BatchCreateRumMetricDefinitions (Prelude.Maybe Prelude.Text)
batchCreateRumMetricDefinitions_destinationArn = Lens.lens (\BatchCreateRumMetricDefinitions' {destinationArn} -> destinationArn) (\s@BatchCreateRumMetricDefinitions' {} a -> s {destinationArn = a} :: BatchCreateRumMetricDefinitions)

-- | The name of the CloudWatch RUM app monitor that is to send the metrics.
batchCreateRumMetricDefinitions_appMonitorName :: Lens.Lens' BatchCreateRumMetricDefinitions Prelude.Text
batchCreateRumMetricDefinitions_appMonitorName = Lens.lens (\BatchCreateRumMetricDefinitions' {appMonitorName} -> appMonitorName) (\s@BatchCreateRumMetricDefinitions' {} a -> s {appMonitorName = a} :: BatchCreateRumMetricDefinitions)

-- | The destination to send the metrics to. Valid values are @CloudWatch@
-- and @Evidently@. If you specify @Evidently@, you must also specify the
-- ARN of the CloudWatchEvidently experiment that will receive the metrics
-- and an IAM role that has permission to write to the experiment.
batchCreateRumMetricDefinitions_destination :: Lens.Lens' BatchCreateRumMetricDefinitions MetricDestination
batchCreateRumMetricDefinitions_destination = Lens.lens (\BatchCreateRumMetricDefinitions' {destination} -> destination) (\s@BatchCreateRumMetricDefinitions' {} a -> s {destination = a} :: BatchCreateRumMetricDefinitions)

-- | An array of structures which define the metrics that you want to send.
batchCreateRumMetricDefinitions_metricDefinitions :: Lens.Lens' BatchCreateRumMetricDefinitions [MetricDefinitionRequest]
batchCreateRumMetricDefinitions_metricDefinitions = Lens.lens (\BatchCreateRumMetricDefinitions' {metricDefinitions} -> metricDefinitions) (\s@BatchCreateRumMetricDefinitions' {} a -> s {metricDefinitions = a} :: BatchCreateRumMetricDefinitions) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchCreateRumMetricDefinitions
  where
  type
    AWSResponse BatchCreateRumMetricDefinitions =
      BatchCreateRumMetricDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateRumMetricDefinitionsResponse'
            Prelude.<$> ( x
                            Data..?> "MetricDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchCreateRumMetricDefinitions
  where
  hashWithSalt
    _salt
    BatchCreateRumMetricDefinitions' {..} =
      _salt
        `Prelude.hashWithSalt` destinationArn
        `Prelude.hashWithSalt` appMonitorName
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` metricDefinitions

instance
  Prelude.NFData
    BatchCreateRumMetricDefinitions
  where
  rnf BatchCreateRumMetricDefinitions' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf appMonitorName
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf metricDefinitions

instance
  Data.ToHeaders
    BatchCreateRumMetricDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCreateRumMetricDefinitions where
  toJSON BatchCreateRumMetricDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationArn" Data..=)
              Prelude.<$> destinationArn,
            Prelude.Just ("Destination" Data..= destination),
            Prelude.Just
              ("MetricDefinitions" Data..= metricDefinitions)
          ]
      )

instance Data.ToPath BatchCreateRumMetricDefinitions where
  toPath BatchCreateRumMetricDefinitions' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metrics"
      ]

instance Data.ToQuery BatchCreateRumMetricDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateRumMetricDefinitionsResponse' smart constructor.
data BatchCreateRumMetricDefinitionsResponse = BatchCreateRumMetricDefinitionsResponse'
  { -- | An array of structures that define the extended metrics.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of error objects, if the operation caused any errors.
    errors :: [BatchCreateRumMetricDefinitionsError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateRumMetricDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDefinitions', 'batchCreateRumMetricDefinitionsResponse_metricDefinitions' - An array of structures that define the extended metrics.
--
-- 'httpStatus', 'batchCreateRumMetricDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchCreateRumMetricDefinitionsResponse_errors' - An array of error objects, if the operation caused any errors.
newBatchCreateRumMetricDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateRumMetricDefinitionsResponse
newBatchCreateRumMetricDefinitionsResponse
  pHttpStatus_ =
    BatchCreateRumMetricDefinitionsResponse'
      { metricDefinitions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errors = Prelude.mempty
      }

-- | An array of structures that define the extended metrics.
batchCreateRumMetricDefinitionsResponse_metricDefinitions :: Lens.Lens' BatchCreateRumMetricDefinitionsResponse (Prelude.Maybe [MetricDefinition])
batchCreateRumMetricDefinitionsResponse_metricDefinitions = Lens.lens (\BatchCreateRumMetricDefinitionsResponse' {metricDefinitions} -> metricDefinitions) (\s@BatchCreateRumMetricDefinitionsResponse' {} a -> s {metricDefinitions = a} :: BatchCreateRumMetricDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateRumMetricDefinitionsResponse_httpStatus :: Lens.Lens' BatchCreateRumMetricDefinitionsResponse Prelude.Int
batchCreateRumMetricDefinitionsResponse_httpStatus = Lens.lens (\BatchCreateRumMetricDefinitionsResponse' {httpStatus} -> httpStatus) (\s@BatchCreateRumMetricDefinitionsResponse' {} a -> s {httpStatus = a} :: BatchCreateRumMetricDefinitionsResponse)

-- | An array of error objects, if the operation caused any errors.
batchCreateRumMetricDefinitionsResponse_errors :: Lens.Lens' BatchCreateRumMetricDefinitionsResponse [BatchCreateRumMetricDefinitionsError]
batchCreateRumMetricDefinitionsResponse_errors = Lens.lens (\BatchCreateRumMetricDefinitionsResponse' {errors} -> errors) (\s@BatchCreateRumMetricDefinitionsResponse' {} a -> s {errors = a} :: BatchCreateRumMetricDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchCreateRumMetricDefinitionsResponse
  where
  rnf BatchCreateRumMetricDefinitionsResponse' {..} =
    Prelude.rnf metricDefinitions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors

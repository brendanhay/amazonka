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
-- Module      : Amazonka.Rum.BatchDeleteRumMetricDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified metrics from being sent to an extended metrics
-- destination.
--
-- If some metric definition IDs specified in a
-- @BatchDeleteRumMetricDefinitions@ operations are not valid, those metric
-- definitions fail and return errors, but all valid metric definition IDs
-- in the same operation are still deleted.
--
-- The maximum number of metric definitions that you can specify in one
-- @BatchDeleteRumMetricDefinitions@ operation is 200.
module Amazonka.Rum.BatchDeleteRumMetricDefinitions
  ( -- * Creating a Request
    BatchDeleteRumMetricDefinitions (..),
    newBatchDeleteRumMetricDefinitions,

    -- * Request Lenses
    batchDeleteRumMetricDefinitions_destinationArn,
    batchDeleteRumMetricDefinitions_appMonitorName,
    batchDeleteRumMetricDefinitions_destination,
    batchDeleteRumMetricDefinitions_metricDefinitionIds,

    -- * Destructuring the Response
    BatchDeleteRumMetricDefinitionsResponse (..),
    newBatchDeleteRumMetricDefinitionsResponse,

    -- * Response Lenses
    batchDeleteRumMetricDefinitionsResponse_metricDefinitionIds,
    batchDeleteRumMetricDefinitionsResponse_httpStatus,
    batchDeleteRumMetricDefinitionsResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newBatchDeleteRumMetricDefinitions' smart constructor.
data BatchDeleteRumMetricDefinitions = BatchDeleteRumMetricDefinitions'
  { -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter.
    --
    -- This parameter specifies the ARN of the Evidently experiment that was
    -- receiving the metrics that are being deleted.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch RUM app monitor that is sending these
    -- metrics.
    appMonitorName :: Prelude.Text,
    -- | Defines the destination where you want to stop sending the specified
    -- metrics. Valid values are @CloudWatch@ and @Evidently@. If you specify
    -- @Evidently@, you must also specify the ARN of the CloudWatchEvidently
    -- experiment that is to be the destination and an IAM role that has
    -- permission to write to the experiment.
    destination :: MetricDestination,
    -- | An array of structures which define the metrics that you want to stop
    -- sending.
    metricDefinitionIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteRumMetricDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'batchDeleteRumMetricDefinitions_destinationArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that was
-- receiving the metrics that are being deleted.
--
-- 'appMonitorName', 'batchDeleteRumMetricDefinitions_appMonitorName' - The name of the CloudWatch RUM app monitor that is sending these
-- metrics.
--
-- 'destination', 'batchDeleteRumMetricDefinitions_destination' - Defines the destination where you want to stop sending the specified
-- metrics. Valid values are @CloudWatch@ and @Evidently@. If you specify
-- @Evidently@, you must also specify the ARN of the CloudWatchEvidently
-- experiment that is to be the destination and an IAM role that has
-- permission to write to the experiment.
--
-- 'metricDefinitionIds', 'batchDeleteRumMetricDefinitions_metricDefinitionIds' - An array of structures which define the metrics that you want to stop
-- sending.
newBatchDeleteRumMetricDefinitions ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  BatchDeleteRumMetricDefinitions
newBatchDeleteRumMetricDefinitions
  pAppMonitorName_
  pDestination_ =
    BatchDeleteRumMetricDefinitions'
      { destinationArn =
          Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_,
        metricDefinitionIds = Prelude.mempty
      }

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that was
-- receiving the metrics that are being deleted.
batchDeleteRumMetricDefinitions_destinationArn :: Lens.Lens' BatchDeleteRumMetricDefinitions (Prelude.Maybe Prelude.Text)
batchDeleteRumMetricDefinitions_destinationArn = Lens.lens (\BatchDeleteRumMetricDefinitions' {destinationArn} -> destinationArn) (\s@BatchDeleteRumMetricDefinitions' {} a -> s {destinationArn = a} :: BatchDeleteRumMetricDefinitions)

-- | The name of the CloudWatch RUM app monitor that is sending these
-- metrics.
batchDeleteRumMetricDefinitions_appMonitorName :: Lens.Lens' BatchDeleteRumMetricDefinitions Prelude.Text
batchDeleteRumMetricDefinitions_appMonitorName = Lens.lens (\BatchDeleteRumMetricDefinitions' {appMonitorName} -> appMonitorName) (\s@BatchDeleteRumMetricDefinitions' {} a -> s {appMonitorName = a} :: BatchDeleteRumMetricDefinitions)

-- | Defines the destination where you want to stop sending the specified
-- metrics. Valid values are @CloudWatch@ and @Evidently@. If you specify
-- @Evidently@, you must also specify the ARN of the CloudWatchEvidently
-- experiment that is to be the destination and an IAM role that has
-- permission to write to the experiment.
batchDeleteRumMetricDefinitions_destination :: Lens.Lens' BatchDeleteRumMetricDefinitions MetricDestination
batchDeleteRumMetricDefinitions_destination = Lens.lens (\BatchDeleteRumMetricDefinitions' {destination} -> destination) (\s@BatchDeleteRumMetricDefinitions' {} a -> s {destination = a} :: BatchDeleteRumMetricDefinitions)

-- | An array of structures which define the metrics that you want to stop
-- sending.
batchDeleteRumMetricDefinitions_metricDefinitionIds :: Lens.Lens' BatchDeleteRumMetricDefinitions [Prelude.Text]
batchDeleteRumMetricDefinitions_metricDefinitionIds = Lens.lens (\BatchDeleteRumMetricDefinitions' {metricDefinitionIds} -> metricDefinitionIds) (\s@BatchDeleteRumMetricDefinitions' {} a -> s {metricDefinitionIds = a} :: BatchDeleteRumMetricDefinitions) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDeleteRumMetricDefinitions
  where
  type
    AWSResponse BatchDeleteRumMetricDefinitions =
      BatchDeleteRumMetricDefinitionsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteRumMetricDefinitionsResponse'
            Prelude.<$> ( x
                            Data..?> "MetricDefinitionIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDeleteRumMetricDefinitions
  where
  hashWithSalt
    _salt
    BatchDeleteRumMetricDefinitions' {..} =
      _salt
        `Prelude.hashWithSalt` destinationArn
        `Prelude.hashWithSalt` appMonitorName
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` metricDefinitionIds

instance
  Prelude.NFData
    BatchDeleteRumMetricDefinitions
  where
  rnf BatchDeleteRumMetricDefinitions' {..} =
    Prelude.rnf destinationArn `Prelude.seq`
      Prelude.rnf appMonitorName `Prelude.seq`
        Prelude.rnf destination `Prelude.seq`
          Prelude.rnf metricDefinitionIds

instance
  Data.ToHeaders
    BatchDeleteRumMetricDefinitions
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

instance Data.ToPath BatchDeleteRumMetricDefinitions where
  toPath BatchDeleteRumMetricDefinitions' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metrics"
      ]

instance Data.ToQuery BatchDeleteRumMetricDefinitions where
  toQuery BatchDeleteRumMetricDefinitions' {..} =
    Prelude.mconcat
      [ "destinationArn" Data.=: destinationArn,
        "destination" Data.=: destination,
        "metricDefinitionIds"
          Data.=: Data.toQueryList "member" metricDefinitionIds
      ]

-- | /See:/ 'newBatchDeleteRumMetricDefinitionsResponse' smart constructor.
data BatchDeleteRumMetricDefinitionsResponse = BatchDeleteRumMetricDefinitionsResponse'
  { -- | The IDs of the metric definitions that were deleted.
    metricDefinitionIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of error objects, if the operation caused any errors.
    errors :: [BatchDeleteRumMetricDefinitionsError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteRumMetricDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDefinitionIds', 'batchDeleteRumMetricDefinitionsResponse_metricDefinitionIds' - The IDs of the metric definitions that were deleted.
--
-- 'httpStatus', 'batchDeleteRumMetricDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchDeleteRumMetricDefinitionsResponse_errors' - An array of error objects, if the operation caused any errors.
newBatchDeleteRumMetricDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteRumMetricDefinitionsResponse
newBatchDeleteRumMetricDefinitionsResponse
  pHttpStatus_ =
    BatchDeleteRumMetricDefinitionsResponse'
      { metricDefinitionIds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errors = Prelude.mempty
      }

-- | The IDs of the metric definitions that were deleted.
batchDeleteRumMetricDefinitionsResponse_metricDefinitionIds :: Lens.Lens' BatchDeleteRumMetricDefinitionsResponse (Prelude.Maybe [Prelude.Text])
batchDeleteRumMetricDefinitionsResponse_metricDefinitionIds = Lens.lens (\BatchDeleteRumMetricDefinitionsResponse' {metricDefinitionIds} -> metricDefinitionIds) (\s@BatchDeleteRumMetricDefinitionsResponse' {} a -> s {metricDefinitionIds = a} :: BatchDeleteRumMetricDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteRumMetricDefinitionsResponse_httpStatus :: Lens.Lens' BatchDeleteRumMetricDefinitionsResponse Prelude.Int
batchDeleteRumMetricDefinitionsResponse_httpStatus = Lens.lens (\BatchDeleteRumMetricDefinitionsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteRumMetricDefinitionsResponse' {} a -> s {httpStatus = a} :: BatchDeleteRumMetricDefinitionsResponse)

-- | An array of error objects, if the operation caused any errors.
batchDeleteRumMetricDefinitionsResponse_errors :: Lens.Lens' BatchDeleteRumMetricDefinitionsResponse [BatchDeleteRumMetricDefinitionsError]
batchDeleteRumMetricDefinitionsResponse_errors = Lens.lens (\BatchDeleteRumMetricDefinitionsResponse' {errors} -> errors) (\s@BatchDeleteRumMetricDefinitionsResponse' {} a -> s {errors = a} :: BatchDeleteRumMetricDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchDeleteRumMetricDefinitionsResponse
  where
  rnf BatchDeleteRumMetricDefinitionsResponse' {..} =
    Prelude.rnf metricDefinitionIds `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf errors

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
-- Module      : Amazonka.Rum.UpdateRumMetricDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies one existing metric definition for CloudWatch RUM extended
-- metrics. For more information about extended metrics, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_BatchCreateRumMetricsDefinitions.html BatchCreateRumMetricsDefinitions>.
module Amazonka.Rum.UpdateRumMetricDefinition
  ( -- * Creating a Request
    UpdateRumMetricDefinition (..),
    newUpdateRumMetricDefinition,

    -- * Request Lenses
    updateRumMetricDefinition_destinationArn,
    updateRumMetricDefinition_appMonitorName,
    updateRumMetricDefinition_destination,
    updateRumMetricDefinition_metricDefinition,
    updateRumMetricDefinition_metricDefinitionId,

    -- * Destructuring the Response
    UpdateRumMetricDefinitionResponse (..),
    newUpdateRumMetricDefinitionResponse,

    -- * Response Lenses
    updateRumMetricDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newUpdateRumMetricDefinition' smart constructor.
data UpdateRumMetricDefinition = UpdateRumMetricDefinition'
  { -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter.
    --
    -- This parameter specifies the ARN of the Evidently experiment that is to
    -- receive the metrics. You must have already defined this experiment as a
    -- valid destination. For more information, see
    -- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch RUM app monitor that sends these metrics.
    appMonitorName :: Prelude.Text,
    -- | The destination to send the metrics to. Valid values are @CloudWatch@
    -- and @Evidently@. If you specify @Evidently@, you must also specify the
    -- ARN of the CloudWatchEvidently experiment that will receive the metrics
    -- and an IAM role that has permission to write to the experiment.
    destination :: MetricDestination,
    -- | A structure that contains the new definition that you want to use for
    -- this metric.
    metricDefinition :: MetricDefinitionRequest,
    -- | The ID of the metric definition to update.
    metricDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRumMetricDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'updateRumMetricDefinition_destinationArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that is to
-- receive the metrics. You must have already defined this experiment as a
-- valid destination. For more information, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
--
-- 'appMonitorName', 'updateRumMetricDefinition_appMonitorName' - The name of the CloudWatch RUM app monitor that sends these metrics.
--
-- 'destination', 'updateRumMetricDefinition_destination' - The destination to send the metrics to. Valid values are @CloudWatch@
-- and @Evidently@. If you specify @Evidently@, you must also specify the
-- ARN of the CloudWatchEvidently experiment that will receive the metrics
-- and an IAM role that has permission to write to the experiment.
--
-- 'metricDefinition', 'updateRumMetricDefinition_metricDefinition' - A structure that contains the new definition that you want to use for
-- this metric.
--
-- 'metricDefinitionId', 'updateRumMetricDefinition_metricDefinitionId' - The ID of the metric definition to update.
newUpdateRumMetricDefinition ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  -- | 'metricDefinition'
  MetricDefinitionRequest ->
  -- | 'metricDefinitionId'
  Prelude.Text ->
  UpdateRumMetricDefinition
newUpdateRumMetricDefinition
  pAppMonitorName_
  pDestination_
  pMetricDefinition_
  pMetricDefinitionId_ =
    UpdateRumMetricDefinition'
      { destinationArn =
          Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_,
        metricDefinition = pMetricDefinition_,
        metricDefinitionId = pMetricDefinitionId_
      }

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that is to
-- receive the metrics. You must have already defined this experiment as a
-- valid destination. For more information, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_PutRumMetricsDestination.html PutRumMetricsDestination>.
updateRumMetricDefinition_destinationArn :: Lens.Lens' UpdateRumMetricDefinition (Prelude.Maybe Prelude.Text)
updateRumMetricDefinition_destinationArn = Lens.lens (\UpdateRumMetricDefinition' {destinationArn} -> destinationArn) (\s@UpdateRumMetricDefinition' {} a -> s {destinationArn = a} :: UpdateRumMetricDefinition)

-- | The name of the CloudWatch RUM app monitor that sends these metrics.
updateRumMetricDefinition_appMonitorName :: Lens.Lens' UpdateRumMetricDefinition Prelude.Text
updateRumMetricDefinition_appMonitorName = Lens.lens (\UpdateRumMetricDefinition' {appMonitorName} -> appMonitorName) (\s@UpdateRumMetricDefinition' {} a -> s {appMonitorName = a} :: UpdateRumMetricDefinition)

-- | The destination to send the metrics to. Valid values are @CloudWatch@
-- and @Evidently@. If you specify @Evidently@, you must also specify the
-- ARN of the CloudWatchEvidently experiment that will receive the metrics
-- and an IAM role that has permission to write to the experiment.
updateRumMetricDefinition_destination :: Lens.Lens' UpdateRumMetricDefinition MetricDestination
updateRumMetricDefinition_destination = Lens.lens (\UpdateRumMetricDefinition' {destination} -> destination) (\s@UpdateRumMetricDefinition' {} a -> s {destination = a} :: UpdateRumMetricDefinition)

-- | A structure that contains the new definition that you want to use for
-- this metric.
updateRumMetricDefinition_metricDefinition :: Lens.Lens' UpdateRumMetricDefinition MetricDefinitionRequest
updateRumMetricDefinition_metricDefinition = Lens.lens (\UpdateRumMetricDefinition' {metricDefinition} -> metricDefinition) (\s@UpdateRumMetricDefinition' {} a -> s {metricDefinition = a} :: UpdateRumMetricDefinition)

-- | The ID of the metric definition to update.
updateRumMetricDefinition_metricDefinitionId :: Lens.Lens' UpdateRumMetricDefinition Prelude.Text
updateRumMetricDefinition_metricDefinitionId = Lens.lens (\UpdateRumMetricDefinition' {metricDefinitionId} -> metricDefinitionId) (\s@UpdateRumMetricDefinition' {} a -> s {metricDefinitionId = a} :: UpdateRumMetricDefinition)

instance Core.AWSRequest UpdateRumMetricDefinition where
  type
    AWSResponse UpdateRumMetricDefinition =
      UpdateRumMetricDefinitionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRumMetricDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRumMetricDefinition where
  hashWithSalt _salt UpdateRumMetricDefinition' {..} =
    _salt `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` appMonitorName
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` metricDefinition
      `Prelude.hashWithSalt` metricDefinitionId

instance Prelude.NFData UpdateRumMetricDefinition where
  rnf UpdateRumMetricDefinition' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf appMonitorName
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf metricDefinition
      `Prelude.seq` Prelude.rnf metricDefinitionId

instance Data.ToHeaders UpdateRumMetricDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRumMetricDefinition where
  toJSON UpdateRumMetricDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationArn" Data..=)
              Prelude.<$> destinationArn,
            Prelude.Just ("Destination" Data..= destination),
            Prelude.Just
              ("MetricDefinition" Data..= metricDefinition),
            Prelude.Just
              ("MetricDefinitionId" Data..= metricDefinitionId)
          ]
      )

instance Data.ToPath UpdateRumMetricDefinition where
  toPath UpdateRumMetricDefinition' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metrics"
      ]

instance Data.ToQuery UpdateRumMetricDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRumMetricDefinitionResponse' smart constructor.
data UpdateRumMetricDefinitionResponse = UpdateRumMetricDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRumMetricDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRumMetricDefinitionResponse_httpStatus' - The response's http status code.
newUpdateRumMetricDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRumMetricDefinitionResponse
newUpdateRumMetricDefinitionResponse pHttpStatus_ =
  UpdateRumMetricDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRumMetricDefinitionResponse_httpStatus :: Lens.Lens' UpdateRumMetricDefinitionResponse Prelude.Int
updateRumMetricDefinitionResponse_httpStatus = Lens.lens (\UpdateRumMetricDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateRumMetricDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateRumMetricDefinitionResponse)

instance
  Prelude.NFData
    UpdateRumMetricDefinitionResponse
  where
  rnf UpdateRumMetricDefinitionResponse' {..} =
    Prelude.rnf httpStatus

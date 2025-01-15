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
-- Module      : Amazonka.Rum.BatchGetRumMetricDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of metrics and dimensions that a RUM app monitor is
-- sending to a single destination.
--
-- This operation returns paginated results.
module Amazonka.Rum.BatchGetRumMetricDefinitions
  ( -- * Creating a Request
    BatchGetRumMetricDefinitions (..),
    newBatchGetRumMetricDefinitions,

    -- * Request Lenses
    batchGetRumMetricDefinitions_destinationArn,
    batchGetRumMetricDefinitions_maxResults,
    batchGetRumMetricDefinitions_nextToken,
    batchGetRumMetricDefinitions_appMonitorName,
    batchGetRumMetricDefinitions_destination,

    -- * Destructuring the Response
    BatchGetRumMetricDefinitionsResponse (..),
    newBatchGetRumMetricDefinitionsResponse,

    -- * Response Lenses
    batchGetRumMetricDefinitionsResponse_metricDefinitions,
    batchGetRumMetricDefinitionsResponse_nextToken,
    batchGetRumMetricDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newBatchGetRumMetricDefinitions' smart constructor.
data BatchGetRumMetricDefinitions = BatchGetRumMetricDefinitions'
  { -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter.
    --
    -- This parameter specifies the ARN of the Evidently experiment that
    -- corresponds to the destination.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in one operation. The default is
    -- 50. The maximum that you can specify is 100.
    --
    -- To retrieve the remaining results, make another call with the returned
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use the token returned by the previous operation to request the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch RUM app monitor that is sending the metrics.
    appMonitorName :: Prelude.Text,
    -- | The type of destination that you want to view metrics for. Valid values
    -- are @CloudWatch@ and @Evidently@.
    destination :: MetricDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRumMetricDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'batchGetRumMetricDefinitions_destinationArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that
-- corresponds to the destination.
--
-- 'maxResults', 'batchGetRumMetricDefinitions_maxResults' - The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
--
-- 'nextToken', 'batchGetRumMetricDefinitions_nextToken' - Use the token returned by the previous operation to request the next
-- page of results.
--
-- 'appMonitorName', 'batchGetRumMetricDefinitions_appMonitorName' - The name of the CloudWatch RUM app monitor that is sending the metrics.
--
-- 'destination', 'batchGetRumMetricDefinitions_destination' - The type of destination that you want to view metrics for. Valid values
-- are @CloudWatch@ and @Evidently@.
newBatchGetRumMetricDefinitions ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  BatchGetRumMetricDefinitions
newBatchGetRumMetricDefinitions
  pAppMonitorName_
  pDestination_ =
    BatchGetRumMetricDefinitions'
      { destinationArn =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_
      }

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of the Evidently experiment that
-- corresponds to the destination.
batchGetRumMetricDefinitions_destinationArn :: Lens.Lens' BatchGetRumMetricDefinitions (Prelude.Maybe Prelude.Text)
batchGetRumMetricDefinitions_destinationArn = Lens.lens (\BatchGetRumMetricDefinitions' {destinationArn} -> destinationArn) (\s@BatchGetRumMetricDefinitions' {} a -> s {destinationArn = a} :: BatchGetRumMetricDefinitions)

-- | The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
batchGetRumMetricDefinitions_maxResults :: Lens.Lens' BatchGetRumMetricDefinitions (Prelude.Maybe Prelude.Natural)
batchGetRumMetricDefinitions_maxResults = Lens.lens (\BatchGetRumMetricDefinitions' {maxResults} -> maxResults) (\s@BatchGetRumMetricDefinitions' {} a -> s {maxResults = a} :: BatchGetRumMetricDefinitions)

-- | Use the token returned by the previous operation to request the next
-- page of results.
batchGetRumMetricDefinitions_nextToken :: Lens.Lens' BatchGetRumMetricDefinitions (Prelude.Maybe Prelude.Text)
batchGetRumMetricDefinitions_nextToken = Lens.lens (\BatchGetRumMetricDefinitions' {nextToken} -> nextToken) (\s@BatchGetRumMetricDefinitions' {} a -> s {nextToken = a} :: BatchGetRumMetricDefinitions)

-- | The name of the CloudWatch RUM app monitor that is sending the metrics.
batchGetRumMetricDefinitions_appMonitorName :: Lens.Lens' BatchGetRumMetricDefinitions Prelude.Text
batchGetRumMetricDefinitions_appMonitorName = Lens.lens (\BatchGetRumMetricDefinitions' {appMonitorName} -> appMonitorName) (\s@BatchGetRumMetricDefinitions' {} a -> s {appMonitorName = a} :: BatchGetRumMetricDefinitions)

-- | The type of destination that you want to view metrics for. Valid values
-- are @CloudWatch@ and @Evidently@.
batchGetRumMetricDefinitions_destination :: Lens.Lens' BatchGetRumMetricDefinitions MetricDestination
batchGetRumMetricDefinitions_destination = Lens.lens (\BatchGetRumMetricDefinitions' {destination} -> destination) (\s@BatchGetRumMetricDefinitions' {} a -> s {destination = a} :: BatchGetRumMetricDefinitions)

instance Core.AWSPager BatchGetRumMetricDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? batchGetRumMetricDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? batchGetRumMetricDefinitionsResponse_metricDefinitions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& batchGetRumMetricDefinitions_nextToken
              Lens..~ rs
              Lens.^? batchGetRumMetricDefinitionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest BatchGetRumMetricDefinitions where
  type
    AWSResponse BatchGetRumMetricDefinitions =
      BatchGetRumMetricDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetRumMetricDefinitionsResponse'
            Prelude.<$> ( x
                            Data..?> "MetricDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetRumMetricDefinitions
  where
  hashWithSalt _salt BatchGetRumMetricDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appMonitorName
      `Prelude.hashWithSalt` destination

instance Prelude.NFData BatchGetRumMetricDefinitions where
  rnf BatchGetRumMetricDefinitions' {..} =
    Prelude.rnf destinationArn `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf appMonitorName `Prelude.seq`
            Prelude.rnf destination

instance Data.ToHeaders BatchGetRumMetricDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath BatchGetRumMetricDefinitions where
  toPath BatchGetRumMetricDefinitions' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metrics"
      ]

instance Data.ToQuery BatchGetRumMetricDefinitions where
  toQuery BatchGetRumMetricDefinitions' {..} =
    Prelude.mconcat
      [ "destinationArn" Data.=: destinationArn,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "destination" Data.=: destination
      ]

-- | /See:/ 'newBatchGetRumMetricDefinitionsResponse' smart constructor.
data BatchGetRumMetricDefinitionsResponse = BatchGetRumMetricDefinitionsResponse'
  { -- | An array of structures that display information about the metrics that
    -- are sent by the specified app monitor to the specified destination.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | A token that you can use in a subsequent operation to retrieve the next
    -- set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRumMetricDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDefinitions', 'batchGetRumMetricDefinitionsResponse_metricDefinitions' - An array of structures that display information about the metrics that
-- are sent by the specified app monitor to the specified destination.
--
-- 'nextToken', 'batchGetRumMetricDefinitionsResponse_nextToken' - A token that you can use in a subsequent operation to retrieve the next
-- set of results.
--
-- 'httpStatus', 'batchGetRumMetricDefinitionsResponse_httpStatus' - The response's http status code.
newBatchGetRumMetricDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetRumMetricDefinitionsResponse
newBatchGetRumMetricDefinitionsResponse pHttpStatus_ =
  BatchGetRumMetricDefinitionsResponse'
    { metricDefinitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that display information about the metrics that
-- are sent by the specified app monitor to the specified destination.
batchGetRumMetricDefinitionsResponse_metricDefinitions :: Lens.Lens' BatchGetRumMetricDefinitionsResponse (Prelude.Maybe [MetricDefinition])
batchGetRumMetricDefinitionsResponse_metricDefinitions = Lens.lens (\BatchGetRumMetricDefinitionsResponse' {metricDefinitions} -> metricDefinitions) (\s@BatchGetRumMetricDefinitionsResponse' {} a -> s {metricDefinitions = a} :: BatchGetRumMetricDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that you can use in a subsequent operation to retrieve the next
-- set of results.
batchGetRumMetricDefinitionsResponse_nextToken :: Lens.Lens' BatchGetRumMetricDefinitionsResponse (Prelude.Maybe Prelude.Text)
batchGetRumMetricDefinitionsResponse_nextToken = Lens.lens (\BatchGetRumMetricDefinitionsResponse' {nextToken} -> nextToken) (\s@BatchGetRumMetricDefinitionsResponse' {} a -> s {nextToken = a} :: BatchGetRumMetricDefinitionsResponse)

-- | The response's http status code.
batchGetRumMetricDefinitionsResponse_httpStatus :: Lens.Lens' BatchGetRumMetricDefinitionsResponse Prelude.Int
batchGetRumMetricDefinitionsResponse_httpStatus = Lens.lens (\BatchGetRumMetricDefinitionsResponse' {httpStatus} -> httpStatus) (\s@BatchGetRumMetricDefinitionsResponse' {} a -> s {httpStatus = a} :: BatchGetRumMetricDefinitionsResponse)

instance
  Prelude.NFData
    BatchGetRumMetricDefinitionsResponse
  where
  rnf BatchGetRumMetricDefinitionsResponse' {..} =
    Prelude.rnf metricDefinitions `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

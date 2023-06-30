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
-- Module      : Amazonka.CloudWatchLogs.PutMetricFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a metric filter and associates it with the specified
-- log group. With metric filters, you can configure rules to extract
-- metric data from log events ingested through
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents>.
--
-- The maximum number of metric filters that can be associated with a log
-- group is 100.
--
-- When you create a metric filter, you can also optionally assign a unit
-- and dimensions to the metric that is created.
--
-- Metrics extracted from log events are charged as custom metrics. To
-- prevent unexpected high charges, do not specify high-cardinality fields
-- such as @IPAddress@ or @requestID@ as dimensions. Each different value
-- found for a dimension is treated as a separate metric and accrues
-- charges as a separate custom metric.
--
-- CloudWatch Logs disables a metric filter if it generates 1,000 different
-- name\/value pairs for your specified dimensions within a certain amount
-- of time. This helps to prevent accidental high charges.
--
-- You can also set up a billing alarm to alert you if your charges are
-- higher than expected. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/monitor_estimated_charges_with_cloudwatch.html Creating a Billing Alarm to Monitor Your Estimated Amazon Web Services Charges>.
module Amazonka.CloudWatchLogs.PutMetricFilter
  ( -- * Creating a Request
    PutMetricFilter (..),
    newPutMetricFilter,

    -- * Request Lenses
    putMetricFilter_logGroupName,
    putMetricFilter_filterName,
    putMetricFilter_filterPattern,
    putMetricFilter_metricTransformations,

    -- * Destructuring the Response
    PutMetricFilterResponse (..),
    newPutMetricFilterResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetricFilter' smart constructor.
data PutMetricFilter = PutMetricFilter'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | A name for the metric filter.
    filterName :: Prelude.Text,
    -- | A filter pattern for extracting metric data out of ingested log events.
    filterPattern :: Prelude.Text,
    -- | A collection of information that defines how metric data gets emitted.
    metricTransformations :: Prelude.NonEmpty MetricTransformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'putMetricFilter_logGroupName' - The name of the log group.
--
-- 'filterName', 'putMetricFilter_filterName' - A name for the metric filter.
--
-- 'filterPattern', 'putMetricFilter_filterPattern' - A filter pattern for extracting metric data out of ingested log events.
--
-- 'metricTransformations', 'putMetricFilter_metricTransformations' - A collection of information that defines how metric data gets emitted.
newPutMetricFilter ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  -- | 'filterPattern'
  Prelude.Text ->
  -- | 'metricTransformations'
  Prelude.NonEmpty MetricTransformation ->
  PutMetricFilter
newPutMetricFilter
  pLogGroupName_
  pFilterName_
  pFilterPattern_
  pMetricTransformations_ =
    PutMetricFilter'
      { logGroupName = pLogGroupName_,
        filterName = pFilterName_,
        filterPattern = pFilterPattern_,
        metricTransformations =
          Lens.coerced Lens.# pMetricTransformations_
      }

-- | The name of the log group.
putMetricFilter_logGroupName :: Lens.Lens' PutMetricFilter Prelude.Text
putMetricFilter_logGroupName = Lens.lens (\PutMetricFilter' {logGroupName} -> logGroupName) (\s@PutMetricFilter' {} a -> s {logGroupName = a} :: PutMetricFilter)

-- | A name for the metric filter.
putMetricFilter_filterName :: Lens.Lens' PutMetricFilter Prelude.Text
putMetricFilter_filterName = Lens.lens (\PutMetricFilter' {filterName} -> filterName) (\s@PutMetricFilter' {} a -> s {filterName = a} :: PutMetricFilter)

-- | A filter pattern for extracting metric data out of ingested log events.
putMetricFilter_filterPattern :: Lens.Lens' PutMetricFilter Prelude.Text
putMetricFilter_filterPattern = Lens.lens (\PutMetricFilter' {filterPattern} -> filterPattern) (\s@PutMetricFilter' {} a -> s {filterPattern = a} :: PutMetricFilter)

-- | A collection of information that defines how metric data gets emitted.
putMetricFilter_metricTransformations :: Lens.Lens' PutMetricFilter (Prelude.NonEmpty MetricTransformation)
putMetricFilter_metricTransformations = Lens.lens (\PutMetricFilter' {metricTransformations} -> metricTransformations) (\s@PutMetricFilter' {} a -> s {metricTransformations = a} :: PutMetricFilter) Prelude.. Lens.coerced

instance Core.AWSRequest PutMetricFilter where
  type
    AWSResponse PutMetricFilter =
      PutMetricFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutMetricFilterResponse'

instance Prelude.Hashable PutMetricFilter where
  hashWithSalt _salt PutMetricFilter' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` filterName
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` metricTransformations

instance Prelude.NFData PutMetricFilter where
  rnf PutMetricFilter' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf filterName
      `Prelude.seq` Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf metricTransformations

instance Data.ToHeaders PutMetricFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutMetricFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMetricFilter where
  toJSON PutMetricFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just ("filterName" Data..= filterName),
            Prelude.Just ("filterPattern" Data..= filterPattern),
            Prelude.Just
              ( "metricTransformations"
                  Data..= metricTransformations
              )
          ]
      )

instance Data.ToPath PutMetricFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMetricFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMetricFilterResponse' smart constructor.
data PutMetricFilterResponse = PutMetricFilterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetricFilterResponse ::
  PutMetricFilterResponse
newPutMetricFilterResponse = PutMetricFilterResponse'

instance Prelude.NFData PutMetricFilterResponse where
  rnf _ = ()

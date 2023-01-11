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
-- Module      : Amazonka.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes metric data points to Amazon CloudWatch. CloudWatch associates
-- the data points with the specified metric. If the specified metric does
-- not exist, CloudWatch creates the metric. When CloudWatch creates a
-- metric, it can take up to fifteen minutes for the metric to appear in
-- calls to
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
--
-- You can publish either individual data points in the @Value@ field, or
-- arrays of values and the number of times each value occurred during the
-- period by using the @Values@ and @Counts@ fields in the @MetricDatum@
-- structure. Using the @Values@ and @Counts@ method enables you to publish
-- up to 150 values per metric with one @PutMetricData@ request, and
-- supports retrieving percentile statistics on this data.
--
-- Each @PutMetricData@ request is limited to 1 MB in size for HTTP POST
-- requests. You can send a payload compressed by gzip. Each request is
-- also limited to no more than 1000 different metrics.
--
-- Although the @Value@ parameter accepts numbers of type @Double@,
-- CloudWatch rejects values that are either too small or too large. Values
-- must be in the range of -2^360 to 2^360. In addition, special values
-- (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- You can use up to 30 dimensions per metric to further clarify what data
-- the metric collects. Each dimension consists of a Name and Value pair.
-- For more information about specifying dimensions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- You specify the time stamp to be associated with each data point. You
-- can specify time stamps that are as much as two weeks before the current
-- date, and as much as 2 hours after the current day and time.
--
-- Data points with time stamps from 24 hours ago or longer can take at
-- least 48 hours to become available for
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>
-- from the time they are submitted. Data points with time stamps between 3
-- and 24 hours ago can take as much as 2 hours to become available for for
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>.
--
-- CloudWatch needs raw data points to calculate percentile statistics. If
-- you publish data using a statistic set instead, you can only retrieve
-- percentile statistics for this data if one of the following conditions
-- is true:
--
-- -   The @SampleCount@ value of the statistic set is 1 and @Min@, @Max@,
--     and @Sum@ are all equal.
--
-- -   The @Min@ and @Max@ are equal, and @Sum@ is equal to @Min@
--     multiplied by @SampleCount@.
module Amazonka.CloudWatch.PutMetricData
  ( -- * Creating a Request
    PutMetricData (..),
    newPutMetricData,

    -- * Request Lenses
    putMetricData_namespace,
    putMetricData_metricData,

    -- * Destructuring the Response
    PutMetricDataResponse (..),
    newPutMetricDataResponse,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetricData' smart constructor.
data PutMetricData = PutMetricData'
  { -- | The namespace for the metric data.
    --
    -- To avoid conflicts with Amazon Web Services service namespaces, you
    -- should not specify a namespace that begins with @AWS\/@
    namespace :: Prelude.Text,
    -- | The data for the metric. The array can include no more than 1000 metrics
    -- per call.
    metricData :: [MetricDatum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'putMetricData_namespace' - The namespace for the metric data.
--
-- To avoid conflicts with Amazon Web Services service namespaces, you
-- should not specify a namespace that begins with @AWS\/@
--
-- 'metricData', 'putMetricData_metricData' - The data for the metric. The array can include no more than 1000 metrics
-- per call.
newPutMetricData ::
  -- | 'namespace'
  Prelude.Text ->
  PutMetricData
newPutMetricData pNamespace_ =
  PutMetricData'
    { namespace = pNamespace_,
      metricData = Prelude.mempty
    }

-- | The namespace for the metric data.
--
-- To avoid conflicts with Amazon Web Services service namespaces, you
-- should not specify a namespace that begins with @AWS\/@
putMetricData_namespace :: Lens.Lens' PutMetricData Prelude.Text
putMetricData_namespace = Lens.lens (\PutMetricData' {namespace} -> namespace) (\s@PutMetricData' {} a -> s {namespace = a} :: PutMetricData)

-- | The data for the metric. The array can include no more than 1000 metrics
-- per call.
putMetricData_metricData :: Lens.Lens' PutMetricData [MetricDatum]
putMetricData_metricData = Lens.lens (\PutMetricData' {metricData} -> metricData) (\s@PutMetricData' {} a -> s {metricData = a} :: PutMetricData) Prelude.. Lens.coerced

instance Core.AWSRequest PutMetricData where
  type
    AWSResponse PutMetricData =
      PutMetricDataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull PutMetricDataResponse'

instance Prelude.Hashable PutMetricData where
  hashWithSalt _salt PutMetricData' {..} =
    _salt `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricData

instance Prelude.NFData PutMetricData where
  rnf PutMetricData' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf metricData

instance Data.ToHeaders PutMetricData where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutMetricData where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMetricData where
  toQuery PutMetricData' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutMetricData" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Namespace" Data.=: namespace,
        "MetricData"
          Data.=: Data.toQueryList "member" metricData
      ]

-- | /See:/ 'newPutMetricDataResponse' smart constructor.
data PutMetricDataResponse = PutMetricDataResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetricDataResponse ::
  PutMetricDataResponse
newPutMetricDataResponse = PutMetricDataResponse'

instance Prelude.NFData PutMetricDataResponse where
  rnf _ = ()

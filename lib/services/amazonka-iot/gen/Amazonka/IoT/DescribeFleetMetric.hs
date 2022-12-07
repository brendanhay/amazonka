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
-- Module      : Amazonka.IoT.DescribeFleetMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified fleet metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeFleetMetric>
-- action.
module Amazonka.IoT.DescribeFleetMetric
  ( -- * Creating a Request
    DescribeFleetMetric (..),
    newDescribeFleetMetric,

    -- * Request Lenses
    describeFleetMetric_metricName,

    -- * Destructuring the Response
    DescribeFleetMetricResponse (..),
    newDescribeFleetMetricResponse,

    -- * Response Lenses
    describeFleetMetricResponse_aggregationField,
    describeFleetMetricResponse_metricArn,
    describeFleetMetricResponse_aggregationType,
    describeFleetMetricResponse_lastModifiedDate,
    describeFleetMetricResponse_period,
    describeFleetMetricResponse_creationDate,
    describeFleetMetricResponse_description,
    describeFleetMetricResponse_indexName,
    describeFleetMetricResponse_queryVersion,
    describeFleetMetricResponse_metricName,
    describeFleetMetricResponse_queryString,
    describeFleetMetricResponse_unit,
    describeFleetMetricResponse_version,
    describeFleetMetricResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetMetric' smart constructor.
data DescribeFleetMetric = DescribeFleetMetric'
  { -- | The name of the fleet metric to describe.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'describeFleetMetric_metricName' - The name of the fleet metric to describe.
newDescribeFleetMetric ::
  -- | 'metricName'
  Prelude.Text ->
  DescribeFleetMetric
newDescribeFleetMetric pMetricName_ =
  DescribeFleetMetric' {metricName = pMetricName_}

-- | The name of the fleet metric to describe.
describeFleetMetric_metricName :: Lens.Lens' DescribeFleetMetric Prelude.Text
describeFleetMetric_metricName = Lens.lens (\DescribeFleetMetric' {metricName} -> metricName) (\s@DescribeFleetMetric' {} a -> s {metricName = a} :: DescribeFleetMetric)

instance Core.AWSRequest DescribeFleetMetric where
  type
    AWSResponse DescribeFleetMetric =
      DescribeFleetMetricResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetMetricResponse'
            Prelude.<$> (x Data..?> "aggregationField")
            Prelude.<*> (x Data..?> "metricArn")
            Prelude.<*> (x Data..?> "aggregationType")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "period")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "indexName")
            Prelude.<*> (x Data..?> "queryVersion")
            Prelude.<*> (x Data..?> "metricName")
            Prelude.<*> (x Data..?> "queryString")
            Prelude.<*> (x Data..?> "unit")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetMetric where
  hashWithSalt _salt DescribeFleetMetric' {..} =
    _salt `Prelude.hashWithSalt` metricName

instance Prelude.NFData DescribeFleetMetric where
  rnf DescribeFleetMetric' {..} = Prelude.rnf metricName

instance Data.ToHeaders DescribeFleetMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFleetMetric where
  toPath DescribeFleetMetric' {..} =
    Prelude.mconcat
      ["/fleet-metric/", Data.toBS metricName]

instance Data.ToQuery DescribeFleetMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetMetricResponse' smart constructor.
data DescribeFleetMetricResponse = DescribeFleetMetricResponse'
  { -- | The field to aggregate.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet metric to describe.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the aggregation query.
    aggregationType :: Prelude.Maybe AggregationType,
    -- | The date when the fleet metric is last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The time in seconds between fleet metric emissions. Range [60(1 min),
    -- 86400(1 day)] and must be multiple of 60.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The date when the fleet metric is created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The fleet metric description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the index to search.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet metric to describe.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The search query string.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | Used to support unit transformation such as milliseconds to seconds. The
    -- unit must be supported by
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
    unit :: Prelude.Maybe FleetMetricUnit,
    -- | The version of the fleet metric.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationField', 'describeFleetMetricResponse_aggregationField' - The field to aggregate.
--
-- 'metricArn', 'describeFleetMetricResponse_metricArn' - The ARN of the fleet metric to describe.
--
-- 'aggregationType', 'describeFleetMetricResponse_aggregationType' - The type of the aggregation query.
--
-- 'lastModifiedDate', 'describeFleetMetricResponse_lastModifiedDate' - The date when the fleet metric is last modified.
--
-- 'period', 'describeFleetMetricResponse_period' - The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
--
-- 'creationDate', 'describeFleetMetricResponse_creationDate' - The date when the fleet metric is created.
--
-- 'description', 'describeFleetMetricResponse_description' - The fleet metric description.
--
-- 'indexName', 'describeFleetMetricResponse_indexName' - The name of the index to search.
--
-- 'queryVersion', 'describeFleetMetricResponse_queryVersion' - The query version.
--
-- 'metricName', 'describeFleetMetricResponse_metricName' - The name of the fleet metric to describe.
--
-- 'queryString', 'describeFleetMetricResponse_queryString' - The search query string.
--
-- 'unit', 'describeFleetMetricResponse_unit' - Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
--
-- 'version', 'describeFleetMetricResponse_version' - The version of the fleet metric.
--
-- 'httpStatus', 'describeFleetMetricResponse_httpStatus' - The response's http status code.
newDescribeFleetMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetMetricResponse
newDescribeFleetMetricResponse pHttpStatus_ =
  DescribeFleetMetricResponse'
    { aggregationField =
        Prelude.Nothing,
      metricArn = Prelude.Nothing,
      aggregationType = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      period = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      indexName = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      metricName = Prelude.Nothing,
      queryString = Prelude.Nothing,
      unit = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The field to aggregate.
describeFleetMetricResponse_aggregationField :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_aggregationField = Lens.lens (\DescribeFleetMetricResponse' {aggregationField} -> aggregationField) (\s@DescribeFleetMetricResponse' {} a -> s {aggregationField = a} :: DescribeFleetMetricResponse)

-- | The ARN of the fleet metric to describe.
describeFleetMetricResponse_metricArn :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_metricArn = Lens.lens (\DescribeFleetMetricResponse' {metricArn} -> metricArn) (\s@DescribeFleetMetricResponse' {} a -> s {metricArn = a} :: DescribeFleetMetricResponse)

-- | The type of the aggregation query.
describeFleetMetricResponse_aggregationType :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe AggregationType)
describeFleetMetricResponse_aggregationType = Lens.lens (\DescribeFleetMetricResponse' {aggregationType} -> aggregationType) (\s@DescribeFleetMetricResponse' {} a -> s {aggregationType = a} :: DescribeFleetMetricResponse)

-- | The date when the fleet metric is last modified.
describeFleetMetricResponse_lastModifiedDate :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetMetricResponse_lastModifiedDate = Lens.lens (\DescribeFleetMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeFleetMetricResponse' {} a -> s {lastModifiedDate = a} :: DescribeFleetMetricResponse) Prelude.. Lens.mapping Data._Time

-- | The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
describeFleetMetricResponse_period :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Natural)
describeFleetMetricResponse_period = Lens.lens (\DescribeFleetMetricResponse' {period} -> period) (\s@DescribeFleetMetricResponse' {} a -> s {period = a} :: DescribeFleetMetricResponse)

-- | The date when the fleet metric is created.
describeFleetMetricResponse_creationDate :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetMetricResponse_creationDate = Lens.lens (\DescribeFleetMetricResponse' {creationDate} -> creationDate) (\s@DescribeFleetMetricResponse' {} a -> s {creationDate = a} :: DescribeFleetMetricResponse) Prelude.. Lens.mapping Data._Time

-- | The fleet metric description.
describeFleetMetricResponse_description :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_description = Lens.lens (\DescribeFleetMetricResponse' {description} -> description) (\s@DescribeFleetMetricResponse' {} a -> s {description = a} :: DescribeFleetMetricResponse)

-- | The name of the index to search.
describeFleetMetricResponse_indexName :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_indexName = Lens.lens (\DescribeFleetMetricResponse' {indexName} -> indexName) (\s@DescribeFleetMetricResponse' {} a -> s {indexName = a} :: DescribeFleetMetricResponse)

-- | The query version.
describeFleetMetricResponse_queryVersion :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_queryVersion = Lens.lens (\DescribeFleetMetricResponse' {queryVersion} -> queryVersion) (\s@DescribeFleetMetricResponse' {} a -> s {queryVersion = a} :: DescribeFleetMetricResponse)

-- | The name of the fleet metric to describe.
describeFleetMetricResponse_metricName :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_metricName = Lens.lens (\DescribeFleetMetricResponse' {metricName} -> metricName) (\s@DescribeFleetMetricResponse' {} a -> s {metricName = a} :: DescribeFleetMetricResponse)

-- | The search query string.
describeFleetMetricResponse_queryString :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Text)
describeFleetMetricResponse_queryString = Lens.lens (\DescribeFleetMetricResponse' {queryString} -> queryString) (\s@DescribeFleetMetricResponse' {} a -> s {queryString = a} :: DescribeFleetMetricResponse)

-- | Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
describeFleetMetricResponse_unit :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe FleetMetricUnit)
describeFleetMetricResponse_unit = Lens.lens (\DescribeFleetMetricResponse' {unit} -> unit) (\s@DescribeFleetMetricResponse' {} a -> s {unit = a} :: DescribeFleetMetricResponse)

-- | The version of the fleet metric.
describeFleetMetricResponse_version :: Lens.Lens' DescribeFleetMetricResponse (Prelude.Maybe Prelude.Integer)
describeFleetMetricResponse_version = Lens.lens (\DescribeFleetMetricResponse' {version} -> version) (\s@DescribeFleetMetricResponse' {} a -> s {version = a} :: DescribeFleetMetricResponse)

-- | The response's http status code.
describeFleetMetricResponse_httpStatus :: Lens.Lens' DescribeFleetMetricResponse Prelude.Int
describeFleetMetricResponse_httpStatus = Lens.lens (\DescribeFleetMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetMetricResponse' {} a -> s {httpStatus = a} :: DescribeFleetMetricResponse)

instance Prelude.NFData DescribeFleetMetricResponse where
  rnf DescribeFleetMetricResponse' {..} =
    Prelude.rnf aggregationField
      `Prelude.seq` Prelude.rnf metricArn
      `Prelude.seq` Prelude.rnf aggregationType
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.IoT.CreateFleetMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateFleetMetric>
-- action.
module Amazonka.IoT.CreateFleetMetric
  ( -- * Creating a Request
    CreateFleetMetric (..),
    newCreateFleetMetric,

    -- * Request Lenses
    createFleetMetric_tags,
    createFleetMetric_description,
    createFleetMetric_indexName,
    createFleetMetric_queryVersion,
    createFleetMetric_unit,
    createFleetMetric_metricName,
    createFleetMetric_queryString,
    createFleetMetric_aggregationType,
    createFleetMetric_period,
    createFleetMetric_aggregationField,

    -- * Destructuring the Response
    CreateFleetMetricResponse (..),
    newCreateFleetMetricResponse,

    -- * Response Lenses
    createFleetMetricResponse_metricArn,
    createFleetMetricResponse_metricName,
    createFleetMetricResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFleetMetric' smart constructor.
data CreateFleetMetric = CreateFleetMetric'
  { -- | Metadata, which can be used to manage the fleet metric.
    tags :: Prelude.Maybe [Tag],
    -- | The fleet metric description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the index to search.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | Used to support unit transformation such as milliseconds to seconds. The
    -- unit must be supported by
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
    -- Default to null.
    unit :: Prelude.Maybe FleetMetricUnit,
    -- | The name of the fleet metric to create.
    metricName :: Prelude.Text,
    -- | The search query string.
    queryString :: Prelude.Text,
    -- | The type of the aggregation query.
    aggregationType :: AggregationType,
    -- | The time in seconds between fleet metric emissions. Range [60(1 min),
    -- 86400(1 day)] and must be multiple of 60.
    period :: Prelude.Natural,
    -- | The field to aggregate.
    aggregationField :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFleetMetric_tags' - Metadata, which can be used to manage the fleet metric.
--
-- 'description', 'createFleetMetric_description' - The fleet metric description.
--
-- 'indexName', 'createFleetMetric_indexName' - The name of the index to search.
--
-- 'queryVersion', 'createFleetMetric_queryVersion' - The query version.
--
-- 'unit', 'createFleetMetric_unit' - Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
-- Default to null.
--
-- 'metricName', 'createFleetMetric_metricName' - The name of the fleet metric to create.
--
-- 'queryString', 'createFleetMetric_queryString' - The search query string.
--
-- 'aggregationType', 'createFleetMetric_aggregationType' - The type of the aggregation query.
--
-- 'period', 'createFleetMetric_period' - The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
--
-- 'aggregationField', 'createFleetMetric_aggregationField' - The field to aggregate.
newCreateFleetMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  -- | 'aggregationType'
  AggregationType ->
  -- | 'period'
  Prelude.Natural ->
  -- | 'aggregationField'
  Prelude.Text ->
  CreateFleetMetric
newCreateFleetMetric
  pMetricName_
  pQueryString_
  pAggregationType_
  pPeriod_
  pAggregationField_ =
    CreateFleetMetric'
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        indexName = Prelude.Nothing,
        queryVersion = Prelude.Nothing,
        unit = Prelude.Nothing,
        metricName = pMetricName_,
        queryString = pQueryString_,
        aggregationType = pAggregationType_,
        period = pPeriod_,
        aggregationField = pAggregationField_
      }

-- | Metadata, which can be used to manage the fleet metric.
createFleetMetric_tags :: Lens.Lens' CreateFleetMetric (Prelude.Maybe [Tag])
createFleetMetric_tags = Lens.lens (\CreateFleetMetric' {tags} -> tags) (\s@CreateFleetMetric' {} a -> s {tags = a} :: CreateFleetMetric) Prelude.. Lens.mapping Lens.coerced

-- | The fleet metric description.
createFleetMetric_description :: Lens.Lens' CreateFleetMetric (Prelude.Maybe Prelude.Text)
createFleetMetric_description = Lens.lens (\CreateFleetMetric' {description} -> description) (\s@CreateFleetMetric' {} a -> s {description = a} :: CreateFleetMetric)

-- | The name of the index to search.
createFleetMetric_indexName :: Lens.Lens' CreateFleetMetric (Prelude.Maybe Prelude.Text)
createFleetMetric_indexName = Lens.lens (\CreateFleetMetric' {indexName} -> indexName) (\s@CreateFleetMetric' {} a -> s {indexName = a} :: CreateFleetMetric)

-- | The query version.
createFleetMetric_queryVersion :: Lens.Lens' CreateFleetMetric (Prelude.Maybe Prelude.Text)
createFleetMetric_queryVersion = Lens.lens (\CreateFleetMetric' {queryVersion} -> queryVersion) (\s@CreateFleetMetric' {} a -> s {queryVersion = a} :: CreateFleetMetric)

-- | Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
-- Default to null.
createFleetMetric_unit :: Lens.Lens' CreateFleetMetric (Prelude.Maybe FleetMetricUnit)
createFleetMetric_unit = Lens.lens (\CreateFleetMetric' {unit} -> unit) (\s@CreateFleetMetric' {} a -> s {unit = a} :: CreateFleetMetric)

-- | The name of the fleet metric to create.
createFleetMetric_metricName :: Lens.Lens' CreateFleetMetric Prelude.Text
createFleetMetric_metricName = Lens.lens (\CreateFleetMetric' {metricName} -> metricName) (\s@CreateFleetMetric' {} a -> s {metricName = a} :: CreateFleetMetric)

-- | The search query string.
createFleetMetric_queryString :: Lens.Lens' CreateFleetMetric Prelude.Text
createFleetMetric_queryString = Lens.lens (\CreateFleetMetric' {queryString} -> queryString) (\s@CreateFleetMetric' {} a -> s {queryString = a} :: CreateFleetMetric)

-- | The type of the aggregation query.
createFleetMetric_aggregationType :: Lens.Lens' CreateFleetMetric AggregationType
createFleetMetric_aggregationType = Lens.lens (\CreateFleetMetric' {aggregationType} -> aggregationType) (\s@CreateFleetMetric' {} a -> s {aggregationType = a} :: CreateFleetMetric)

-- | The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
createFleetMetric_period :: Lens.Lens' CreateFleetMetric Prelude.Natural
createFleetMetric_period = Lens.lens (\CreateFleetMetric' {period} -> period) (\s@CreateFleetMetric' {} a -> s {period = a} :: CreateFleetMetric)

-- | The field to aggregate.
createFleetMetric_aggregationField :: Lens.Lens' CreateFleetMetric Prelude.Text
createFleetMetric_aggregationField = Lens.lens (\CreateFleetMetric' {aggregationField} -> aggregationField) (\s@CreateFleetMetric' {} a -> s {aggregationField = a} :: CreateFleetMetric)

instance Core.AWSRequest CreateFleetMetric where
  type
    AWSResponse CreateFleetMetric =
      CreateFleetMetricResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetMetricResponse'
            Prelude.<$> (x Core..?> "metricArn")
            Prelude.<*> (x Core..?> "metricName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleetMetric where
  hashWithSalt _salt CreateFleetMetric' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` queryVersion
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` aggregationType
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` aggregationField

instance Prelude.NFData CreateFleetMetric where
  rnf CreateFleetMetric' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf aggregationType
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf aggregationField

instance Core.ToHeaders CreateFleetMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateFleetMetric where
  toJSON CreateFleetMetric' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            ("indexName" Core..=) Prelude.<$> indexName,
            ("queryVersion" Core..=) Prelude.<$> queryVersion,
            ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just ("queryString" Core..= queryString),
            Prelude.Just
              ("aggregationType" Core..= aggregationType),
            Prelude.Just ("period" Core..= period),
            Prelude.Just
              ("aggregationField" Core..= aggregationField)
          ]
      )

instance Core.ToPath CreateFleetMetric where
  toPath CreateFleetMetric' {..} =
    Prelude.mconcat
      ["/fleet-metric/", Core.toBS metricName]

instance Core.ToQuery CreateFleetMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetMetricResponse' smart constructor.
data CreateFleetMetricResponse = CreateFleetMetricResponse'
  { -- | The Amazon Resource Name (ARN) of the new fleet metric.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet metric to create.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricArn', 'createFleetMetricResponse_metricArn' - The Amazon Resource Name (ARN) of the new fleet metric.
--
-- 'metricName', 'createFleetMetricResponse_metricName' - The name of the fleet metric to create.
--
-- 'httpStatus', 'createFleetMetricResponse_httpStatus' - The response's http status code.
newCreateFleetMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetMetricResponse
newCreateFleetMetricResponse pHttpStatus_ =
  CreateFleetMetricResponse'
    { metricArn =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new fleet metric.
createFleetMetricResponse_metricArn :: Lens.Lens' CreateFleetMetricResponse (Prelude.Maybe Prelude.Text)
createFleetMetricResponse_metricArn = Lens.lens (\CreateFleetMetricResponse' {metricArn} -> metricArn) (\s@CreateFleetMetricResponse' {} a -> s {metricArn = a} :: CreateFleetMetricResponse)

-- | The name of the fleet metric to create.
createFleetMetricResponse_metricName :: Lens.Lens' CreateFleetMetricResponse (Prelude.Maybe Prelude.Text)
createFleetMetricResponse_metricName = Lens.lens (\CreateFleetMetricResponse' {metricName} -> metricName) (\s@CreateFleetMetricResponse' {} a -> s {metricName = a} :: CreateFleetMetricResponse)

-- | The response's http status code.
createFleetMetricResponse_httpStatus :: Lens.Lens' CreateFleetMetricResponse Prelude.Int
createFleetMetricResponse_httpStatus = Lens.lens (\CreateFleetMetricResponse' {httpStatus} -> httpStatus) (\s@CreateFleetMetricResponse' {} a -> s {httpStatus = a} :: CreateFleetMetricResponse)

instance Prelude.NFData CreateFleetMetricResponse where
  rnf CreateFleetMetricResponse' {..} =
    Prelude.rnf metricArn
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf httpStatus

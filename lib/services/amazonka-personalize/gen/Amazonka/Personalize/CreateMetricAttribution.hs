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
-- Module      : Amazonka.Personalize.CreateMetricAttribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a metric attribution. A metric attribution creates reports on
-- the data that you import into Amazon Personalize. Depending on how you
-- imported the data, you can view reports in Amazon CloudWatch or Amazon
-- S3. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
module Amazonka.Personalize.CreateMetricAttribution
  ( -- * Creating a Request
    CreateMetricAttribution (..),
    newCreateMetricAttribution,

    -- * Request Lenses
    createMetricAttribution_name,
    createMetricAttribution_datasetGroupArn,
    createMetricAttribution_metrics,
    createMetricAttribution_metricsOutputConfig,

    -- * Destructuring the Response
    CreateMetricAttributionResponse (..),
    newCreateMetricAttributionResponse,

    -- * Response Lenses
    createMetricAttributionResponse_metricAttributionArn,
    createMetricAttributionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMetricAttribution' smart constructor.
data CreateMetricAttribution = CreateMetricAttribution'
  { -- | A name for the metric attribution.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination dataset group for the
    -- metric attribution.
    datasetGroupArn :: Prelude.Text,
    -- | A list of metric attributes for the metric attribution. Each metric
    -- attribute specifies an event type to track and a function. Available
    -- functions are @SUM()@ or @SAMPLECOUNT()@. For SUM() functions, provide
    -- the dataset type (either Interactions or Items) and column to sum as a
    -- parameter. For example SUM(Items.PRICE).
    metrics :: [MetricAttribute],
    -- | The output configuration details for the metric attribution.
    metricsOutputConfig :: MetricAttributionOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createMetricAttribution_name' - A name for the metric attribution.
--
-- 'datasetGroupArn', 'createMetricAttribution_datasetGroupArn' - The Amazon Resource Name (ARN) of the destination dataset group for the
-- metric attribution.
--
-- 'metrics', 'createMetricAttribution_metrics' - A list of metric attributes for the metric attribution. Each metric
-- attribute specifies an event type to track and a function. Available
-- functions are @SUM()@ or @SAMPLECOUNT()@. For SUM() functions, provide
-- the dataset type (either Interactions or Items) and column to sum as a
-- parameter. For example SUM(Items.PRICE).
--
-- 'metricsOutputConfig', 'createMetricAttribution_metricsOutputConfig' - The output configuration details for the metric attribution.
newCreateMetricAttribution ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  -- | 'metricsOutputConfig'
  MetricAttributionOutput ->
  CreateMetricAttribution
newCreateMetricAttribution
  pName_
  pDatasetGroupArn_
  pMetricsOutputConfig_ =
    CreateMetricAttribution'
      { name = pName_,
        datasetGroupArn = pDatasetGroupArn_,
        metrics = Prelude.mempty,
        metricsOutputConfig = pMetricsOutputConfig_
      }

-- | A name for the metric attribution.
createMetricAttribution_name :: Lens.Lens' CreateMetricAttribution Prelude.Text
createMetricAttribution_name = Lens.lens (\CreateMetricAttribution' {name} -> name) (\s@CreateMetricAttribution' {} a -> s {name = a} :: CreateMetricAttribution)

-- | The Amazon Resource Name (ARN) of the destination dataset group for the
-- metric attribution.
createMetricAttribution_datasetGroupArn :: Lens.Lens' CreateMetricAttribution Prelude.Text
createMetricAttribution_datasetGroupArn = Lens.lens (\CreateMetricAttribution' {datasetGroupArn} -> datasetGroupArn) (\s@CreateMetricAttribution' {} a -> s {datasetGroupArn = a} :: CreateMetricAttribution)

-- | A list of metric attributes for the metric attribution. Each metric
-- attribute specifies an event type to track and a function. Available
-- functions are @SUM()@ or @SAMPLECOUNT()@. For SUM() functions, provide
-- the dataset type (either Interactions or Items) and column to sum as a
-- parameter. For example SUM(Items.PRICE).
createMetricAttribution_metrics :: Lens.Lens' CreateMetricAttribution [MetricAttribute]
createMetricAttribution_metrics = Lens.lens (\CreateMetricAttribution' {metrics} -> metrics) (\s@CreateMetricAttribution' {} a -> s {metrics = a} :: CreateMetricAttribution) Prelude.. Lens.coerced

-- | The output configuration details for the metric attribution.
createMetricAttribution_metricsOutputConfig :: Lens.Lens' CreateMetricAttribution MetricAttributionOutput
createMetricAttribution_metricsOutputConfig = Lens.lens (\CreateMetricAttribution' {metricsOutputConfig} -> metricsOutputConfig) (\s@CreateMetricAttribution' {} a -> s {metricsOutputConfig = a} :: CreateMetricAttribution)

instance Core.AWSRequest CreateMetricAttribution where
  type
    AWSResponse CreateMetricAttribution =
      CreateMetricAttributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMetricAttributionResponse'
            Prelude.<$> (x Data..?> "metricAttributionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMetricAttribution where
  hashWithSalt _salt CreateMetricAttribution' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` metricsOutputConfig

instance Prelude.NFData CreateMetricAttribution where
  rnf CreateMetricAttribution' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf datasetGroupArn `Prelude.seq`
        Prelude.rnf metrics `Prelude.seq`
          Prelude.rnf metricsOutputConfig

instance Data.ToHeaders CreateMetricAttribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateMetricAttribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMetricAttribution where
  toJSON CreateMetricAttribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn),
            Prelude.Just ("metrics" Data..= metrics),
            Prelude.Just
              ("metricsOutputConfig" Data..= metricsOutputConfig)
          ]
      )

instance Data.ToPath CreateMetricAttribution where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMetricAttribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMetricAttributionResponse' smart constructor.
data CreateMetricAttributionResponse = CreateMetricAttributionResponse'
  { -- | The Amazon Resource Name (ARN) for the new metric attribution.
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMetricAttributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttributionArn', 'createMetricAttributionResponse_metricAttributionArn' - The Amazon Resource Name (ARN) for the new metric attribution.
--
-- 'httpStatus', 'createMetricAttributionResponse_httpStatus' - The response's http status code.
newCreateMetricAttributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMetricAttributionResponse
newCreateMetricAttributionResponse pHttpStatus_ =
  CreateMetricAttributionResponse'
    { metricAttributionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the new metric attribution.
createMetricAttributionResponse_metricAttributionArn :: Lens.Lens' CreateMetricAttributionResponse (Prelude.Maybe Prelude.Text)
createMetricAttributionResponse_metricAttributionArn = Lens.lens (\CreateMetricAttributionResponse' {metricAttributionArn} -> metricAttributionArn) (\s@CreateMetricAttributionResponse' {} a -> s {metricAttributionArn = a} :: CreateMetricAttributionResponse)

-- | The response's http status code.
createMetricAttributionResponse_httpStatus :: Lens.Lens' CreateMetricAttributionResponse Prelude.Int
createMetricAttributionResponse_httpStatus = Lens.lens (\CreateMetricAttributionResponse' {httpStatus} -> httpStatus) (\s@CreateMetricAttributionResponse' {} a -> s {httpStatus = a} :: CreateMetricAttributionResponse)

instance
  Prelude.NFData
    CreateMetricAttributionResponse
  where
  rnf CreateMetricAttributionResponse' {..} =
    Prelude.rnf metricAttributionArn `Prelude.seq`
      Prelude.rnf httpStatus

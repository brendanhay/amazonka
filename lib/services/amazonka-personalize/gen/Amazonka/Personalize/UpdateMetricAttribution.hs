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
-- Module      : Amazonka.Personalize.UpdateMetricAttribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metric attribution.
module Amazonka.Personalize.UpdateMetricAttribution
  ( -- * Creating a Request
    UpdateMetricAttribution (..),
    newUpdateMetricAttribution,

    -- * Request Lenses
    updateMetricAttribution_addMetrics,
    updateMetricAttribution_metricAttributionArn,
    updateMetricAttribution_metricsOutputConfig,
    updateMetricAttribution_removeMetrics,

    -- * Destructuring the Response
    UpdateMetricAttributionResponse (..),
    newUpdateMetricAttributionResponse,

    -- * Response Lenses
    updateMetricAttributionResponse_metricAttributionArn,
    updateMetricAttributionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMetricAttribution' smart constructor.
data UpdateMetricAttribution = UpdateMetricAttribution'
  { -- | Add new metric attributes to the metric attribution.
    addMetrics :: Prelude.Maybe [MetricAttribute],
    -- | The Amazon Resource Name (ARN) for the metric attribution to update.
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | An output config for the metric attribution.
    metricsOutputConfig :: Prelude.Maybe MetricAttributionOutput,
    -- | Remove metric attributes from the metric attribution.
    removeMetrics :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addMetrics', 'updateMetricAttribution_addMetrics' - Add new metric attributes to the metric attribution.
--
-- 'metricAttributionArn', 'updateMetricAttribution_metricAttributionArn' - The Amazon Resource Name (ARN) for the metric attribution to update.
--
-- 'metricsOutputConfig', 'updateMetricAttribution_metricsOutputConfig' - An output config for the metric attribution.
--
-- 'removeMetrics', 'updateMetricAttribution_removeMetrics' - Remove metric attributes from the metric attribution.
newUpdateMetricAttribution ::
  UpdateMetricAttribution
newUpdateMetricAttribution =
  UpdateMetricAttribution'
    { addMetrics =
        Prelude.Nothing,
      metricAttributionArn = Prelude.Nothing,
      metricsOutputConfig = Prelude.Nothing,
      removeMetrics = Prelude.Nothing
    }

-- | Add new metric attributes to the metric attribution.
updateMetricAttribution_addMetrics :: Lens.Lens' UpdateMetricAttribution (Prelude.Maybe [MetricAttribute])
updateMetricAttribution_addMetrics = Lens.lens (\UpdateMetricAttribution' {addMetrics} -> addMetrics) (\s@UpdateMetricAttribution' {} a -> s {addMetrics = a} :: UpdateMetricAttribution) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the metric attribution to update.
updateMetricAttribution_metricAttributionArn :: Lens.Lens' UpdateMetricAttribution (Prelude.Maybe Prelude.Text)
updateMetricAttribution_metricAttributionArn = Lens.lens (\UpdateMetricAttribution' {metricAttributionArn} -> metricAttributionArn) (\s@UpdateMetricAttribution' {} a -> s {metricAttributionArn = a} :: UpdateMetricAttribution)

-- | An output config for the metric attribution.
updateMetricAttribution_metricsOutputConfig :: Lens.Lens' UpdateMetricAttribution (Prelude.Maybe MetricAttributionOutput)
updateMetricAttribution_metricsOutputConfig = Lens.lens (\UpdateMetricAttribution' {metricsOutputConfig} -> metricsOutputConfig) (\s@UpdateMetricAttribution' {} a -> s {metricsOutputConfig = a} :: UpdateMetricAttribution)

-- | Remove metric attributes from the metric attribution.
updateMetricAttribution_removeMetrics :: Lens.Lens' UpdateMetricAttribution (Prelude.Maybe [Prelude.Text])
updateMetricAttribution_removeMetrics = Lens.lens (\UpdateMetricAttribution' {removeMetrics} -> removeMetrics) (\s@UpdateMetricAttribution' {} a -> s {removeMetrics = a} :: UpdateMetricAttribution) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest UpdateMetricAttribution where
  type
    AWSResponse UpdateMetricAttribution =
      UpdateMetricAttributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMetricAttributionResponse'
            Prelude.<$> (x Data..?> "metricAttributionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMetricAttribution where
  hashWithSalt _salt UpdateMetricAttribution' {..} =
    _salt `Prelude.hashWithSalt` addMetrics
      `Prelude.hashWithSalt` metricAttributionArn
      `Prelude.hashWithSalt` metricsOutputConfig
      `Prelude.hashWithSalt` removeMetrics

instance Prelude.NFData UpdateMetricAttribution where
  rnf UpdateMetricAttribution' {..} =
    Prelude.rnf addMetrics
      `Prelude.seq` Prelude.rnf metricAttributionArn
      `Prelude.seq` Prelude.rnf metricsOutputConfig
      `Prelude.seq` Prelude.rnf removeMetrics

instance Data.ToHeaders UpdateMetricAttribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.UpdateMetricAttribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMetricAttribution where
  toJSON UpdateMetricAttribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addMetrics" Data..=) Prelude.<$> addMetrics,
            ("metricAttributionArn" Data..=)
              Prelude.<$> metricAttributionArn,
            ("metricsOutputConfig" Data..=)
              Prelude.<$> metricsOutputConfig,
            ("removeMetrics" Data..=) Prelude.<$> removeMetrics
          ]
      )

instance Data.ToPath UpdateMetricAttribution where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMetricAttribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMetricAttributionResponse' smart constructor.
data UpdateMetricAttributionResponse = UpdateMetricAttributionResponse'
  { -- | The Amazon Resource Name (ARN) for the metric attribution that you
    -- updated.
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMetricAttributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttributionArn', 'updateMetricAttributionResponse_metricAttributionArn' - The Amazon Resource Name (ARN) for the metric attribution that you
-- updated.
--
-- 'httpStatus', 'updateMetricAttributionResponse_httpStatus' - The response's http status code.
newUpdateMetricAttributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMetricAttributionResponse
newUpdateMetricAttributionResponse pHttpStatus_ =
  UpdateMetricAttributionResponse'
    { metricAttributionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the metric attribution that you
-- updated.
updateMetricAttributionResponse_metricAttributionArn :: Lens.Lens' UpdateMetricAttributionResponse (Prelude.Maybe Prelude.Text)
updateMetricAttributionResponse_metricAttributionArn = Lens.lens (\UpdateMetricAttributionResponse' {metricAttributionArn} -> metricAttributionArn) (\s@UpdateMetricAttributionResponse' {} a -> s {metricAttributionArn = a} :: UpdateMetricAttributionResponse)

-- | The response's http status code.
updateMetricAttributionResponse_httpStatus :: Lens.Lens' UpdateMetricAttributionResponse Prelude.Int
updateMetricAttributionResponse_httpStatus = Lens.lens (\UpdateMetricAttributionResponse' {httpStatus} -> httpStatus) (\s@UpdateMetricAttributionResponse' {} a -> s {httpStatus = a} :: UpdateMetricAttributionResponse)

instance
  Prelude.NFData
    UpdateMetricAttributionResponse
  where
  rnf UpdateMetricAttributionResponse' {..} =
    Prelude.rnf metricAttributionArn
      `Prelude.seq` Prelude.rnf httpStatus

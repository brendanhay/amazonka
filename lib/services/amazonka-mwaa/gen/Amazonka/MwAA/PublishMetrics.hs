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
-- Module      : Amazonka.MwAA.PublishMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Internal only__. Publishes environment health metrics to Amazon
-- CloudWatch.
module Amazonka.MwAA.PublishMetrics
  ( -- * Creating a Request
    PublishMetrics (..),
    newPublishMetrics,

    -- * Request Lenses
    publishMetrics_environmentName,
    publishMetrics_metricData,

    -- * Destructuring the Response
    PublishMetricsResponse (..),
    newPublishMetricsResponse,

    -- * Response Lenses
    publishMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishMetrics' smart constructor.
data PublishMetrics = PublishMetrics'
  { -- | __Internal only__. The name of the environment.
    environmentName :: Prelude.Text,
    -- | __Internal only__. Publishes metrics to Amazon CloudWatch. To learn more
    -- about the metrics published to Amazon CloudWatch, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/cw-metrics.html Amazon MWAA performance metrics in Amazon CloudWatch>.
    metricData :: [MetricDatum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'publishMetrics_environmentName' - __Internal only__. The name of the environment.
--
-- 'metricData', 'publishMetrics_metricData' - __Internal only__. Publishes metrics to Amazon CloudWatch. To learn more
-- about the metrics published to Amazon CloudWatch, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/cw-metrics.html Amazon MWAA performance metrics in Amazon CloudWatch>.
newPublishMetrics ::
  -- | 'environmentName'
  Prelude.Text ->
  PublishMetrics
newPublishMetrics pEnvironmentName_ =
  PublishMetrics'
    { environmentName =
        pEnvironmentName_,
      metricData = Prelude.mempty
    }

-- | __Internal only__. The name of the environment.
publishMetrics_environmentName :: Lens.Lens' PublishMetrics Prelude.Text
publishMetrics_environmentName = Lens.lens (\PublishMetrics' {environmentName} -> environmentName) (\s@PublishMetrics' {} a -> s {environmentName = a} :: PublishMetrics)

-- | __Internal only__. Publishes metrics to Amazon CloudWatch. To learn more
-- about the metrics published to Amazon CloudWatch, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/cw-metrics.html Amazon MWAA performance metrics in Amazon CloudWatch>.
publishMetrics_metricData :: Lens.Lens' PublishMetrics [MetricDatum]
publishMetrics_metricData = Lens.lens (\PublishMetrics' {metricData} -> metricData) (\s@PublishMetrics' {} a -> s {metricData = a} :: PublishMetrics) Prelude.. Lens.coerced

instance Core.AWSRequest PublishMetrics where
  type
    AWSResponse PublishMetrics =
      PublishMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PublishMetricsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishMetrics where
  hashWithSalt _salt PublishMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` metricData

instance Prelude.NFData PublishMetrics where
  rnf PublishMetrics' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf metricData

instance Data.ToHeaders PublishMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PublishMetrics where
  toJSON PublishMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MetricData" Data..= metricData)]
      )

instance Data.ToPath PublishMetrics where
  toPath PublishMetrics' {..} =
    Prelude.mconcat
      ["/metrics/environments/", Data.toBS environmentName]

instance Data.ToQuery PublishMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishMetricsResponse' smart constructor.
data PublishMetricsResponse = PublishMetricsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'publishMetricsResponse_httpStatus' - The response's http status code.
newPublishMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishMetricsResponse
newPublishMetricsResponse pHttpStatus_ =
  PublishMetricsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
publishMetricsResponse_httpStatus :: Lens.Lens' PublishMetricsResponse Prelude.Int
publishMetricsResponse_httpStatus = Lens.lens (\PublishMetricsResponse' {httpStatus} -> httpStatus) (\s@PublishMetricsResponse' {} a -> s {httpStatus = a} :: PublishMetricsResponse)

instance Prelude.NFData PublishMetricsResponse where
  rnf PublishMetricsResponse' {..} =
    Prelude.rnf httpStatus

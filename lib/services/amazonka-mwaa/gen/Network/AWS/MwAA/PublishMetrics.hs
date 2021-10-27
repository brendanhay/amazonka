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
-- Module      : Network.AWS.MwAA.PublishMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An operation for publishing metrics from the customers to the Ops plane.
module Network.AWS.MwAA.PublishMetrics
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MwAA.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPublishMetrics' smart constructor.
data PublishMetrics = PublishMetrics'
  { -- | Publishes environment metric data to Amazon CloudWatch.
    environmentName :: Prelude.Text,
    -- | Publishes metric data points to Amazon CloudWatch. CloudWatch associates
    -- the data points with the specified metrica.
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
-- 'environmentName', 'publishMetrics_environmentName' - Publishes environment metric data to Amazon CloudWatch.
--
-- 'metricData', 'publishMetrics_metricData' - Publishes metric data points to Amazon CloudWatch. CloudWatch associates
-- the data points with the specified metrica.
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

-- | Publishes environment metric data to Amazon CloudWatch.
publishMetrics_environmentName :: Lens.Lens' PublishMetrics Prelude.Text
publishMetrics_environmentName = Lens.lens (\PublishMetrics' {environmentName} -> environmentName) (\s@PublishMetrics' {} a -> s {environmentName = a} :: PublishMetrics)

-- | Publishes metric data points to Amazon CloudWatch. CloudWatch associates
-- the data points with the specified metrica.
publishMetrics_metricData :: Lens.Lens' PublishMetrics [MetricDatum]
publishMetrics_metricData = Lens.lens (\PublishMetrics' {metricData} -> metricData) (\s@PublishMetrics' {} a -> s {metricData = a} :: PublishMetrics) Prelude.. Lens.coerced

instance Core.AWSRequest PublishMetrics where
  type
    AWSResponse PublishMetrics =
      PublishMetricsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PublishMetricsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishMetrics

instance Prelude.NFData PublishMetrics

instance Core.ToHeaders PublishMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PublishMetrics where
  toJSON PublishMetrics' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("MetricData" Core..= metricData)]
      )

instance Core.ToPath PublishMetrics where
  toPath PublishMetrics' {..} =
    Prelude.mconcat
      ["/metrics/environments/", Core.toBS environmentName]

instance Core.ToQuery PublishMetrics where
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

instance Prelude.NFData PublishMetricsResponse

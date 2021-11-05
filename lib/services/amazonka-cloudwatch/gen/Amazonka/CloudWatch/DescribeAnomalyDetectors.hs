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
-- Module      : Amazonka.CloudWatch.DescribeAnomalyDetectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the anomaly detection models that you have created in your
-- account. You can list all models in your account or filter the results
-- to only the models that are related to a certain namespace, metric name,
-- or metric dimension.
module Amazonka.CloudWatch.DescribeAnomalyDetectors
  ( -- * Creating a Request
    DescribeAnomalyDetectors (..),
    newDescribeAnomalyDetectors,

    -- * Request Lenses
    describeAnomalyDetectors_metricName,
    describeAnomalyDetectors_namespace,
    describeAnomalyDetectors_nextToken,
    describeAnomalyDetectors_dimensions,
    describeAnomalyDetectors_maxResults,

    -- * Destructuring the Response
    DescribeAnomalyDetectorsResponse (..),
    newDescribeAnomalyDetectorsResponse,

    -- * Response Lenses
    describeAnomalyDetectorsResponse_anomalyDetectors,
    describeAnomalyDetectorsResponse_nextToken,
    describeAnomalyDetectorsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnomalyDetectors' smart constructor.
data DescribeAnomalyDetectors = DescribeAnomalyDetectors'
  { -- | Limits the results to only the anomaly detection models that are
    -- associated with the specified metric name. If there are multiple metrics
    -- with this name in different namespaces that have anomaly detection
    -- models, they\'re all returned.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | Limits the results to only the anomaly detection models that are
    -- associated with the specified namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | Use the token returned by the previous operation to request the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limits the results to only the anomaly detection models that are
    -- associated with the specified metric dimensions. If there are multiple
    -- metrics that have these dimensions and have anomaly detection models
    -- associated, they\'re all returned.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The maximum number of results to return in one operation. The maximum
    -- value that you can specify is 100.
    --
    -- To retrieve the remaining results, make another call with the returned
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'describeAnomalyDetectors_metricName' - Limits the results to only the anomaly detection models that are
-- associated with the specified metric name. If there are multiple metrics
-- with this name in different namespaces that have anomaly detection
-- models, they\'re all returned.
--
-- 'namespace', 'describeAnomalyDetectors_namespace' - Limits the results to only the anomaly detection models that are
-- associated with the specified namespace.
--
-- 'nextToken', 'describeAnomalyDetectors_nextToken' - Use the token returned by the previous operation to request the next
-- page of results.
--
-- 'dimensions', 'describeAnomalyDetectors_dimensions' - Limits the results to only the anomaly detection models that are
-- associated with the specified metric dimensions. If there are multiple
-- metrics that have these dimensions and have anomaly detection models
-- associated, they\'re all returned.
--
-- 'maxResults', 'describeAnomalyDetectors_maxResults' - The maximum number of results to return in one operation. The maximum
-- value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
newDescribeAnomalyDetectors ::
  DescribeAnomalyDetectors
newDescribeAnomalyDetectors =
  DescribeAnomalyDetectors'
    { metricName =
        Prelude.Nothing,
      namespace = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Limits the results to only the anomaly detection models that are
-- associated with the specified metric name. If there are multiple metrics
-- with this name in different namespaces that have anomaly detection
-- models, they\'re all returned.
describeAnomalyDetectors_metricName :: Lens.Lens' DescribeAnomalyDetectors (Prelude.Maybe Prelude.Text)
describeAnomalyDetectors_metricName = Lens.lens (\DescribeAnomalyDetectors' {metricName} -> metricName) (\s@DescribeAnomalyDetectors' {} a -> s {metricName = a} :: DescribeAnomalyDetectors)

-- | Limits the results to only the anomaly detection models that are
-- associated with the specified namespace.
describeAnomalyDetectors_namespace :: Lens.Lens' DescribeAnomalyDetectors (Prelude.Maybe Prelude.Text)
describeAnomalyDetectors_namespace = Lens.lens (\DescribeAnomalyDetectors' {namespace} -> namespace) (\s@DescribeAnomalyDetectors' {} a -> s {namespace = a} :: DescribeAnomalyDetectors)

-- | Use the token returned by the previous operation to request the next
-- page of results.
describeAnomalyDetectors_nextToken :: Lens.Lens' DescribeAnomalyDetectors (Prelude.Maybe Prelude.Text)
describeAnomalyDetectors_nextToken = Lens.lens (\DescribeAnomalyDetectors' {nextToken} -> nextToken) (\s@DescribeAnomalyDetectors' {} a -> s {nextToken = a} :: DescribeAnomalyDetectors)

-- | Limits the results to only the anomaly detection models that are
-- associated with the specified metric dimensions. If there are multiple
-- metrics that have these dimensions and have anomaly detection models
-- associated, they\'re all returned.
describeAnomalyDetectors_dimensions :: Lens.Lens' DescribeAnomalyDetectors (Prelude.Maybe [Dimension])
describeAnomalyDetectors_dimensions = Lens.lens (\DescribeAnomalyDetectors' {dimensions} -> dimensions) (\s@DescribeAnomalyDetectors' {} a -> s {dimensions = a} :: DescribeAnomalyDetectors) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in one operation. The maximum
-- value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
describeAnomalyDetectors_maxResults :: Lens.Lens' DescribeAnomalyDetectors (Prelude.Maybe Prelude.Natural)
describeAnomalyDetectors_maxResults = Lens.lens (\DescribeAnomalyDetectors' {maxResults} -> maxResults) (\s@DescribeAnomalyDetectors' {} a -> s {maxResults = a} :: DescribeAnomalyDetectors)

instance Core.AWSRequest DescribeAnomalyDetectors where
  type
    AWSResponse DescribeAnomalyDetectors =
      DescribeAnomalyDetectorsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAnomalyDetectorsResult"
      ( \s h x ->
          DescribeAnomalyDetectorsResponse'
            Prelude.<$> ( x Core..@? "AnomalyDetectors"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAnomalyDetectors

instance Prelude.NFData DescribeAnomalyDetectors

instance Core.ToHeaders DescribeAnomalyDetectors where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAnomalyDetectors where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAnomalyDetectors where
  toQuery DescribeAnomalyDetectors' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeAnomalyDetectors" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace,
        "NextToken" Core.=: nextToken,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> dimensions),
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeAnomalyDetectorsResponse' smart constructor.
data DescribeAnomalyDetectorsResponse = DescribeAnomalyDetectorsResponse'
  { -- | The list of anomaly detection models returned by the operation.
    anomalyDetectors :: Prelude.Maybe [AnomalyDetector],
    -- | A token that you can use in a subsequent operation to retrieve the next
    -- set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectors', 'describeAnomalyDetectorsResponse_anomalyDetectors' - The list of anomaly detection models returned by the operation.
--
-- 'nextToken', 'describeAnomalyDetectorsResponse_nextToken' - A token that you can use in a subsequent operation to retrieve the next
-- set of results.
--
-- 'httpStatus', 'describeAnomalyDetectorsResponse_httpStatus' - The response's http status code.
newDescribeAnomalyDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAnomalyDetectorsResponse
newDescribeAnomalyDetectorsResponse pHttpStatus_ =
  DescribeAnomalyDetectorsResponse'
    { anomalyDetectors =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of anomaly detection models returned by the operation.
describeAnomalyDetectorsResponse_anomalyDetectors :: Lens.Lens' DescribeAnomalyDetectorsResponse (Prelude.Maybe [AnomalyDetector])
describeAnomalyDetectorsResponse_anomalyDetectors = Lens.lens (\DescribeAnomalyDetectorsResponse' {anomalyDetectors} -> anomalyDetectors) (\s@DescribeAnomalyDetectorsResponse' {} a -> s {anomalyDetectors = a} :: DescribeAnomalyDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that you can use in a subsequent operation to retrieve the next
-- set of results.
describeAnomalyDetectorsResponse_nextToken :: Lens.Lens' DescribeAnomalyDetectorsResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectorsResponse_nextToken = Lens.lens (\DescribeAnomalyDetectorsResponse' {nextToken} -> nextToken) (\s@DescribeAnomalyDetectorsResponse' {} a -> s {nextToken = a} :: DescribeAnomalyDetectorsResponse)

-- | The response's http status code.
describeAnomalyDetectorsResponse_httpStatus :: Lens.Lens' DescribeAnomalyDetectorsResponse Prelude.Int
describeAnomalyDetectorsResponse_httpStatus = Lens.lens (\DescribeAnomalyDetectorsResponse' {httpStatus} -> httpStatus) (\s@DescribeAnomalyDetectorsResponse' {} a -> s {httpStatus = a} :: DescribeAnomalyDetectorsResponse)

instance
  Prelude.NFData
    DescribeAnomalyDetectorsResponse

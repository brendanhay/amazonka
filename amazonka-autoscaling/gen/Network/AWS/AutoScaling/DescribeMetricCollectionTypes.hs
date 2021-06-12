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
-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available CloudWatch metrics for Amazon EC2 Auto Scaling.
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must
-- explicitly request this metric when calling the EnableMetricsCollection
-- API.
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
  ( -- * Creating a Request
    DescribeMetricCollectionTypes (..),
    newDescribeMetricCollectionTypes,

    -- * Destructuring the Response
    DescribeMetricCollectionTypesResponse (..),
    newDescribeMetricCollectionTypesResponse,

    -- * Response Lenses
    describeMetricCollectionTypesResponse_metrics,
    describeMetricCollectionTypesResponse_granularities,
    describeMetricCollectionTypesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMetricCollectionTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeMetricCollectionTypes ::
  DescribeMetricCollectionTypes
newDescribeMetricCollectionTypes =
  DescribeMetricCollectionTypes'

instance
  Core.AWSRequest
    DescribeMetricCollectionTypes
  where
  type
    AWSResponse DescribeMetricCollectionTypes =
      DescribeMetricCollectionTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeMetricCollectionTypesResult"
      ( \s h x ->
          DescribeMetricCollectionTypesResponse'
            Core.<$> ( x Core..@? "Metrics" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "Granularities" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMetricCollectionTypes

instance Core.NFData DescribeMetricCollectionTypes

instance Core.ToHeaders DescribeMetricCollectionTypes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeMetricCollectionTypes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMetricCollectionTypes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeMetricCollectionTypes" :: Core.ByteString),
            "Version" Core.=: ("2011-01-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeMetricCollectionTypesResponse' smart constructor.
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
  { -- | One or more metrics.
    metrics :: Core.Maybe [MetricCollectionType],
    -- | The granularities for the metrics.
    granularities :: Core.Maybe [MetricGranularityType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMetricCollectionTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'describeMetricCollectionTypesResponse_metrics' - One or more metrics.
--
-- 'granularities', 'describeMetricCollectionTypesResponse_granularities' - The granularities for the metrics.
--
-- 'httpStatus', 'describeMetricCollectionTypesResponse_httpStatus' - The response's http status code.
newDescribeMetricCollectionTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMetricCollectionTypesResponse
newDescribeMetricCollectionTypesResponse pHttpStatus_ =
  DescribeMetricCollectionTypesResponse'
    { metrics =
        Core.Nothing,
      granularities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more metrics.
describeMetricCollectionTypesResponse_metrics :: Lens.Lens' DescribeMetricCollectionTypesResponse (Core.Maybe [MetricCollectionType])
describeMetricCollectionTypesResponse_metrics = Lens.lens (\DescribeMetricCollectionTypesResponse' {metrics} -> metrics) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {metrics = a} :: DescribeMetricCollectionTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The granularities for the metrics.
describeMetricCollectionTypesResponse_granularities :: Lens.Lens' DescribeMetricCollectionTypesResponse (Core.Maybe [MetricGranularityType])
describeMetricCollectionTypesResponse_granularities = Lens.lens (\DescribeMetricCollectionTypesResponse' {granularities} -> granularities) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {granularities = a} :: DescribeMetricCollectionTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMetricCollectionTypesResponse_httpStatus :: Lens.Lens' DescribeMetricCollectionTypesResponse Core.Int
describeMetricCollectionTypesResponse_httpStatus = Lens.lens (\DescribeMetricCollectionTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {httpStatus = a} :: DescribeMetricCollectionTypesResponse)

instance
  Core.NFData
    DescribeMetricCollectionTypesResponse

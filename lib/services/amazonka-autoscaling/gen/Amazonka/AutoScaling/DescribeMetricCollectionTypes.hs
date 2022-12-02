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
-- Module      : Amazonka.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available CloudWatch metrics for Amazon EC2 Auto Scaling.
module Amazonka.AutoScaling.DescribeMetricCollectionTypes
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeMetricCollectionTypesResult"
      ( \s h x ->
          DescribeMetricCollectionTypesResponse'
            Prelude.<$> ( x Data..@? "Metrics" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "Granularities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMetricCollectionTypes
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeMetricCollectionTypes where
  rnf _ = ()

instance Data.ToHeaders DescribeMetricCollectionTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeMetricCollectionTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMetricCollectionTypes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DescribeMetricCollectionTypes" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeMetricCollectionTypesResponse' smart constructor.
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
  { -- | The metrics.
    metrics :: Prelude.Maybe [MetricCollectionType],
    -- | The granularities for the metrics.
    granularities :: Prelude.Maybe [MetricGranularityType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricCollectionTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'describeMetricCollectionTypesResponse_metrics' - The metrics.
--
-- 'granularities', 'describeMetricCollectionTypesResponse_granularities' - The granularities for the metrics.
--
-- 'httpStatus', 'describeMetricCollectionTypesResponse_httpStatus' - The response's http status code.
newDescribeMetricCollectionTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMetricCollectionTypesResponse
newDescribeMetricCollectionTypesResponse pHttpStatus_ =
  DescribeMetricCollectionTypesResponse'
    { metrics =
        Prelude.Nothing,
      granularities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metrics.
describeMetricCollectionTypesResponse_metrics :: Lens.Lens' DescribeMetricCollectionTypesResponse (Prelude.Maybe [MetricCollectionType])
describeMetricCollectionTypesResponse_metrics = Lens.lens (\DescribeMetricCollectionTypesResponse' {metrics} -> metrics) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {metrics = a} :: DescribeMetricCollectionTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The granularities for the metrics.
describeMetricCollectionTypesResponse_granularities :: Lens.Lens' DescribeMetricCollectionTypesResponse (Prelude.Maybe [MetricGranularityType])
describeMetricCollectionTypesResponse_granularities = Lens.lens (\DescribeMetricCollectionTypesResponse' {granularities} -> granularities) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {granularities = a} :: DescribeMetricCollectionTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMetricCollectionTypesResponse_httpStatus :: Lens.Lens' DescribeMetricCollectionTypesResponse Prelude.Int
describeMetricCollectionTypesResponse_httpStatus = Lens.lens (\DescribeMetricCollectionTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricCollectionTypesResponse' {} a -> s {httpStatus = a} :: DescribeMetricCollectionTypesResponse)

instance
  Prelude.NFData
    DescribeMetricCollectionTypesResponse
  where
  rnf DescribeMetricCollectionTypesResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf granularities
      `Prelude.seq` Prelude.rnf httpStatus

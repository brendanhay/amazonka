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
-- Module      : Amazonka.Personalize.DescribeMetricAttribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a metric attribution.
module Amazonka.Personalize.DescribeMetricAttribution
  ( -- * Creating a Request
    DescribeMetricAttribution (..),
    newDescribeMetricAttribution,

    -- * Request Lenses
    describeMetricAttribution_metricAttributionArn,

    -- * Destructuring the Response
    DescribeMetricAttributionResponse (..),
    newDescribeMetricAttributionResponse,

    -- * Response Lenses
    describeMetricAttributionResponse_metricAttribution,
    describeMetricAttributionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMetricAttribution' smart constructor.
data DescribeMetricAttribution = DescribeMetricAttribution'
  { -- | The metric attribution\'s Amazon Resource Name (ARN).
    metricAttributionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttributionArn', 'describeMetricAttribution_metricAttributionArn' - The metric attribution\'s Amazon Resource Name (ARN).
newDescribeMetricAttribution ::
  -- | 'metricAttributionArn'
  Prelude.Text ->
  DescribeMetricAttribution
newDescribeMetricAttribution pMetricAttributionArn_ =
  DescribeMetricAttribution'
    { metricAttributionArn =
        pMetricAttributionArn_
    }

-- | The metric attribution\'s Amazon Resource Name (ARN).
describeMetricAttribution_metricAttributionArn :: Lens.Lens' DescribeMetricAttribution Prelude.Text
describeMetricAttribution_metricAttributionArn = Lens.lens (\DescribeMetricAttribution' {metricAttributionArn} -> metricAttributionArn) (\s@DescribeMetricAttribution' {} a -> s {metricAttributionArn = a} :: DescribeMetricAttribution)

instance Core.AWSRequest DescribeMetricAttribution where
  type
    AWSResponse DescribeMetricAttribution =
      DescribeMetricAttributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMetricAttributionResponse'
            Prelude.<$> (x Data..?> "metricAttribution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMetricAttribution where
  hashWithSalt _salt DescribeMetricAttribution' {..} =
    _salt `Prelude.hashWithSalt` metricAttributionArn

instance Prelude.NFData DescribeMetricAttribution where
  rnf DescribeMetricAttribution' {..} =
    Prelude.rnf metricAttributionArn

instance Data.ToHeaders DescribeMetricAttribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeMetricAttribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMetricAttribution where
  toJSON DescribeMetricAttribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "metricAttributionArn"
                  Data..= metricAttributionArn
              )
          ]
      )

instance Data.ToPath DescribeMetricAttribution where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMetricAttribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMetricAttributionResponse' smart constructor.
data DescribeMetricAttributionResponse = DescribeMetricAttributionResponse'
  { -- | The details of the metric attribution.
    metricAttribution :: Prelude.Maybe MetricAttribution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricAttributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttribution', 'describeMetricAttributionResponse_metricAttribution' - The details of the metric attribution.
--
-- 'httpStatus', 'describeMetricAttributionResponse_httpStatus' - The response's http status code.
newDescribeMetricAttributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMetricAttributionResponse
newDescribeMetricAttributionResponse pHttpStatus_ =
  DescribeMetricAttributionResponse'
    { metricAttribution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the metric attribution.
describeMetricAttributionResponse_metricAttribution :: Lens.Lens' DescribeMetricAttributionResponse (Prelude.Maybe MetricAttribution)
describeMetricAttributionResponse_metricAttribution = Lens.lens (\DescribeMetricAttributionResponse' {metricAttribution} -> metricAttribution) (\s@DescribeMetricAttributionResponse' {} a -> s {metricAttribution = a} :: DescribeMetricAttributionResponse)

-- | The response's http status code.
describeMetricAttributionResponse_httpStatus :: Lens.Lens' DescribeMetricAttributionResponse Prelude.Int
describeMetricAttributionResponse_httpStatus = Lens.lens (\DescribeMetricAttributionResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricAttributionResponse' {} a -> s {httpStatus = a} :: DescribeMetricAttributionResponse)

instance
  Prelude.NFData
    DescribeMetricAttributionResponse
  where
  rnf DescribeMetricAttributionResponse' {..} =
    Prelude.rnf metricAttribution
      `Prelude.seq` Prelude.rnf httpStatus

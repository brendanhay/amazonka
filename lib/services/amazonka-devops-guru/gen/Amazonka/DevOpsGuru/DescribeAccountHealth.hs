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
-- Module      : Amazonka.DevOpsGuru.DescribeAccountHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open reactive insights, the number of open
-- proactive insights, and the number of metrics analyzed in your AWS
-- account. Use these numbers to gauge the health of operations in your AWS
-- account.
module Amazonka.DevOpsGuru.DescribeAccountHealth
  ( -- * Creating a Request
    DescribeAccountHealth (..),
    newDescribeAccountHealth,

    -- * Destructuring the Response
    DescribeAccountHealthResponse (..),
    newDescribeAccountHealthResponse,

    -- * Response Lenses
    describeAccountHealthResponse_httpStatus,
    describeAccountHealthResponse_openReactiveInsights,
    describeAccountHealthResponse_openProactiveInsights,
    describeAccountHealthResponse_metricsAnalyzed,
    describeAccountHealthResponse_resourceHours,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountHealth' smart constructor.
data DescribeAccountHealth = DescribeAccountHealth'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountHealth ::
  DescribeAccountHealth
newDescribeAccountHealth = DescribeAccountHealth'

instance Core.AWSRequest DescribeAccountHealth where
  type
    AWSResponse DescribeAccountHealth =
      DescribeAccountHealthResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountHealthResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "OpenReactiveInsights")
            Prelude.<*> (x Core..:> "OpenProactiveInsights")
            Prelude.<*> (x Core..:> "MetricsAnalyzed")
            Prelude.<*> (x Core..:> "ResourceHours")
      )

instance Prelude.Hashable DescribeAccountHealth

instance Prelude.NFData DescribeAccountHealth

instance Core.ToHeaders DescribeAccountHealth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAccountHealth where
  toPath = Prelude.const "/accounts/health"

instance Core.ToQuery DescribeAccountHealth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountHealthResponse' smart constructor.
data DescribeAccountHealthResponse = DescribeAccountHealthResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An integer that specifies the number of open reactive insights in your
    -- AWS account.
    openReactiveInsights :: Prelude.Int,
    -- | An integer that specifies the number of open proactive insights in your
    -- AWS account.
    openProactiveInsights :: Prelude.Int,
    -- | An integer that specifies the number of metrics that have been analyzed
    -- in your AWS account.
    metricsAnalyzed :: Prelude.Int,
    -- | The number of Amazon DevOps Guru resource analysis hours billed to the
    -- current AWS account in the last hour.
    resourceHours :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAccountHealthResponse_httpStatus' - The response's http status code.
--
-- 'openReactiveInsights', 'describeAccountHealthResponse_openReactiveInsights' - An integer that specifies the number of open reactive insights in your
-- AWS account.
--
-- 'openProactiveInsights', 'describeAccountHealthResponse_openProactiveInsights' - An integer that specifies the number of open proactive insights in your
-- AWS account.
--
-- 'metricsAnalyzed', 'describeAccountHealthResponse_metricsAnalyzed' - An integer that specifies the number of metrics that have been analyzed
-- in your AWS account.
--
-- 'resourceHours', 'describeAccountHealthResponse_resourceHours' - The number of Amazon DevOps Guru resource analysis hours billed to the
-- current AWS account in the last hour.
newDescribeAccountHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'openReactiveInsights'
  Prelude.Int ->
  -- | 'openProactiveInsights'
  Prelude.Int ->
  -- | 'metricsAnalyzed'
  Prelude.Int ->
  -- | 'resourceHours'
  Prelude.Integer ->
  DescribeAccountHealthResponse
newDescribeAccountHealthResponse
  pHttpStatus_
  pOpenReactiveInsights_
  pOpenProactiveInsights_
  pMetricsAnalyzed_
  pResourceHours_ =
    DescribeAccountHealthResponse'
      { httpStatus =
          pHttpStatus_,
        openReactiveInsights =
          pOpenReactiveInsights_,
        openProactiveInsights =
          pOpenProactiveInsights_,
        metricsAnalyzed = pMetricsAnalyzed_,
        resourceHours = pResourceHours_
      }

-- | The response's http status code.
describeAccountHealthResponse_httpStatus :: Lens.Lens' DescribeAccountHealthResponse Prelude.Int
describeAccountHealthResponse_httpStatus = Lens.lens (\DescribeAccountHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountHealthResponse' {} a -> s {httpStatus = a} :: DescribeAccountHealthResponse)

-- | An integer that specifies the number of open reactive insights in your
-- AWS account.
describeAccountHealthResponse_openReactiveInsights :: Lens.Lens' DescribeAccountHealthResponse Prelude.Int
describeAccountHealthResponse_openReactiveInsights = Lens.lens (\DescribeAccountHealthResponse' {openReactiveInsights} -> openReactiveInsights) (\s@DescribeAccountHealthResponse' {} a -> s {openReactiveInsights = a} :: DescribeAccountHealthResponse)

-- | An integer that specifies the number of open proactive insights in your
-- AWS account.
describeAccountHealthResponse_openProactiveInsights :: Lens.Lens' DescribeAccountHealthResponse Prelude.Int
describeAccountHealthResponse_openProactiveInsights = Lens.lens (\DescribeAccountHealthResponse' {openProactiveInsights} -> openProactiveInsights) (\s@DescribeAccountHealthResponse' {} a -> s {openProactiveInsights = a} :: DescribeAccountHealthResponse)

-- | An integer that specifies the number of metrics that have been analyzed
-- in your AWS account.
describeAccountHealthResponse_metricsAnalyzed :: Lens.Lens' DescribeAccountHealthResponse Prelude.Int
describeAccountHealthResponse_metricsAnalyzed = Lens.lens (\DescribeAccountHealthResponse' {metricsAnalyzed} -> metricsAnalyzed) (\s@DescribeAccountHealthResponse' {} a -> s {metricsAnalyzed = a} :: DescribeAccountHealthResponse)

-- | The number of Amazon DevOps Guru resource analysis hours billed to the
-- current AWS account in the last hour.
describeAccountHealthResponse_resourceHours :: Lens.Lens' DescribeAccountHealthResponse Prelude.Integer
describeAccountHealthResponse_resourceHours = Lens.lens (\DescribeAccountHealthResponse' {resourceHours} -> resourceHours) (\s@DescribeAccountHealthResponse' {} a -> s {resourceHours = a} :: DescribeAccountHealthResponse)

instance Prelude.NFData DescribeAccountHealthResponse

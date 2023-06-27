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
-- Module      : Amazonka.DevOpsGuru.DescribeOrganizationHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns active insights, predictive insights, and resource hours
-- analyzed in last hour.
module Amazonka.DevOpsGuru.DescribeOrganizationHealth
  ( -- * Creating a Request
    DescribeOrganizationHealth (..),
    newDescribeOrganizationHealth,

    -- * Request Lenses
    describeOrganizationHealth_accountIds,
    describeOrganizationHealth_organizationalUnitIds,

    -- * Destructuring the Response
    DescribeOrganizationHealthResponse (..),
    newDescribeOrganizationHealthResponse,

    -- * Response Lenses
    describeOrganizationHealthResponse_httpStatus,
    describeOrganizationHealthResponse_openReactiveInsights,
    describeOrganizationHealthResponse_openProactiveInsights,
    describeOrganizationHealthResponse_metricsAnalyzed,
    describeOrganizationHealthResponse_resourceHours,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationHealth' smart constructor.
data DescribeOrganizationHealth = DescribeOrganizationHealth'
  { -- | The ID of the Amazon Web Services account.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the organizational unit.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'describeOrganizationHealth_accountIds' - The ID of the Amazon Web Services account.
--
-- 'organizationalUnitIds', 'describeOrganizationHealth_organizationalUnitIds' - The ID of the organizational unit.
newDescribeOrganizationHealth ::
  DescribeOrganizationHealth
newDescribeOrganizationHealth =
  DescribeOrganizationHealth'
    { accountIds =
        Prelude.Nothing,
      organizationalUnitIds = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account.
describeOrganizationHealth_accountIds :: Lens.Lens' DescribeOrganizationHealth (Prelude.Maybe [Prelude.Text])
describeOrganizationHealth_accountIds = Lens.lens (\DescribeOrganizationHealth' {accountIds} -> accountIds) (\s@DescribeOrganizationHealth' {} a -> s {accountIds = a} :: DescribeOrganizationHealth) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the organizational unit.
describeOrganizationHealth_organizationalUnitIds :: Lens.Lens' DescribeOrganizationHealth (Prelude.Maybe [Prelude.Text])
describeOrganizationHealth_organizationalUnitIds = Lens.lens (\DescribeOrganizationHealth' {organizationalUnitIds} -> organizationalUnitIds) (\s@DescribeOrganizationHealth' {} a -> s {organizationalUnitIds = a} :: DescribeOrganizationHealth) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeOrganizationHealth where
  type
    AWSResponse DescribeOrganizationHealth =
      DescribeOrganizationHealthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationHealthResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "OpenReactiveInsights")
            Prelude.<*> (x Data..:> "OpenProactiveInsights")
            Prelude.<*> (x Data..:> "MetricsAnalyzed")
            Prelude.<*> (x Data..:> "ResourceHours")
      )

instance Prelude.Hashable DescribeOrganizationHealth where
  hashWithSalt _salt DescribeOrganizationHealth' {..} =
    _salt
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` organizationalUnitIds

instance Prelude.NFData DescribeOrganizationHealth where
  rnf DescribeOrganizationHealth' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf organizationalUnitIds

instance Data.ToHeaders DescribeOrganizationHealth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOrganizationHealth where
  toJSON DescribeOrganizationHealth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountIds" Data..=) Prelude.<$> accountIds,
            ("OrganizationalUnitIds" Data..=)
              Prelude.<$> organizationalUnitIds
          ]
      )

instance Data.ToPath DescribeOrganizationHealth where
  toPath = Prelude.const "/organization/health"

instance Data.ToQuery DescribeOrganizationHealth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationHealthResponse' smart constructor.
data DescribeOrganizationHealthResponse = DescribeOrganizationHealthResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An integer that specifies the number of open reactive insights in your
    -- Amazon Web Services account.
    openReactiveInsights :: Prelude.Int,
    -- | An integer that specifies the number of open proactive insights in your
    -- Amazon Web Services account.
    openProactiveInsights :: Prelude.Int,
    -- | An integer that specifies the number of metrics that have been analyzed
    -- in your organization.
    metricsAnalyzed :: Prelude.Int,
    -- | The number of Amazon DevOps Guru resource analysis hours billed to the
    -- current Amazon Web Services account in the last hour.
    resourceHours :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeOrganizationHealthResponse_httpStatus' - The response's http status code.
--
-- 'openReactiveInsights', 'describeOrganizationHealthResponse_openReactiveInsights' - An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
--
-- 'openProactiveInsights', 'describeOrganizationHealthResponse_openProactiveInsights' - An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
--
-- 'metricsAnalyzed', 'describeOrganizationHealthResponse_metricsAnalyzed' - An integer that specifies the number of metrics that have been analyzed
-- in your organization.
--
-- 'resourceHours', 'describeOrganizationHealthResponse_resourceHours' - The number of Amazon DevOps Guru resource analysis hours billed to the
-- current Amazon Web Services account in the last hour.
newDescribeOrganizationHealthResponse ::
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
  DescribeOrganizationHealthResponse
newDescribeOrganizationHealthResponse
  pHttpStatus_
  pOpenReactiveInsights_
  pOpenProactiveInsights_
  pMetricsAnalyzed_
  pResourceHours_ =
    DescribeOrganizationHealthResponse'
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
describeOrganizationHealthResponse_httpStatus :: Lens.Lens' DescribeOrganizationHealthResponse Prelude.Int
describeOrganizationHealthResponse_httpStatus = Lens.lens (\DescribeOrganizationHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationHealthResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationHealthResponse)

-- | An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
describeOrganizationHealthResponse_openReactiveInsights :: Lens.Lens' DescribeOrganizationHealthResponse Prelude.Int
describeOrganizationHealthResponse_openReactiveInsights = Lens.lens (\DescribeOrganizationHealthResponse' {openReactiveInsights} -> openReactiveInsights) (\s@DescribeOrganizationHealthResponse' {} a -> s {openReactiveInsights = a} :: DescribeOrganizationHealthResponse)

-- | An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
describeOrganizationHealthResponse_openProactiveInsights :: Lens.Lens' DescribeOrganizationHealthResponse Prelude.Int
describeOrganizationHealthResponse_openProactiveInsights = Lens.lens (\DescribeOrganizationHealthResponse' {openProactiveInsights} -> openProactiveInsights) (\s@DescribeOrganizationHealthResponse' {} a -> s {openProactiveInsights = a} :: DescribeOrganizationHealthResponse)

-- | An integer that specifies the number of metrics that have been analyzed
-- in your organization.
describeOrganizationHealthResponse_metricsAnalyzed :: Lens.Lens' DescribeOrganizationHealthResponse Prelude.Int
describeOrganizationHealthResponse_metricsAnalyzed = Lens.lens (\DescribeOrganizationHealthResponse' {metricsAnalyzed} -> metricsAnalyzed) (\s@DescribeOrganizationHealthResponse' {} a -> s {metricsAnalyzed = a} :: DescribeOrganizationHealthResponse)

-- | The number of Amazon DevOps Guru resource analysis hours billed to the
-- current Amazon Web Services account in the last hour.
describeOrganizationHealthResponse_resourceHours :: Lens.Lens' DescribeOrganizationHealthResponse Prelude.Integer
describeOrganizationHealthResponse_resourceHours = Lens.lens (\DescribeOrganizationHealthResponse' {resourceHours} -> resourceHours) (\s@DescribeOrganizationHealthResponse' {} a -> s {resourceHours = a} :: DescribeOrganizationHealthResponse)

instance
  Prelude.NFData
    DescribeOrganizationHealthResponse
  where
  rnf DescribeOrganizationHealthResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf openReactiveInsights
      `Prelude.seq` Prelude.rnf openProactiveInsights
      `Prelude.seq` Prelude.rnf metricsAnalyzed
      `Prelude.seq` Prelude.rnf resourceHours

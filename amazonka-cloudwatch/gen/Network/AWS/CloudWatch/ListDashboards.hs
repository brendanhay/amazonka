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
-- Module      : Network.AWS.CloudWatch.ListDashboards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the dashboards for your account. If you include
-- @DashboardNamePrefix@, only those dashboards with names starting with
-- the prefix are listed. Otherwise, all dashboards in your account are
-- listed.
--
-- @ListDashboards@ returns up to 1000 results on one page. If there are
-- more than 1000 dashboards, you can call @ListDashboards@ again and
-- include the value you received for @NextToken@ in the first call, to
-- receive the next 1000 results.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListDashboards
  ( -- * Creating a Request
    ListDashboards (..),
    newListDashboards,

    -- * Request Lenses
    listDashboards_nextToken,
    listDashboards_dashboardNamePrefix,

    -- * Destructuring the Response
    ListDashboardsResponse (..),
    newListDashboardsResponse,

    -- * Response Lenses
    listDashboardsResponse_nextToken,
    listDashboardsResponse_dashboardEntries,
    listDashboardsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Core.Maybe Core.Text,
    -- | If you specify this parameter, only the dashboards with names starting
    -- with the specified string are listed. The maximum length is 255, and
    -- valid characters are A-Z, a-z, 0-9, \".\", \"-\", and \"_\".
    dashboardNamePrefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDashboards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDashboards_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'dashboardNamePrefix', 'listDashboards_dashboardNamePrefix' - If you specify this parameter, only the dashboards with names starting
-- with the specified string are listed. The maximum length is 255, and
-- valid characters are A-Z, a-z, 0-9, \".\", \"-\", and \"_\".
newListDashboards ::
  ListDashboards
newListDashboards =
  ListDashboards'
    { nextToken = Core.Nothing,
      dashboardNamePrefix = Core.Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
listDashboards_nextToken :: Lens.Lens' ListDashboards (Core.Maybe Core.Text)
listDashboards_nextToken = Lens.lens (\ListDashboards' {nextToken} -> nextToken) (\s@ListDashboards' {} a -> s {nextToken = a} :: ListDashboards)

-- | If you specify this parameter, only the dashboards with names starting
-- with the specified string are listed. The maximum length is 255, and
-- valid characters are A-Z, a-z, 0-9, \".\", \"-\", and \"_\".
listDashboards_dashboardNamePrefix :: Lens.Lens' ListDashboards (Core.Maybe Core.Text)
listDashboards_dashboardNamePrefix = Lens.lens (\ListDashboards' {dashboardNamePrefix} -> dashboardNamePrefix) (\s@ListDashboards' {} a -> s {dashboardNamePrefix = a} :: ListDashboards)

instance Core.AWSPager ListDashboards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDashboardsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDashboardsResponse_dashboardEntries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDashboards_nextToken
          Lens..~ rs
          Lens.^? listDashboardsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDashboards where
  type
    AWSResponse ListDashboards =
      ListDashboardsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListDashboardsResult"
      ( \s h x ->
          ListDashboardsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "DashboardEntries" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDashboards

instance Core.NFData ListDashboards

instance Core.ToHeaders ListDashboards where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDashboards where
  toPath = Core.const "/"

instance Core.ToQuery ListDashboards where
  toQuery ListDashboards' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListDashboards" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DashboardNamePrefix" Core.=: dashboardNamePrefix
      ]

-- | /See:/ 'newListDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of matching dashboards.
    dashboardEntries :: Core.Maybe [DashboardEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDashboardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDashboardsResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'dashboardEntries', 'listDashboardsResponse_dashboardEntries' - The list of matching dashboards.
--
-- 'httpStatus', 'listDashboardsResponse_httpStatus' - The response's http status code.
newListDashboardsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDashboardsResponse
newListDashboardsResponse pHttpStatus_ =
  ListDashboardsResponse'
    { nextToken = Core.Nothing,
      dashboardEntries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that marks the start of the next batch of returned results.
listDashboardsResponse_nextToken :: Lens.Lens' ListDashboardsResponse (Core.Maybe Core.Text)
listDashboardsResponse_nextToken = Lens.lens (\ListDashboardsResponse' {nextToken} -> nextToken) (\s@ListDashboardsResponse' {} a -> s {nextToken = a} :: ListDashboardsResponse)

-- | The list of matching dashboards.
listDashboardsResponse_dashboardEntries :: Lens.Lens' ListDashboardsResponse (Core.Maybe [DashboardEntry])
listDashboardsResponse_dashboardEntries = Lens.lens (\ListDashboardsResponse' {dashboardEntries} -> dashboardEntries) (\s@ListDashboardsResponse' {} a -> s {dashboardEntries = a} :: ListDashboardsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDashboardsResponse_httpStatus :: Lens.Lens' ListDashboardsResponse Core.Int
listDashboardsResponse_httpStatus = Lens.lens (\ListDashboardsResponse' {httpStatus} -> httpStatus) (\s@ListDashboardsResponse' {} a -> s {httpStatus = a} :: ListDashboardsResponse)

instance Core.NFData ListDashboardsResponse

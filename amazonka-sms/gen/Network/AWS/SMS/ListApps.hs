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
-- Module      : Network.AWS.SMS.ListApps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summaries for all applications.
--
-- This operation returns paginated results.
module Network.AWS.SMS.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_appIds,
    listApps_nextToken,
    listApps_maxResults,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_nextToken,
    listAppsResponse_apps,
    listAppsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | The unique application IDs.
    appIds :: Core.Maybe [Core.Text],
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 100. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIds', 'listApps_appIds' - The unique application IDs.
--
-- 'nextToken', 'listApps_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listApps_maxResults' - The maximum number of results to return in a single call. The default
-- value is 100. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { appIds = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The unique application IDs.
listApps_appIds :: Lens.Lens' ListApps (Core.Maybe [Core.Text])
listApps_appIds = Lens.lens (\ListApps' {appIds} -> appIds) (\s@ListApps' {} a -> s {appIds = a} :: ListApps) Core.. Lens.mapping Lens._Coerce

-- | The token for the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Core.Maybe Core.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | The maximum number of results to return in a single call. The default
-- value is 100. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
listApps_maxResults :: Lens.Lens' ListApps (Core.Maybe Core.Int)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

instance Core.AWSPager ListApps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listAppsResponse_apps Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApps_nextToken
          Lens..~ rs
          Lens.^? listAppsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "apps" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApps

instance Core.NFData ListApps

instance Core.ToHeaders ListApps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.ListApps" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApps where
  toJSON ListApps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appIds" Core..=) Core.<$> appIds,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListApps where
  toPath = Core.const "/"

instance Core.ToQuery ListApps where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The application summaries.
    apps :: Core.Maybe [AppSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'apps', 'listAppsResponse_apps' - The application summaries.
--
-- 'httpStatus', 'listAppsResponse_httpStatus' - The response's http status code.
newListAppsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAppsResponse
newListAppsResponse pHttpStatus_ =
  ListAppsResponse'
    { nextToken = Core.Nothing,
      apps = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Core.Maybe Core.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The application summaries.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse (Core.Maybe [AppSummary])
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Core.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

instance Core.NFData ListAppsResponse

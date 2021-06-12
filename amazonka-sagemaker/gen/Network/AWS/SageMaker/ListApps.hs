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
-- Module      : Network.AWS.SageMaker.ListApps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_sortOrder,
    listApps_nextToken,
    listApps_maxResults,
    listApps_domainIdEquals,
    listApps_userProfileNameEquals,
    listApps_sortBy,

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
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | A parameter to search for the domain ID.
    domainIdEquals :: Core.Maybe Core.Text,
    -- | A parameter to search by user profile name.
    userProfileNameEquals :: Core.Maybe Core.Text,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Core.Maybe AppSortKey
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
-- 'sortOrder', 'listApps_sortOrder' - The sort order for the results. The default is Ascending.
--
-- 'nextToken', 'listApps_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'maxResults', 'listApps_maxResults' - Returns a list up to a specified limit.
--
-- 'domainIdEquals', 'listApps_domainIdEquals' - A parameter to search for the domain ID.
--
-- 'userProfileNameEquals', 'listApps_userProfileNameEquals' - A parameter to search by user profile name.
--
-- 'sortBy', 'listApps_sortBy' - The parameter by which to sort the results. The default is CreationTime.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      domainIdEquals = Core.Nothing,
      userProfileNameEquals = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | The sort order for the results. The default is Ascending.
listApps_sortOrder :: Lens.Lens' ListApps (Core.Maybe SortOrder)
listApps_sortOrder = Lens.lens (\ListApps' {sortOrder} -> sortOrder) (\s@ListApps' {} a -> s {sortOrder = a} :: ListApps)

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Core.Maybe Core.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | Returns a list up to a specified limit.
listApps_maxResults :: Lens.Lens' ListApps (Core.Maybe Core.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | A parameter to search for the domain ID.
listApps_domainIdEquals :: Lens.Lens' ListApps (Core.Maybe Core.Text)
listApps_domainIdEquals = Lens.lens (\ListApps' {domainIdEquals} -> domainIdEquals) (\s@ListApps' {} a -> s {domainIdEquals = a} :: ListApps)

-- | A parameter to search by user profile name.
listApps_userProfileNameEquals :: Lens.Lens' ListApps (Core.Maybe Core.Text)
listApps_userProfileNameEquals = Lens.lens (\ListApps' {userProfileNameEquals} -> userProfileNameEquals) (\s@ListApps' {} a -> s {userProfileNameEquals = a} :: ListApps)

-- | The parameter by which to sort the results. The default is CreationTime.
listApps_sortBy :: Lens.Lens' ListApps (Core.Maybe AppSortKey)
listApps_sortBy = Lens.lens (\ListApps' {sortBy} -> sortBy) (\s@ListApps' {} a -> s {sortBy = a} :: ListApps)

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Apps" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApps

instance Core.NFData ListApps

instance Core.ToHeaders ListApps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListApps" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApps where
  toJSON ListApps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DomainIdEquals" Core..=) Core.<$> domainIdEquals,
            ("UserProfileNameEquals" Core..=)
              Core.<$> userProfileNameEquals,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListApps where
  toPath = Core.const "/"

instance Core.ToQuery ListApps where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of apps.
    apps :: Core.Maybe [AppDetails],
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
-- 'nextToken', 'listAppsResponse_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'apps', 'listAppsResponse_apps' - The list of apps.
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

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Core.Maybe Core.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The list of apps.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse (Core.Maybe [AppDetails])
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Core.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

instance Core.NFData ListAppsResponse

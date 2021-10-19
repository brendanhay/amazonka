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
    listApps_domainIdEquals,
    listApps_nextToken,
    listApps_sortOrder,
    listApps_userProfileNameEquals,
    listApps_maxResults,
    listApps_sortBy,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | A parameter to search for the domain ID.
    domainIdEquals :: Prelude.Maybe Prelude.Text,
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A parameter to search by user profile name.
    userProfileNameEquals :: Prelude.Maybe Prelude.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Prelude.Maybe AppSortKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainIdEquals', 'listApps_domainIdEquals' - A parameter to search for the domain ID.
--
-- 'nextToken', 'listApps_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'sortOrder', 'listApps_sortOrder' - The sort order for the results. The default is Ascending.
--
-- 'userProfileNameEquals', 'listApps_userProfileNameEquals' - A parameter to search by user profile name.
--
-- 'maxResults', 'listApps_maxResults' - Returns a list up to a specified limit.
--
-- 'sortBy', 'listApps_sortBy' - The parameter by which to sort the results. The default is CreationTime.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { domainIdEquals = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      userProfileNameEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A parameter to search for the domain ID.
listApps_domainIdEquals :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_domainIdEquals = Lens.lens (\ListApps' {domainIdEquals} -> domainIdEquals) (\s@ListApps' {} a -> s {domainIdEquals = a} :: ListApps)

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | The sort order for the results. The default is Ascending.
listApps_sortOrder :: Lens.Lens' ListApps (Prelude.Maybe SortOrder)
listApps_sortOrder = Lens.lens (\ListApps' {sortOrder} -> sortOrder) (\s@ListApps' {} a -> s {sortOrder = a} :: ListApps)

-- | A parameter to search by user profile name.
listApps_userProfileNameEquals :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_userProfileNameEquals = Lens.lens (\ListApps' {userProfileNameEquals} -> userProfileNameEquals) (\s@ListApps' {} a -> s {userProfileNameEquals = a} :: ListApps)

-- | Returns a list up to a specified limit.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | The parameter by which to sort the results. The default is CreationTime.
listApps_sortBy :: Lens.Lens' ListApps (Prelude.Maybe AppSortKey)
listApps_sortBy = Lens.lens (\ListApps' {sortBy} -> sortBy) (\s@ListApps' {} a -> s {sortBy = a} :: ListApps)

instance Core.AWSPager ListApps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_apps Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApps_nextToken
          Lens..~ rs
          Lens.^? listAppsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Core..?> "Apps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApps

instance Prelude.NFData ListApps

instance Core.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListApps" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListApps where
  toJSON ListApps' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DomainIdEquals" Core..=)
              Prelude.<$> domainIdEquals,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("UserProfileNameEquals" Core..=)
              Prelude.<$> userProfileNameEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListApps where
  toPath = Prelude.const "/"

instance Core.ToQuery ListApps where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The list of apps.
    apps :: Prelude.Maybe [AppDetails],
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apps', 'listAppsResponse_apps' - The list of apps.
--
-- 'nextToken', 'listAppsResponse_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'httpStatus', 'listAppsResponse_httpStatus' - The response's http status code.
newListAppsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppsResponse
newListAppsResponse pHttpStatus_ =
  ListAppsResponse'
    { apps = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of apps.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse (Prelude.Maybe [AppDetails])
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Prelude.Maybe Prelude.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Prelude.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

instance Prelude.NFData ListAppsResponse

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
-- Module      : Amazonka.SageMaker.ListApps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_domainIdEquals,
    listApps_maxResults,
    listApps_nextToken,
    listApps_sortBy,
    listApps_sortOrder,
    listApps_spaceNameEquals,
    listApps_userProfileNameEquals,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | A parameter to search for the domain ID.
    domainIdEquals :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return in the response. If the total number
    -- of items available is more than the value specified, a @NextToken@ is
    -- provided in the response. To resume pagination, provide the @NextToken@
    -- value in the as part of a subsequent call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Prelude.Maybe AppSortKey,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A parameter to search by space name. If @UserProfileNameEquals@ is set,
    -- then this value cannot be set.
    spaceNameEquals :: Prelude.Maybe Prelude.Text,
    -- | A parameter to search by user profile name. If @SpaceNameEquals@ is set,
    -- then this value cannot be set.
    userProfileNameEquals :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listApps_maxResults' - The total number of items to return in the response. If the total number
-- of items available is more than the value specified, a @NextToken@ is
-- provided in the response. To resume pagination, provide the @NextToken@
-- value in the as part of a subsequent call. The default value is 10.
--
-- 'nextToken', 'listApps_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'sortBy', 'listApps_sortBy' - The parameter by which to sort the results. The default is CreationTime.
--
-- 'sortOrder', 'listApps_sortOrder' - The sort order for the results. The default is Ascending.
--
-- 'spaceNameEquals', 'listApps_spaceNameEquals' - A parameter to search by space name. If @UserProfileNameEquals@ is set,
-- then this value cannot be set.
--
-- 'userProfileNameEquals', 'listApps_userProfileNameEquals' - A parameter to search by user profile name. If @SpaceNameEquals@ is set,
-- then this value cannot be set.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { domainIdEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      spaceNameEquals = Prelude.Nothing,
      userProfileNameEquals = Prelude.Nothing
    }

-- | A parameter to search for the domain ID.
listApps_domainIdEquals :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_domainIdEquals = Lens.lens (\ListApps' {domainIdEquals} -> domainIdEquals) (\s@ListApps' {} a -> s {domainIdEquals = a} :: ListApps)

-- | The total number of items to return in the response. If the total number
-- of items available is more than the value specified, a @NextToken@ is
-- provided in the response. To resume pagination, provide the @NextToken@
-- value in the as part of a subsequent call. The default value is 10.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | The parameter by which to sort the results. The default is CreationTime.
listApps_sortBy :: Lens.Lens' ListApps (Prelude.Maybe AppSortKey)
listApps_sortBy = Lens.lens (\ListApps' {sortBy} -> sortBy) (\s@ListApps' {} a -> s {sortBy = a} :: ListApps)

-- | The sort order for the results. The default is Ascending.
listApps_sortOrder :: Lens.Lens' ListApps (Prelude.Maybe SortOrder)
listApps_sortOrder = Lens.lens (\ListApps' {sortOrder} -> sortOrder) (\s@ListApps' {} a -> s {sortOrder = a} :: ListApps)

-- | A parameter to search by space name. If @UserProfileNameEquals@ is set,
-- then this value cannot be set.
listApps_spaceNameEquals :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_spaceNameEquals = Lens.lens (\ListApps' {spaceNameEquals} -> spaceNameEquals) (\s@ListApps' {} a -> s {spaceNameEquals = a} :: ListApps)

-- | A parameter to search by user profile name. If @SpaceNameEquals@ is set,
-- then this value cannot be set.
listApps_userProfileNameEquals :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_userProfileNameEquals = Lens.lens (\ListApps' {userProfileNameEquals} -> userProfileNameEquals) (\s@ListApps' {} a -> s {userProfileNameEquals = a} :: ListApps)

instance Core.AWSPager ListApps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_apps
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApps_nextToken
          Lens..~ rs
          Lens.^? listAppsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Data..?> "Apps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApps where
  hashWithSalt _salt ListApps' {..} =
    _salt
      `Prelude.hashWithSalt` domainIdEquals
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` spaceNameEquals
      `Prelude.hashWithSalt` userProfileNameEquals

instance Prelude.NFData ListApps where
  rnf ListApps' {..} =
    Prelude.rnf domainIdEquals
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf spaceNameEquals
      `Prelude.seq` Prelude.rnf userProfileNameEquals

instance Data.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListApps" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApps where
  toJSON ListApps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DomainIdEquals" Data..=)
              Prelude.<$> domainIdEquals,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SpaceNameEquals" Data..=)
              Prelude.<$> spaceNameEquals,
            ("UserProfileNameEquals" Data..=)
              Prelude.<$> userProfileNameEquals
          ]
      )

instance Data.ToPath ListApps where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApps where
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

instance Prelude.NFData ListAppsResponse where
  rnf ListAppsResponse' {..} =
    Prelude.rnf apps
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

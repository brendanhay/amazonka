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
-- Module      : Network.AWS.SageMaker.ListUserProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists user profiles.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListUserProfiles
  ( -- * Creating a Request
    ListUserProfiles (..),
    newListUserProfiles,

    -- * Request Lenses
    listUserProfiles_sortOrder,
    listUserProfiles_nextToken,
    listUserProfiles_userProfileNameContains,
    listUserProfiles_maxResults,
    listUserProfiles_domainIdEquals,
    listUserProfiles_sortBy,

    -- * Destructuring the Response
    ListUserProfilesResponse (..),
    newListUserProfilesResponse,

    -- * Response Lenses
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_userProfiles,
    listUserProfilesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A parameter by which to filter the results.
    userProfileNameContains :: Core.Maybe Core.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | A parameter by which to filter the results.
    domainIdEquals :: Core.Maybe Core.Text,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Core.Maybe UserProfileSortKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listUserProfiles_sortOrder' - The sort order for the results. The default is Ascending.
--
-- 'nextToken', 'listUserProfiles_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'userProfileNameContains', 'listUserProfiles_userProfileNameContains' - A parameter by which to filter the results.
--
-- 'maxResults', 'listUserProfiles_maxResults' - Returns a list up to a specified limit.
--
-- 'domainIdEquals', 'listUserProfiles_domainIdEquals' - A parameter by which to filter the results.
--
-- 'sortBy', 'listUserProfiles_sortBy' - The parameter by which to sort the results. The default is CreationTime.
newListUserProfiles ::
  ListUserProfiles
newListUserProfiles =
  ListUserProfiles'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      userProfileNameContains = Core.Nothing,
      maxResults = Core.Nothing,
      domainIdEquals = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | The sort order for the results. The default is Ascending.
listUserProfiles_sortOrder :: Lens.Lens' ListUserProfiles (Core.Maybe SortOrder)
listUserProfiles_sortOrder = Lens.lens (\ListUserProfiles' {sortOrder} -> sortOrder) (\s@ListUserProfiles' {} a -> s {sortOrder = a} :: ListUserProfiles)

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listUserProfiles_nextToken :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Text)
listUserProfiles_nextToken = Lens.lens (\ListUserProfiles' {nextToken} -> nextToken) (\s@ListUserProfiles' {} a -> s {nextToken = a} :: ListUserProfiles)

-- | A parameter by which to filter the results.
listUserProfiles_userProfileNameContains :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Text)
listUserProfiles_userProfileNameContains = Lens.lens (\ListUserProfiles' {userProfileNameContains} -> userProfileNameContains) (\s@ListUserProfiles' {} a -> s {userProfileNameContains = a} :: ListUserProfiles)

-- | Returns a list up to a specified limit.
listUserProfiles_maxResults :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Natural)
listUserProfiles_maxResults = Lens.lens (\ListUserProfiles' {maxResults} -> maxResults) (\s@ListUserProfiles' {} a -> s {maxResults = a} :: ListUserProfiles)

-- | A parameter by which to filter the results.
listUserProfiles_domainIdEquals :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Text)
listUserProfiles_domainIdEquals = Lens.lens (\ListUserProfiles' {domainIdEquals} -> domainIdEquals) (\s@ListUserProfiles' {} a -> s {domainIdEquals = a} :: ListUserProfiles)

-- | The parameter by which to sort the results. The default is CreationTime.
listUserProfiles_sortBy :: Lens.Lens' ListUserProfiles (Core.Maybe UserProfileSortKey)
listUserProfiles_sortBy = Lens.lens (\ListUserProfiles' {sortBy} -> sortBy) (\s@ListUserProfiles' {} a -> s {sortBy = a} :: ListUserProfiles)

instance Core.AWSPager ListUserProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserProfilesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserProfilesResponse_userProfiles
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUserProfiles_nextToken
          Lens..~ rs
          Lens.^? listUserProfilesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUserProfiles where
  type
    AWSResponse ListUserProfiles =
      ListUserProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserProfilesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "UserProfiles" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUserProfiles

instance Core.NFData ListUserProfiles

instance Core.ToHeaders ListUserProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListUserProfiles" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUserProfiles where
  toJSON ListUserProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("UserProfileNameContains" Core..=)
              Core.<$> userProfileNameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DomainIdEquals" Core..=) Core.<$> domainIdEquals,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListUserProfiles where
  toPath = Core.const "/"

instance Core.ToQuery ListUserProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of user profiles.
    userProfiles :: Core.Maybe [UserProfileDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserProfilesResponse_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'userProfiles', 'listUserProfilesResponse_userProfiles' - The list of user profiles.
--
-- 'httpStatus', 'listUserProfilesResponse_httpStatus' - The response's http status code.
newListUserProfilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUserProfilesResponse
newListUserProfilesResponse pHttpStatus_ =
  ListUserProfilesResponse'
    { nextToken = Core.Nothing,
      userProfiles = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listUserProfilesResponse_nextToken :: Lens.Lens' ListUserProfilesResponse (Core.Maybe Core.Text)
listUserProfilesResponse_nextToken = Lens.lens (\ListUserProfilesResponse' {nextToken} -> nextToken) (\s@ListUserProfilesResponse' {} a -> s {nextToken = a} :: ListUserProfilesResponse)

-- | The list of user profiles.
listUserProfilesResponse_userProfiles :: Lens.Lens' ListUserProfilesResponse (Core.Maybe [UserProfileDetails])
listUserProfilesResponse_userProfiles = Lens.lens (\ListUserProfilesResponse' {userProfiles} -> userProfiles) (\s@ListUserProfilesResponse' {} a -> s {userProfiles = a} :: ListUserProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUserProfilesResponse_httpStatus :: Lens.Lens' ListUserProfilesResponse Core.Int
listUserProfilesResponse_httpStatus = Lens.lens (\ListUserProfilesResponse' {httpStatus} -> httpStatus) (\s@ListUserProfilesResponse' {} a -> s {httpStatus = a} :: ListUserProfilesResponse)

instance Core.NFData ListUserProfilesResponse

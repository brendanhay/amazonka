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
-- Module      : Network.AWS.CodeStar.ListUserProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the user profiles configured for your AWS account in AWS
-- CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListUserProfiles
  ( -- * Creating a Request
    ListUserProfiles (..),
    newListUserProfiles,

    -- * Request Lenses
    listUserProfiles_nextToken,
    listUserProfiles_maxResults,

    -- * Destructuring the Response
    ListUserProfilesResponse (..),
    newListUserProfilesResponse,

    -- * Response Lenses
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_httpStatus,
    listUserProfilesResponse_userProfiles,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { -- | The continuation token for the next set of results, if the results
    -- cannot be returned in one response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a response.
    maxResults :: Core.Maybe Core.Natural
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
-- 'nextToken', 'listUserProfiles_nextToken' - The continuation token for the next set of results, if the results
-- cannot be returned in one response.
--
-- 'maxResults', 'listUserProfiles_maxResults' - The maximum number of results to return in a response.
newListUserProfiles ::
  ListUserProfiles
newListUserProfiles =
  ListUserProfiles'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The continuation token for the next set of results, if the results
-- cannot be returned in one response.
listUserProfiles_nextToken :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Text)
listUserProfiles_nextToken = Lens.lens (\ListUserProfiles' {nextToken} -> nextToken) (\s@ListUserProfiles' {} a -> s {nextToken = a} :: ListUserProfiles)

-- | The maximum number of results to return in a response.
listUserProfiles_maxResults :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Natural)
listUserProfiles_maxResults = Lens.lens (\ListUserProfiles' {maxResults} -> maxResults) (\s@ListUserProfiles' {} a -> s {maxResults = a} :: ListUserProfiles)

instance Core.AWSPager ListUserProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserProfilesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listUserProfilesResponse_userProfiles) =
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
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "userProfiles" Core..!@ Core.mempty)
      )

instance Core.Hashable ListUserProfiles

instance Core.NFData ListUserProfiles

instance Core.ToHeaders ListUserProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListUserProfiles" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUserProfiles where
  toJSON ListUserProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListUserProfiles where
  toPath = Core.const "/"

instance Core.ToQuery ListUserProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { -- | The continuation token to use when requesting the next set of results,
    -- if there are more results to be returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | All the user profiles configured in AWS CodeStar for an AWS account.
    userProfiles :: [UserProfileSummary]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserProfilesResponse_nextToken' - The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
--
-- 'httpStatus', 'listUserProfilesResponse_httpStatus' - The response's http status code.
--
-- 'userProfiles', 'listUserProfilesResponse_userProfiles' - All the user profiles configured in AWS CodeStar for an AWS account.
newListUserProfilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUserProfilesResponse
newListUserProfilesResponse pHttpStatus_ =
  ListUserProfilesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      userProfiles = Core.mempty
    }

-- | The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
listUserProfilesResponse_nextToken :: Lens.Lens' ListUserProfilesResponse (Core.Maybe Core.Text)
listUserProfilesResponse_nextToken = Lens.lens (\ListUserProfilesResponse' {nextToken} -> nextToken) (\s@ListUserProfilesResponse' {} a -> s {nextToken = a} :: ListUserProfilesResponse)

-- | The response's http status code.
listUserProfilesResponse_httpStatus :: Lens.Lens' ListUserProfilesResponse Core.Int
listUserProfilesResponse_httpStatus = Lens.lens (\ListUserProfilesResponse' {httpStatus} -> httpStatus) (\s@ListUserProfilesResponse' {} a -> s {httpStatus = a} :: ListUserProfilesResponse)

-- | All the user profiles configured in AWS CodeStar for an AWS account.
listUserProfilesResponse_userProfiles :: Lens.Lens' ListUserProfilesResponse [UserProfileSummary]
listUserProfilesResponse_userProfiles = Lens.lens (\ListUserProfilesResponse' {userProfiles} -> userProfiles) (\s@ListUserProfilesResponse' {} a -> s {userProfiles = a} :: ListUserProfilesResponse) Core.. Lens._Coerce

instance Core.NFData ListUserProfilesResponse

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
-- Module      : Network.AWS.GuardDuty.ListOrganizationAdminAccounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts configured as GuardDuty delegated administrators.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListOrganizationAdminAccounts
  ( -- * Creating a Request
    ListOrganizationAdminAccounts (..),
    newListOrganizationAdminAccounts,

    -- * Request Lenses
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,

    -- * Destructuring the Response
    ListOrganizationAdminAccountsResponse (..),
    newListOrganizationAdminAccountsResponse,

    -- * Response Lenses
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOrganizationAdminAccounts' smart constructor.
data ListOrganizationAdminAccounts = ListOrganizationAdminAccounts'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOrganizationAdminAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationAdminAccounts_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'maxResults', 'listOrganizationAdminAccounts_maxResults' - The maximum number of results to return in the response.
newListOrganizationAdminAccounts ::
  ListOrganizationAdminAccounts
newListOrganizationAdminAccounts =
  ListOrganizationAdminAccounts'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listOrganizationAdminAccounts_nextToken :: Lens.Lens' ListOrganizationAdminAccounts (Core.Maybe Core.Text)
listOrganizationAdminAccounts_nextToken = Lens.lens (\ListOrganizationAdminAccounts' {nextToken} -> nextToken) (\s@ListOrganizationAdminAccounts' {} a -> s {nextToken = a} :: ListOrganizationAdminAccounts)

-- | The maximum number of results to return in the response.
listOrganizationAdminAccounts_maxResults :: Lens.Lens' ListOrganizationAdminAccounts (Core.Maybe Core.Natural)
listOrganizationAdminAccounts_maxResults = Lens.lens (\ListOrganizationAdminAccounts' {maxResults} -> maxResults) (\s@ListOrganizationAdminAccounts' {} a -> s {maxResults = a} :: ListOrganizationAdminAccounts)

instance Core.AWSPager ListOrganizationAdminAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationAdminAccountsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationAdminAccountsResponse_adminAccounts
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOrganizationAdminAccounts_nextToken
          Lens..~ rs
          Lens.^? listOrganizationAdminAccountsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListOrganizationAdminAccounts
  where
  type
    AWSResponse ListOrganizationAdminAccounts =
      ListOrganizationAdminAccountsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationAdminAccountsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "adminAccounts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOrganizationAdminAccounts

instance Core.NFData ListOrganizationAdminAccounts

instance Core.ToHeaders ListOrganizationAdminAccounts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListOrganizationAdminAccounts where
  toPath = Core.const "/admin"

instance Core.ToQuery ListOrganizationAdminAccounts where
  toQuery ListOrganizationAdminAccounts' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListOrganizationAdminAccountsResponse' smart constructor.
data ListOrganizationAdminAccountsResponse = ListOrganizationAdminAccountsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of accounts configured as GuardDuty delegated administrators.
    adminAccounts :: Core.Maybe [AdminAccount],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOrganizationAdminAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationAdminAccountsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'adminAccounts', 'listOrganizationAdminAccountsResponse_adminAccounts' - A list of accounts configured as GuardDuty delegated administrators.
--
-- 'httpStatus', 'listOrganizationAdminAccountsResponse_httpStatus' - The response's http status code.
newListOrganizationAdminAccountsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOrganizationAdminAccountsResponse
newListOrganizationAdminAccountsResponse pHttpStatus_ =
  ListOrganizationAdminAccountsResponse'
    { nextToken =
        Core.Nothing,
      adminAccounts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listOrganizationAdminAccountsResponse_nextToken :: Lens.Lens' ListOrganizationAdminAccountsResponse (Core.Maybe Core.Text)
listOrganizationAdminAccountsResponse_nextToken = Lens.lens (\ListOrganizationAdminAccountsResponse' {nextToken} -> nextToken) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {nextToken = a} :: ListOrganizationAdminAccountsResponse)

-- | A list of accounts configured as GuardDuty delegated administrators.
listOrganizationAdminAccountsResponse_adminAccounts :: Lens.Lens' ListOrganizationAdminAccountsResponse (Core.Maybe [AdminAccount])
listOrganizationAdminAccountsResponse_adminAccounts = Lens.lens (\ListOrganizationAdminAccountsResponse' {adminAccounts} -> adminAccounts) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {adminAccounts = a} :: ListOrganizationAdminAccountsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOrganizationAdminAccountsResponse_httpStatus :: Lens.Lens' ListOrganizationAdminAccountsResponse Core.Int
listOrganizationAdminAccountsResponse_httpStatus = Lens.lens (\ListOrganizationAdminAccountsResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {httpStatus = a} :: ListOrganizationAdminAccountsResponse)

instance
  Core.NFData
    ListOrganizationAdminAccountsResponse

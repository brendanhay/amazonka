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
-- Module      : Network.AWS.AppStream.DescribeUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified users in the user
-- pool.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUsers
  ( -- * Creating a Request
    DescribeUsers (..),
    newDescribeUsers,

    -- * Request Lenses
    describeUsers_nextToken,
    describeUsers_maxResults,
    describeUsers_authenticationType,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_nextToken,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int,
    -- | The authentication type for the users in the user pool to describe. You
    -- must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUsers_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'describeUsers_maxResults' - The maximum size of each page of results.
--
-- 'authenticationType', 'describeUsers_authenticationType' - The authentication type for the users in the user pool to describe. You
-- must specify USERPOOL.
newDescribeUsers ::
  -- | 'authenticationType'
  AuthenticationType ->
  DescribeUsers
newDescribeUsers pAuthenticationType_ =
  DescribeUsers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      authenticationType = pAuthenticationType_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUsers_nextToken :: Lens.Lens' DescribeUsers (Core.Maybe Core.Text)
describeUsers_nextToken = Lens.lens (\DescribeUsers' {nextToken} -> nextToken) (\s@DescribeUsers' {} a -> s {nextToken = a} :: DescribeUsers)

-- | The maximum size of each page of results.
describeUsers_maxResults :: Lens.Lens' DescribeUsers (Core.Maybe Core.Int)
describeUsers_maxResults = Lens.lens (\DescribeUsers' {maxResults} -> maxResults) (\s@DescribeUsers' {} a -> s {maxResults = a} :: DescribeUsers)

-- | The authentication type for the users in the user pool to describe. You
-- must specify USERPOOL.
describeUsers_authenticationType :: Lens.Lens' DescribeUsers AuthenticationType
describeUsers_authenticationType = Lens.lens (\DescribeUsers' {authenticationType} -> authenticationType) (\s@DescribeUsers' {} a -> s {authenticationType = a} :: DescribeUsers)

instance Core.AWSPager DescribeUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_users Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeUsers_nextToken
          Lens..~ rs
          Lens.^? describeUsersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUsers

instance Core.NFData DescribeUsers

instance Core.ToHeaders DescribeUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeUsers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUsers where
  toJSON DescribeUsers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath DescribeUsers where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about users in the user pool.
    users :: Core.Maybe [User],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUsersResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'users', 'describeUsersResponse_users' - Information about users in the user pool.
--
-- 'httpStatus', 'describeUsersResponse_httpStatus' - The response's http status code.
newDescribeUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { nextToken = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUsersResponse_nextToken :: Lens.Lens' DescribeUsersResponse (Core.Maybe Core.Text)
describeUsersResponse_nextToken = Lens.lens (\DescribeUsersResponse' {nextToken} -> nextToken) (\s@DescribeUsersResponse' {} a -> s {nextToken = a} :: DescribeUsersResponse)

-- | Information about users in the user pool.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Core.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Core.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Core.NFData DescribeUsersResponse

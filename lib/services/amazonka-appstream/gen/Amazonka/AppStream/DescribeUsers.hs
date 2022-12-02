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
-- Module      : Amazonka.AppStream.DescribeUsers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified users in the user
-- pool.
--
-- This operation returns paginated results.
module Amazonka.AppStream.DescribeUsers
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The authentication type for the users in the user pool to describe. You
    -- must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      authenticationType = pAuthenticationType_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUsers_nextToken :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_nextToken = Lens.lens (\DescribeUsers' {nextToken} -> nextToken) (\s@DescribeUsers' {} a -> s {nextToken = a} :: DescribeUsers)

-- | The maximum size of each page of results.
describeUsers_maxResults :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Int)
describeUsers_maxResults = Lens.lens (\DescribeUsers' {maxResults} -> maxResults) (\s@DescribeUsers' {} a -> s {maxResults = a} :: DescribeUsers)

-- | The authentication type for the users in the user pool to describe. You
-- must specify USERPOOL.
describeUsers_authenticationType :: Lens.Lens' DescribeUsers AuthenticationType
describeUsers_authenticationType = Lens.lens (\DescribeUsers' {authenticationType} -> authenticationType) (\s@DescribeUsers' {} a -> s {authenticationType = a} :: DescribeUsers)

instance Core.AWSPager DescribeUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUsers_nextToken
          Lens..~ rs
          Lens.^? describeUsersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers where
  hashWithSalt _salt DescribeUsers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData DescribeUsers where
  rnf DescribeUsers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf authenticationType

instance Data.ToHeaders DescribeUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUsers where
  toJSON DescribeUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("AuthenticationType" Data..= authenticationType)
          ]
      )

instance Data.ToPath DescribeUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about users in the user pool.
    users :: Prelude.Maybe [User],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { nextToken = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUsersResponse_nextToken :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_nextToken = Lens.lens (\DescribeUsersResponse' {nextToken} -> nextToken) (\s@DescribeUsersResponse' {} a -> s {nextToken = a} :: DescribeUsersResponse)

-- | Information about users in the user pool.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse where
  rnf DescribeUsersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus

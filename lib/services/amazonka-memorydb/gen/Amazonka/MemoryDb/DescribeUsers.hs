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
-- Module      : Amazonka.MemoryDb.DescribeUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users.
module Amazonka.MemoryDb.DescribeUsers
  ( -- * Creating a Request
    DescribeUsers (..),
    newDescribeUsers,

    -- * Request Lenses
    describeUsers_filters,
    describeUsers_userName,
    describeUsers_nextToken,
    describeUsers_maxResults,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_users,
    describeUsersResponse_nextToken,
    describeUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | Filter to determine the list of users to return.
    filters :: Prelude.Maybe [Filter],
    -- | The name of the user
    userName :: Prelude.Maybe Prelude.Text,
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'filters', 'describeUsers_filters' - Filter to determine the list of users to return.
--
-- 'userName', 'describeUsers_userName' - The name of the user
--
-- 'nextToken', 'describeUsers_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'maxResults', 'describeUsers_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
newDescribeUsers ::
  DescribeUsers
newDescribeUsers =
  DescribeUsers'
    { filters = Prelude.Nothing,
      userName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filter to determine the list of users to return.
describeUsers_filters :: Lens.Lens' DescribeUsers (Prelude.Maybe [Filter])
describeUsers_filters = Lens.lens (\DescribeUsers' {filters} -> filters) (\s@DescribeUsers' {} a -> s {filters = a} :: DescribeUsers) Prelude.. Lens.mapping Lens.coerced

-- | The name of the user
describeUsers_userName :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_userName = Lens.lens (\DescribeUsers' {userName} -> userName) (\s@DescribeUsers' {} a -> s {userName = a} :: DescribeUsers)

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeUsers_nextToken :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_nextToken = Lens.lens (\DescribeUsers' {nextToken} -> nextToken) (\s@DescribeUsers' {} a -> s {nextToken = a} :: DescribeUsers)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeUsers_maxResults :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Int)
describeUsers_maxResults = Lens.lens (\DescribeUsers' {maxResults} -> maxResults) (\s@DescribeUsers' {} a -> s {maxResults = a} :: DescribeUsers)

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Prelude.<$> (x Core..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers where
  hashWithSalt _salt DescribeUsers' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeUsers where
  rnf DescribeUsers' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.DescribeUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUsers where
  toJSON DescribeUsers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("UserName" Core..=) Prelude.<$> userName,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeUsers where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | A list of users.
    users :: Prelude.Maybe [User],
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'users', 'describeUsersResponse_users' - A list of users.
--
-- 'nextToken', 'describeUsersResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'httpStatus', 'describeUsersResponse_httpStatus' - The response's http status code.
newDescribeUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { users = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of users.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeUsersResponse_nextToken :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_nextToken = Lens.lens (\DescribeUsersResponse' {nextToken} -> nextToken) (\s@DescribeUsersResponse' {} a -> s {nextToken = a} :: DescribeUsersResponse)

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse where
  rnf DescribeUsersResponse' {..} =
    Prelude.rnf users
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

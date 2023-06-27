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
-- Module      : Amazonka.FinSpace.ListKxUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the users in a kdb environment.
module Amazonka.FinSpace.ListKxUsers
  ( -- * Creating a Request
    ListKxUsers (..),
    newListKxUsers,

    -- * Request Lenses
    listKxUsers_maxResults,
    listKxUsers_nextToken,
    listKxUsers_environmentId,

    -- * Destructuring the Response
    ListKxUsersResponse (..),
    newListKxUsersResponse,

    -- * Response Lenses
    listKxUsersResponse_nextToken,
    listKxUsersResponse_users,
    listKxUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxUsers' smart constructor.
data ListKxUsers = ListKxUsers'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKxUsers_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxUsers_nextToken' - A token that indicates where a results page should begin.
--
-- 'environmentId', 'listKxUsers_environmentId' - A unique identifier for the kdb environment.
newListKxUsers ::
  -- | 'environmentId'
  Prelude.Text ->
  ListKxUsers
newListKxUsers pEnvironmentId_ =
  ListKxUsers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | The maximum number of results to return in this request.
listKxUsers_maxResults :: Lens.Lens' ListKxUsers (Prelude.Maybe Prelude.Natural)
listKxUsers_maxResults = Lens.lens (\ListKxUsers' {maxResults} -> maxResults) (\s@ListKxUsers' {} a -> s {maxResults = a} :: ListKxUsers)

-- | A token that indicates where a results page should begin.
listKxUsers_nextToken :: Lens.Lens' ListKxUsers (Prelude.Maybe Prelude.Text)
listKxUsers_nextToken = Lens.lens (\ListKxUsers' {nextToken} -> nextToken) (\s@ListKxUsers' {} a -> s {nextToken = a} :: ListKxUsers)

-- | A unique identifier for the kdb environment.
listKxUsers_environmentId :: Lens.Lens' ListKxUsers Prelude.Text
listKxUsers_environmentId = Lens.lens (\ListKxUsers' {environmentId} -> environmentId) (\s@ListKxUsers' {} a -> s {environmentId = a} :: ListKxUsers)

instance Core.AWSRequest ListKxUsers where
  type AWSResponse ListKxUsers = ListKxUsersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxUsersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxUsers where
  hashWithSalt _salt ListKxUsers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ListKxUsers where
  rnf ListKxUsers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders ListKxUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxUsers where
  toPath ListKxUsers' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/users"
      ]

instance Data.ToQuery ListKxUsers where
  toQuery ListKxUsers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxUsersResponse' smart constructor.
data ListKxUsersResponse = ListKxUsersResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of users in a kdb environment.
    users :: Prelude.Maybe [KxUser],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKxUsersResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'users', 'listKxUsersResponse_users' - A list of users in a kdb environment.
--
-- 'httpStatus', 'listKxUsersResponse_httpStatus' - The response's http status code.
newListKxUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxUsersResponse
newListKxUsersResponse pHttpStatus_ =
  ListKxUsersResponse'
    { nextToken = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listKxUsersResponse_nextToken :: Lens.Lens' ListKxUsersResponse (Prelude.Maybe Prelude.Text)
listKxUsersResponse_nextToken = Lens.lens (\ListKxUsersResponse' {nextToken} -> nextToken) (\s@ListKxUsersResponse' {} a -> s {nextToken = a} :: ListKxUsersResponse)

-- | A list of users in a kdb environment.
listKxUsersResponse_users :: Lens.Lens' ListKxUsersResponse (Prelude.Maybe [KxUser])
listKxUsersResponse_users = Lens.lens (\ListKxUsersResponse' {users} -> users) (\s@ListKxUsersResponse' {} a -> s {users = a} :: ListKxUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listKxUsersResponse_httpStatus :: Lens.Lens' ListKxUsersResponse Prelude.Int
listKxUsersResponse_httpStatus = Lens.lens (\ListKxUsersResponse' {httpStatus} -> httpStatus) (\s@ListKxUsersResponse' {} a -> s {httpStatus = a} :: ListKxUsersResponse)

instance Prelude.NFData ListKxUsersResponse where
  rnf ListKxUsersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus

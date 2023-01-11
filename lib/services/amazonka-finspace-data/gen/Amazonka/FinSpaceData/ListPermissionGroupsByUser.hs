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
-- Module      : Amazonka.FinSpaceData.ListPermissionGroupsByUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the permission groups that are associated with a specific user
-- account.
module Amazonka.FinSpaceData.ListPermissionGroupsByUser
  ( -- * Creating a Request
    ListPermissionGroupsByUser (..),
    newListPermissionGroupsByUser,

    -- * Request Lenses
    listPermissionGroupsByUser_nextToken,
    listPermissionGroupsByUser_userId,
    listPermissionGroupsByUser_maxResults,

    -- * Destructuring the Response
    ListPermissionGroupsByUserResponse (..),
    newListPermissionGroupsByUserResponse,

    -- * Response Lenses
    listPermissionGroupsByUserResponse_nextToken,
    listPermissionGroupsByUserResponse_permissionGroups,
    listPermissionGroupsByUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissionGroupsByUser' smart constructor.
data ListPermissionGroupsByUser = ListPermissionGroupsByUser'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user.
    userId :: Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionGroupsByUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionGroupsByUser_nextToken' - A token that indicates where a results page should begin.
--
-- 'userId', 'listPermissionGroupsByUser_userId' - The unique identifier for the user.
--
-- 'maxResults', 'listPermissionGroupsByUser_maxResults' - The maximum number of results per page.
newListPermissionGroupsByUser ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'maxResults'
  Prelude.Natural ->
  ListPermissionGroupsByUser
newListPermissionGroupsByUser pUserId_ pMaxResults_ =
  ListPermissionGroupsByUser'
    { nextToken =
        Prelude.Nothing,
      userId = pUserId_,
      maxResults = pMaxResults_
    }

-- | A token that indicates where a results page should begin.
listPermissionGroupsByUser_nextToken :: Lens.Lens' ListPermissionGroupsByUser (Prelude.Maybe Prelude.Text)
listPermissionGroupsByUser_nextToken = Lens.lens (\ListPermissionGroupsByUser' {nextToken} -> nextToken) (\s@ListPermissionGroupsByUser' {} a -> s {nextToken = a} :: ListPermissionGroupsByUser)

-- | The unique identifier for the user.
listPermissionGroupsByUser_userId :: Lens.Lens' ListPermissionGroupsByUser Prelude.Text
listPermissionGroupsByUser_userId = Lens.lens (\ListPermissionGroupsByUser' {userId} -> userId) (\s@ListPermissionGroupsByUser' {} a -> s {userId = a} :: ListPermissionGroupsByUser)

-- | The maximum number of results per page.
listPermissionGroupsByUser_maxResults :: Lens.Lens' ListPermissionGroupsByUser Prelude.Natural
listPermissionGroupsByUser_maxResults = Lens.lens (\ListPermissionGroupsByUser' {maxResults} -> maxResults) (\s@ListPermissionGroupsByUser' {} a -> s {maxResults = a} :: ListPermissionGroupsByUser)

instance Core.AWSRequest ListPermissionGroupsByUser where
  type
    AWSResponse ListPermissionGroupsByUser =
      ListPermissionGroupsByUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionGroupsByUserResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "permissionGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissionGroupsByUser where
  hashWithSalt _salt ListPermissionGroupsByUser' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPermissionGroupsByUser where
  rnf ListPermissionGroupsByUser' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListPermissionGroupsByUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPermissionGroupsByUser where
  toPath ListPermissionGroupsByUser' {..} =
    Prelude.mconcat
      ["/user/", Data.toBS userId, "/permission-groups"]

instance Data.ToQuery ListPermissionGroupsByUser where
  toQuery ListPermissionGroupsByUser' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListPermissionGroupsByUserResponse' smart constructor.
data ListPermissionGroupsByUserResponse = ListPermissionGroupsByUserResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of returned permission groups.
    permissionGroups :: Prelude.Maybe [PermissionGroupByUser],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionGroupsByUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionGroupsByUserResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'permissionGroups', 'listPermissionGroupsByUserResponse_permissionGroups' - A list of returned permission groups.
--
-- 'httpStatus', 'listPermissionGroupsByUserResponse_httpStatus' - The response's http status code.
newListPermissionGroupsByUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionGroupsByUserResponse
newListPermissionGroupsByUserResponse pHttpStatus_ =
  ListPermissionGroupsByUserResponse'
    { nextToken =
        Prelude.Nothing,
      permissionGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listPermissionGroupsByUserResponse_nextToken :: Lens.Lens' ListPermissionGroupsByUserResponse (Prelude.Maybe Prelude.Text)
listPermissionGroupsByUserResponse_nextToken = Lens.lens (\ListPermissionGroupsByUserResponse' {nextToken} -> nextToken) (\s@ListPermissionGroupsByUserResponse' {} a -> s {nextToken = a} :: ListPermissionGroupsByUserResponse)

-- | A list of returned permission groups.
listPermissionGroupsByUserResponse_permissionGroups :: Lens.Lens' ListPermissionGroupsByUserResponse (Prelude.Maybe [PermissionGroupByUser])
listPermissionGroupsByUserResponse_permissionGroups = Lens.lens (\ListPermissionGroupsByUserResponse' {permissionGroups} -> permissionGroups) (\s@ListPermissionGroupsByUserResponse' {} a -> s {permissionGroups = a} :: ListPermissionGroupsByUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionGroupsByUserResponse_httpStatus :: Lens.Lens' ListPermissionGroupsByUserResponse Prelude.Int
listPermissionGroupsByUserResponse_httpStatus = Lens.lens (\ListPermissionGroupsByUserResponse' {httpStatus} -> httpStatus) (\s@ListPermissionGroupsByUserResponse' {} a -> s {httpStatus = a} :: ListPermissionGroupsByUserResponse)

instance
  Prelude.NFData
    ListPermissionGroupsByUserResponse
  where
  rnf ListPermissionGroupsByUserResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionGroups
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.FinSpaceData.ListUsersByPermissionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details of all the users in a specific permission group.
module Amazonka.FinSpaceData.ListUsersByPermissionGroup
  ( -- * Creating a Request
    ListUsersByPermissionGroup (..),
    newListUsersByPermissionGroup,

    -- * Request Lenses
    listUsersByPermissionGroup_nextToken,
    listUsersByPermissionGroup_permissionGroupId,
    listUsersByPermissionGroup_maxResults,

    -- * Destructuring the Response
    ListUsersByPermissionGroupResponse (..),
    newListUsersByPermissionGroupResponse,

    -- * Response Lenses
    listUsersByPermissionGroupResponse_nextToken,
    listUsersByPermissionGroupResponse_users,
    listUsersByPermissionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsersByPermissionGroup' smart constructor.
data ListUsersByPermissionGroup = ListUsersByPermissionGroup'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsersByPermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersByPermissionGroup_nextToken' - A token that indicates where a results page should begin.
--
-- 'permissionGroupId', 'listUsersByPermissionGroup_permissionGroupId' - The unique identifier for the permission group.
--
-- 'maxResults', 'listUsersByPermissionGroup_maxResults' - The maximum number of results per page.
newListUsersByPermissionGroup ::
  -- | 'permissionGroupId'
  Prelude.Text ->
  -- | 'maxResults'
  Prelude.Natural ->
  ListUsersByPermissionGroup
newListUsersByPermissionGroup
  pPermissionGroupId_
  pMaxResults_ =
    ListUsersByPermissionGroup'
      { nextToken =
          Prelude.Nothing,
        permissionGroupId = pPermissionGroupId_,
        maxResults = pMaxResults_
      }

-- | A token that indicates where a results page should begin.
listUsersByPermissionGroup_nextToken :: Lens.Lens' ListUsersByPermissionGroup (Prelude.Maybe Prelude.Text)
listUsersByPermissionGroup_nextToken = Lens.lens (\ListUsersByPermissionGroup' {nextToken} -> nextToken) (\s@ListUsersByPermissionGroup' {} a -> s {nextToken = a} :: ListUsersByPermissionGroup)

-- | The unique identifier for the permission group.
listUsersByPermissionGroup_permissionGroupId :: Lens.Lens' ListUsersByPermissionGroup Prelude.Text
listUsersByPermissionGroup_permissionGroupId = Lens.lens (\ListUsersByPermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@ListUsersByPermissionGroup' {} a -> s {permissionGroupId = a} :: ListUsersByPermissionGroup)

-- | The maximum number of results per page.
listUsersByPermissionGroup_maxResults :: Lens.Lens' ListUsersByPermissionGroup Prelude.Natural
listUsersByPermissionGroup_maxResults = Lens.lens (\ListUsersByPermissionGroup' {maxResults} -> maxResults) (\s@ListUsersByPermissionGroup' {} a -> s {maxResults = a} :: ListUsersByPermissionGroup)

instance Core.AWSRequest ListUsersByPermissionGroup where
  type
    AWSResponse ListUsersByPermissionGroup =
      ListUsersByPermissionGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersByPermissionGroupResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsersByPermissionGroup where
  hashWithSalt _salt ListUsersByPermissionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` permissionGroupId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListUsersByPermissionGroup where
  rnf ListUsersByPermissionGroup' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionGroupId
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListUsersByPermissionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListUsersByPermissionGroup where
  toPath ListUsersByPermissionGroup' {..} =
    Prelude.mconcat
      [ "/permission-group/",
        Data.toBS permissionGroupId,
        "/users"
      ]

instance Data.ToQuery ListUsersByPermissionGroup where
  toQuery ListUsersByPermissionGroup' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListUsersByPermissionGroupResponse' smart constructor.
data ListUsersByPermissionGroupResponse = ListUsersByPermissionGroupResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists details of all users in a specific permission group.
    users :: Prelude.Maybe [UserByPermissionGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsersByPermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersByPermissionGroupResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'users', 'listUsersByPermissionGroupResponse_users' - Lists details of all users in a specific permission group.
--
-- 'httpStatus', 'listUsersByPermissionGroupResponse_httpStatus' - The response's http status code.
newListUsersByPermissionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsersByPermissionGroupResponse
newListUsersByPermissionGroupResponse pHttpStatus_ =
  ListUsersByPermissionGroupResponse'
    { nextToken =
        Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listUsersByPermissionGroupResponse_nextToken :: Lens.Lens' ListUsersByPermissionGroupResponse (Prelude.Maybe Prelude.Text)
listUsersByPermissionGroupResponse_nextToken = Lens.lens (\ListUsersByPermissionGroupResponse' {nextToken} -> nextToken) (\s@ListUsersByPermissionGroupResponse' {} a -> s {nextToken = a} :: ListUsersByPermissionGroupResponse)

-- | Lists details of all users in a specific permission group.
listUsersByPermissionGroupResponse_users :: Lens.Lens' ListUsersByPermissionGroupResponse (Prelude.Maybe [UserByPermissionGroup])
listUsersByPermissionGroupResponse_users = Lens.lens (\ListUsersByPermissionGroupResponse' {users} -> users) (\s@ListUsersByPermissionGroupResponse' {} a -> s {users = a} :: ListUsersByPermissionGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUsersByPermissionGroupResponse_httpStatus :: Lens.Lens' ListUsersByPermissionGroupResponse Prelude.Int
listUsersByPermissionGroupResponse_httpStatus = Lens.lens (\ListUsersByPermissionGroupResponse' {httpStatus} -> httpStatus) (\s@ListUsersByPermissionGroupResponse' {} a -> s {httpStatus = a} :: ListUsersByPermissionGroupResponse)

instance
  Prelude.NFData
    ListUsersByPermissionGroupResponse
  where
  rnf ListUsersByPermissionGroupResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus

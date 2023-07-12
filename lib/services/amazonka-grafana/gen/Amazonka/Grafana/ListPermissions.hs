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
-- Module      : Amazonka.Grafana.ListPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users and groups who have the Grafana @Admin@ and @Editor@
-- roles in this workspace. If you use this operation without specifying
-- @userId@ or @groupId@, the operation returns the roles of all users and
-- groups. If you specify a @userId@ or a @groupId@, only the roles for
-- that user or group are returned. If you do this, you can specify only
-- one @userId@ or one @groupId@.
--
-- This operation returns paginated results.
module Amazonka.Grafana.ListPermissions
  ( -- * Creating a Request
    ListPermissions (..),
    newListPermissions,

    -- * Request Lenses
    listPermissions_groupId,
    listPermissions_maxResults,
    listPermissions_nextToken,
    listPermissions_userId,
    listPermissions_userType,
    listPermissions_workspaceId,

    -- * Destructuring the Response
    ListPermissionsResponse (..),
    newListPermissionsResponse,

    -- * Response Lenses
    listPermissionsResponse_nextToken,
    listPermissionsResponse_httpStatus,
    listPermissionsResponse_permissions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { -- | (Optional) Limits the results to only the group that matches this ID.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use when requesting the next set of results. You received
    -- this token from a previous @ListPermissions@ operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Limits the results to only the user that matches this ID.
    userId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If you specify @SSO_USER@, then only the permissions of IAM
    -- Identity Center users are returned. If you specify @SSO_GROUP@, only the
    -- permissions of IAM Identity Center groups are returned.
    userType :: Prelude.Maybe UserType,
    -- | The ID of the workspace to list permissions for. This parameter is
    -- required.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'listPermissions_groupId' - (Optional) Limits the results to only the group that matches this ID.
--
-- 'maxResults', 'listPermissions_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listPermissions_nextToken' - The token to use when requesting the next set of results. You received
-- this token from a previous @ListPermissions@ operation.
--
-- 'userId', 'listPermissions_userId' - (Optional) Limits the results to only the user that matches this ID.
--
-- 'userType', 'listPermissions_userType' - (Optional) If you specify @SSO_USER@, then only the permissions of IAM
-- Identity Center users are returned. If you specify @SSO_GROUP@, only the
-- permissions of IAM Identity Center groups are returned.
--
-- 'workspaceId', 'listPermissions_workspaceId' - The ID of the workspace to list permissions for. This parameter is
-- required.
newListPermissions ::
  -- | 'workspaceId'
  Prelude.Text ->
  ListPermissions
newListPermissions pWorkspaceId_ =
  ListPermissions'
    { groupId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userId = Prelude.Nothing,
      userType = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | (Optional) Limits the results to only the group that matches this ID.
listPermissions_groupId :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_groupId = Lens.lens (\ListPermissions' {groupId} -> groupId) (\s@ListPermissions' {} a -> s {groupId = a} :: ListPermissions)

-- | The maximum number of results to include in the response.
listPermissions_maxResults :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Natural)
listPermissions_maxResults = Lens.lens (\ListPermissions' {maxResults} -> maxResults) (\s@ListPermissions' {} a -> s {maxResults = a} :: ListPermissions)

-- | The token to use when requesting the next set of results. You received
-- this token from a previous @ListPermissions@ operation.
listPermissions_nextToken :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_nextToken = Lens.lens (\ListPermissions' {nextToken} -> nextToken) (\s@ListPermissions' {} a -> s {nextToken = a} :: ListPermissions)

-- | (Optional) Limits the results to only the user that matches this ID.
listPermissions_userId :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_userId = Lens.lens (\ListPermissions' {userId} -> userId) (\s@ListPermissions' {} a -> s {userId = a} :: ListPermissions)

-- | (Optional) If you specify @SSO_USER@, then only the permissions of IAM
-- Identity Center users are returned. If you specify @SSO_GROUP@, only the
-- permissions of IAM Identity Center groups are returned.
listPermissions_userType :: Lens.Lens' ListPermissions (Prelude.Maybe UserType)
listPermissions_userType = Lens.lens (\ListPermissions' {userType} -> userType) (\s@ListPermissions' {} a -> s {userType = a} :: ListPermissions)

-- | The ID of the workspace to list permissions for. This parameter is
-- required.
listPermissions_workspaceId :: Lens.Lens' ListPermissions Prelude.Text
listPermissions_workspaceId = Lens.lens (\ListPermissions' {workspaceId} -> workspaceId) (\s@ListPermissions' {} a -> s {workspaceId = a} :: ListPermissions)

instance Core.AWSPager ListPermissions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPermissionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPermissionsResponse_permissions) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPermissions_nextToken
          Lens..~ rs
          Lens.^? listPermissionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPermissions where
  type
    AWSResponse ListPermissions =
      ListPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "permissions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPermissions where
  hashWithSalt _salt ListPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData ListPermissions where
  rnf ListPermissions' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders ListPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPermissions where
  toPath ListPermissions' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/permissions"
      ]

instance Data.ToQuery ListPermissions where
  toQuery ListPermissions' {..} =
    Prelude.mconcat
      [ "groupId" Data.=: groupId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "userId" Data.=: userId,
        "userType" Data.=: userType
      ]

-- | /See:/ 'newListPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { -- | The token to use in a subsequent @ListPermissions@ operation to return
    -- the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The permissions returned by the operation.
    permissions :: [PermissionEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionsResponse_nextToken' - The token to use in a subsequent @ListPermissions@ operation to return
-- the next set of results.
--
-- 'httpStatus', 'listPermissionsResponse_httpStatus' - The response's http status code.
--
-- 'permissions', 'listPermissionsResponse_permissions' - The permissions returned by the operation.
newListPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionsResponse
newListPermissionsResponse pHttpStatus_ =
  ListPermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      permissions = Prelude.mempty
    }

-- | The token to use in a subsequent @ListPermissions@ operation to return
-- the next set of results.
listPermissionsResponse_nextToken :: Lens.Lens' ListPermissionsResponse (Prelude.Maybe Prelude.Text)
listPermissionsResponse_nextToken = Lens.lens (\ListPermissionsResponse' {nextToken} -> nextToken) (\s@ListPermissionsResponse' {} a -> s {nextToken = a} :: ListPermissionsResponse)

-- | The response's http status code.
listPermissionsResponse_httpStatus :: Lens.Lens' ListPermissionsResponse Prelude.Int
listPermissionsResponse_httpStatus = Lens.lens (\ListPermissionsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionsResponse' {} a -> s {httpStatus = a} :: ListPermissionsResponse)

-- | The permissions returned by the operation.
listPermissionsResponse_permissions :: Lens.Lens' ListPermissionsResponse [PermissionEntry]
listPermissionsResponse_permissions = Lens.lens (\ListPermissionsResponse' {permissions} -> permissions) (\s@ListPermissionsResponse' {} a -> s {permissions = a} :: ListPermissionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPermissionsResponse where
  rnf ListPermissionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf permissions

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
-- Module      : Amazonka.RAM.ListPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RAM permissions.
module Amazonka.RAM.ListPermissions
  ( -- * Creating a Request
    ListPermissions (..),
    newListPermissions,

    -- * Request Lenses
    listPermissions_resourceType,
    listPermissions_nextToken,
    listPermissions_maxResults,

    -- * Destructuring the Response
    ListPermissionsResponse (..),
    newListPermissionsResponse,

    -- * Response Lenses
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { -- | Specifies the resource type for which to list permissions. For example,
    -- to list only permissions that apply to EC2 subnets, specify
    -- @ec2:Subnet@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'resourceType', 'listPermissions_resourceType' - Specifies the resource type for which to list permissions. For example,
-- to list only permissions that apply to EC2 subnets, specify
-- @ec2:Subnet@.
--
-- 'nextToken', 'listPermissions_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listPermissions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListPermissions ::
  ListPermissions
newListPermissions =
  ListPermissions'
    { resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specifies the resource type for which to list permissions. For example,
-- to list only permissions that apply to EC2 subnets, specify
-- @ec2:Subnet@.
listPermissions_resourceType :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_resourceType = Lens.lens (\ListPermissions' {resourceType} -> resourceType) (\s@ListPermissions' {} a -> s {resourceType = a} :: ListPermissions)

-- | The token for the next page of results.
listPermissions_nextToken :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_nextToken = Lens.lens (\ListPermissions' {nextToken} -> nextToken) (\s@ListPermissions' {} a -> s {nextToken = a} :: ListPermissions)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listPermissions_maxResults :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Natural)
listPermissions_maxResults = Lens.lens (\ListPermissions' {maxResults} -> maxResults) (\s@ListPermissions' {} a -> s {maxResults = a} :: ListPermissions)

instance Core.AWSRequest ListPermissions where
  type
    AWSResponse ListPermissions =
      ListPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissions where
  hashWithSalt _salt ListPermissions' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPermissions where
  rnf ListPermissions' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPermissions where
  toJSON ListPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceType" Core..=) Prelude.<$> resourceType,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPermissions where
  toPath = Prelude.const "/listpermissions"

instance Core.ToQuery ListPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions.
    permissions :: Prelude.Maybe [ResourceSharePermissionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listPermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'permissions', 'listPermissionsResponse_permissions' - Information about the permissions.
--
-- 'httpStatus', 'listPermissionsResponse_httpStatus' - The response's http status code.
newListPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionsResponse
newListPermissionsResponse pHttpStatus_ =
  ListPermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listPermissionsResponse_nextToken :: Lens.Lens' ListPermissionsResponse (Prelude.Maybe Prelude.Text)
listPermissionsResponse_nextToken = Lens.lens (\ListPermissionsResponse' {nextToken} -> nextToken) (\s@ListPermissionsResponse' {} a -> s {nextToken = a} :: ListPermissionsResponse)

-- | Information about the permissions.
listPermissionsResponse_permissions :: Lens.Lens' ListPermissionsResponse (Prelude.Maybe [ResourceSharePermissionSummary])
listPermissionsResponse_permissions = Lens.lens (\ListPermissionsResponse' {permissions} -> permissions) (\s@ListPermissionsResponse' {} a -> s {permissions = a} :: ListPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionsResponse_httpStatus :: Lens.Lens' ListPermissionsResponse Prelude.Int
listPermissionsResponse_httpStatus = Lens.lens (\ListPermissionsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionsResponse' {} a -> s {httpStatus = a} :: ListPermissionsResponse)

instance Prelude.NFData ListPermissionsResponse where
  rnf ListPermissionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus

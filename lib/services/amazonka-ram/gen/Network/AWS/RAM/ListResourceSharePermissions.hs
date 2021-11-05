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
-- Module      : Amazonka.RAM.ListResourceSharePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RAM permissions that are associated with a resource share.
module Amazonka.RAM.ListResourceSharePermissions
  ( -- * Creating a Request
    ListResourceSharePermissions (..),
    newListResourceSharePermissions,

    -- * Request Lenses
    listResourceSharePermissions_nextToken,
    listResourceSharePermissions_maxResults,
    listResourceSharePermissions_resourceShareArn,

    -- * Destructuring the Response
    ListResourceSharePermissionsResponse (..),
    newListResourceSharePermissionsResponse,

    -- * Response Lenses
    listResourceSharePermissionsResponse_nextToken,
    listResourceSharePermissionsResponse_permissions,
    listResourceSharePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceSharePermissions' smart constructor.
data ListResourceSharePermissions = ListResourceSharePermissions'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSharePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSharePermissions_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listResourceSharePermissions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareArn', 'listResourceSharePermissions_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
newListResourceSharePermissions ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  ListResourceSharePermissions
newListResourceSharePermissions pResourceShareArn_ =
  ListResourceSharePermissions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
    }

-- | The token for the next page of results.
listResourceSharePermissions_nextToken :: Lens.Lens' ListResourceSharePermissions (Prelude.Maybe Prelude.Text)
listResourceSharePermissions_nextToken = Lens.lens (\ListResourceSharePermissions' {nextToken} -> nextToken) (\s@ListResourceSharePermissions' {} a -> s {nextToken = a} :: ListResourceSharePermissions)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listResourceSharePermissions_maxResults :: Lens.Lens' ListResourceSharePermissions (Prelude.Maybe Prelude.Natural)
listResourceSharePermissions_maxResults = Lens.lens (\ListResourceSharePermissions' {maxResults} -> maxResults) (\s@ListResourceSharePermissions' {} a -> s {maxResults = a} :: ListResourceSharePermissions)

-- | The Amazon Resource Name (ARN) of the resource share.
listResourceSharePermissions_resourceShareArn :: Lens.Lens' ListResourceSharePermissions Prelude.Text
listResourceSharePermissions_resourceShareArn = Lens.lens (\ListResourceSharePermissions' {resourceShareArn} -> resourceShareArn) (\s@ListResourceSharePermissions' {} a -> s {resourceShareArn = a} :: ListResourceSharePermissions)

instance Core.AWSRequest ListResourceSharePermissions where
  type
    AWSResponse ListResourceSharePermissions =
      ListResourceSharePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceSharePermissionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceSharePermissions

instance Prelude.NFData ListResourceSharePermissions

instance Core.ToHeaders ListResourceSharePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceSharePermissions where
  toJSON ListResourceSharePermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn)
          ]
      )

instance Core.ToPath ListResourceSharePermissions where
  toPath =
    Prelude.const "/listresourcesharepermissions"

instance Core.ToQuery ListResourceSharePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceSharePermissionsResponse' smart constructor.
data ListResourceSharePermissionsResponse = ListResourceSharePermissionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The permissions associated with the resource share.
    permissions :: Prelude.Maybe [ResourceSharePermissionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSharePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSharePermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'permissions', 'listResourceSharePermissionsResponse_permissions' - The permissions associated with the resource share.
--
-- 'httpStatus', 'listResourceSharePermissionsResponse_httpStatus' - The response's http status code.
newListResourceSharePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceSharePermissionsResponse
newListResourceSharePermissionsResponse pHttpStatus_ =
  ListResourceSharePermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listResourceSharePermissionsResponse_nextToken :: Lens.Lens' ListResourceSharePermissionsResponse (Prelude.Maybe Prelude.Text)
listResourceSharePermissionsResponse_nextToken = Lens.lens (\ListResourceSharePermissionsResponse' {nextToken} -> nextToken) (\s@ListResourceSharePermissionsResponse' {} a -> s {nextToken = a} :: ListResourceSharePermissionsResponse)

-- | The permissions associated with the resource share.
listResourceSharePermissionsResponse_permissions :: Lens.Lens' ListResourceSharePermissionsResponse (Prelude.Maybe [ResourceSharePermissionSummary])
listResourceSharePermissionsResponse_permissions = Lens.lens (\ListResourceSharePermissionsResponse' {permissions} -> permissions) (\s@ListResourceSharePermissionsResponse' {} a -> s {permissions = a} :: ListResourceSharePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceSharePermissionsResponse_httpStatus :: Lens.Lens' ListResourceSharePermissionsResponse Prelude.Int
listResourceSharePermissionsResponse_httpStatus = Lens.lens (\ListResourceSharePermissionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceSharePermissionsResponse' {} a -> s {httpStatus = a} :: ListResourceSharePermissionsResponse)

instance
  Prelude.NFData
    ListResourceSharePermissionsResponse

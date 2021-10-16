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
-- Module      : Network.AWS.WorkDocs.DescribeResourcePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a specified resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeResourcePermissions
  ( -- * Creating a Request
    DescribeResourcePermissions (..),
    newDescribeResourcePermissions,

    -- * Request Lenses
    describeResourcePermissions_principalId,
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_limit,
    describeResourcePermissions_marker,
    describeResourcePermissions_resourceId,

    -- * Destructuring the Response
    DescribeResourcePermissionsResponse (..),
    newDescribeResourcePermissionsResponse,

    -- * Response Lenses
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeResourcePermissions' smart constructor.
data DescribeResourcePermissions = DescribeResourcePermissions'
  { -- | The ID of the principal to filter permissions by.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'describeResourcePermissions_principalId' - The ID of the principal to filter permissions by.
--
-- 'authenticationToken', 'describeResourcePermissions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeResourcePermissions_limit' - The maximum number of items to return with this call.
--
-- 'marker', 'describeResourcePermissions_marker' - The marker for the next set of results. (You received this marker from a
-- previous call)
--
-- 'resourceId', 'describeResourcePermissions_resourceId' - The ID of the resource.
newDescribeResourcePermissions ::
  -- | 'resourceId'
  Prelude.Text ->
  DescribeResourcePermissions
newDescribeResourcePermissions pResourceId_ =
  DescribeResourcePermissions'
    { principalId =
        Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the principal to filter permissions by.
describeResourcePermissions_principalId :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_principalId = Lens.lens (\DescribeResourcePermissions' {principalId} -> principalId) (\s@DescribeResourcePermissions' {} a -> s {principalId = a} :: DescribeResourcePermissions)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeResourcePermissions_authenticationToken :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_authenticationToken = Lens.lens (\DescribeResourcePermissions' {authenticationToken} -> authenticationToken) (\s@DescribeResourcePermissions' {} a -> s {authenticationToken = a} :: DescribeResourcePermissions) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return with this call.
describeResourcePermissions_limit :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Natural)
describeResourcePermissions_limit = Lens.lens (\DescribeResourcePermissions' {limit} -> limit) (\s@DescribeResourcePermissions' {} a -> s {limit = a} :: DescribeResourcePermissions)

-- | The marker for the next set of results. (You received this marker from a
-- previous call)
describeResourcePermissions_marker :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_marker = Lens.lens (\DescribeResourcePermissions' {marker} -> marker) (\s@DescribeResourcePermissions' {} a -> s {marker = a} :: DescribeResourcePermissions)

-- | The ID of the resource.
describeResourcePermissions_resourceId :: Lens.Lens' DescribeResourcePermissions Prelude.Text
describeResourcePermissions_resourceId = Lens.lens (\DescribeResourcePermissions' {resourceId} -> resourceId) (\s@DescribeResourcePermissions' {} a -> s {resourceId = a} :: DescribeResourcePermissions)

instance Core.AWSPager DescribeResourcePermissions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeResourcePermissionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourcePermissionsResponse_principals
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeResourcePermissions_marker
          Lens..~ rs
          Lens.^? describeResourcePermissionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeResourcePermissions where
  type
    AWSResponse DescribeResourcePermissions =
      DescribeResourcePermissionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePermissionsResponse'
            Prelude.<$> (x Core..?> "Principals" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePermissions

instance Prelude.NFData DescribeResourcePermissions

instance Core.ToHeaders DescribeResourcePermissions where
  toHeaders DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeResourcePermissions where
  toPath DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/permissions"
      ]

instance Core.ToQuery DescribeResourcePermissions where
  toQuery DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "principalId" Core.=: principalId,
        "limit" Core.=: limit,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeResourcePermissionsResponse' smart constructor.
data DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse'
  { -- | The principals.
    principals :: Prelude.Maybe [Principal],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principals', 'describeResourcePermissionsResponse_principals' - The principals.
--
-- 'marker', 'describeResourcePermissionsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeResourcePermissionsResponse_httpStatus' - The response's http status code.
newDescribeResourcePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePermissionsResponse
newDescribeResourcePermissionsResponse pHttpStatus_ =
  DescribeResourcePermissionsResponse'
    { principals =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The principals.
describeResourcePermissionsResponse_principals :: Lens.Lens' DescribeResourcePermissionsResponse (Prelude.Maybe [Principal])
describeResourcePermissionsResponse_principals = Lens.lens (\DescribeResourcePermissionsResponse' {principals} -> principals) (\s@DescribeResourcePermissionsResponse' {} a -> s {principals = a} :: DescribeResourcePermissionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeResourcePermissionsResponse_marker :: Lens.Lens' DescribeResourcePermissionsResponse (Prelude.Maybe Prelude.Text)
describeResourcePermissionsResponse_marker = Lens.lens (\DescribeResourcePermissionsResponse' {marker} -> marker) (\s@DescribeResourcePermissionsResponse' {} a -> s {marker = a} :: DescribeResourcePermissionsResponse)

-- | The response's http status code.
describeResourcePermissionsResponse_httpStatus :: Lens.Lens' DescribeResourcePermissionsResponse Prelude.Int
describeResourcePermissionsResponse_httpStatus = Lens.lens (\DescribeResourcePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeResourcePermissionsResponse)

instance
  Prelude.NFData
    DescribeResourcePermissionsResponse

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
-- Module      : Amazonka.WorkDocs.DescribeResourcePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a specified resource.
--
-- This operation returns paginated results.
module Amazonka.WorkDocs.DescribeResourcePermissions
  ( -- * Creating a Request
    DescribeResourcePermissions (..),
    newDescribeResourcePermissions,

    -- * Request Lenses
    describeResourcePermissions_authenticationToken,
    describeResourcePermissions_limit,
    describeResourcePermissions_marker,
    describeResourcePermissions_principalId,
    describeResourcePermissions_resourceId,

    -- * Destructuring the Response
    DescribeResourcePermissionsResponse (..),
    newDescribeResourcePermissionsResponse,

    -- * Response Lenses
    describeResourcePermissionsResponse_marker,
    describeResourcePermissionsResponse_principals,
    describeResourcePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeResourcePermissions' smart constructor.
data DescribeResourcePermissions = DescribeResourcePermissions'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the principal to filter permissions by.
    principalId :: Prelude.Maybe Prelude.Text,
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
-- 'authenticationToken', 'describeResourcePermissions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'limit', 'describeResourcePermissions_limit' - The maximum number of items to return with this call.
--
-- 'marker', 'describeResourcePermissions_marker' - The marker for the next set of results. (You received this marker from a
-- previous call)
--
-- 'principalId', 'describeResourcePermissions_principalId' - The ID of the principal to filter permissions by.
--
-- 'resourceId', 'describeResourcePermissions_resourceId' - The ID of the resource.
newDescribeResourcePermissions ::
  -- | 'resourceId'
  Prelude.Text ->
  DescribeResourcePermissions
newDescribeResourcePermissions pResourceId_ =
  DescribeResourcePermissions'
    { authenticationToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      principalId = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
describeResourcePermissions_authenticationToken :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_authenticationToken = Lens.lens (\DescribeResourcePermissions' {authenticationToken} -> authenticationToken) (\s@DescribeResourcePermissions' {} a -> s {authenticationToken = a} :: DescribeResourcePermissions) Prelude.. Lens.mapping Data._Sensitive

-- | The maximum number of items to return with this call.
describeResourcePermissions_limit :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Natural)
describeResourcePermissions_limit = Lens.lens (\DescribeResourcePermissions' {limit} -> limit) (\s@DescribeResourcePermissions' {} a -> s {limit = a} :: DescribeResourcePermissions)

-- | The marker for the next set of results. (You received this marker from a
-- previous call)
describeResourcePermissions_marker :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_marker = Lens.lens (\DescribeResourcePermissions' {marker} -> marker) (\s@DescribeResourcePermissions' {} a -> s {marker = a} :: DescribeResourcePermissions)

-- | The ID of the principal to filter permissions by.
describeResourcePermissions_principalId :: Lens.Lens' DescribeResourcePermissions (Prelude.Maybe Prelude.Text)
describeResourcePermissions_principalId = Lens.lens (\DescribeResourcePermissions' {principalId} -> principalId) (\s@DescribeResourcePermissions' {} a -> s {principalId = a} :: DescribeResourcePermissions)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeResourcePermissions_marker
          Lens..~ rs
          Lens.^? describeResourcePermissionsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeResourcePermissions where
  type
    AWSResponse DescribeResourcePermissions =
      DescribeResourcePermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePermissionsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "Principals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePermissions where
  hashWithSalt _salt DescribeResourcePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DescribeResourcePermissions where
  rnf DescribeResourcePermissions' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DescribeResourcePermissions where
  toHeaders DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DescribeResourcePermissions where
  toPath DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Data.toBS resourceId,
        "/permissions"
      ]

instance Data.ToQuery DescribeResourcePermissions where
  toQuery DescribeResourcePermissions' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "marker" Data.=: marker,
        "principalId" Data.=: principalId
      ]

-- | /See:/ 'newDescribeResourcePermissionsResponse' smart constructor.
data DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse'
  { -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The principals.
    principals :: Prelude.Maybe [Principal],
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
-- 'marker', 'describeResourcePermissionsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'principals', 'describeResourcePermissionsResponse_principals' - The principals.
--
-- 'httpStatus', 'describeResourcePermissionsResponse_httpStatus' - The response's http status code.
newDescribeResourcePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePermissionsResponse
newDescribeResourcePermissionsResponse pHttpStatus_ =
  DescribeResourcePermissionsResponse'
    { marker =
        Prelude.Nothing,
      principals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeResourcePermissionsResponse_marker :: Lens.Lens' DescribeResourcePermissionsResponse (Prelude.Maybe Prelude.Text)
describeResourcePermissionsResponse_marker = Lens.lens (\DescribeResourcePermissionsResponse' {marker} -> marker) (\s@DescribeResourcePermissionsResponse' {} a -> s {marker = a} :: DescribeResourcePermissionsResponse)

-- | The principals.
describeResourcePermissionsResponse_principals :: Lens.Lens' DescribeResourcePermissionsResponse (Prelude.Maybe [Principal])
describeResourcePermissionsResponse_principals = Lens.lens (\DescribeResourcePermissionsResponse' {principals} -> principals) (\s@DescribeResourcePermissionsResponse' {} a -> s {principals = a} :: DescribeResourcePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeResourcePermissionsResponse_httpStatus :: Lens.Lens' DescribeResourcePermissionsResponse Prelude.Int
describeResourcePermissionsResponse_httpStatus = Lens.lens (\DescribeResourcePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeResourcePermissionsResponse)

instance
  Prelude.NFData
    DescribeResourcePermissionsResponse
  where
  rnf DescribeResourcePermissionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf httpStatus

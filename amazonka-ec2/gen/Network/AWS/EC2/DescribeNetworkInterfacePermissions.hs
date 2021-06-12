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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfacePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for your network interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInterfacePermissions
  ( -- * Creating a Request
    DescribeNetworkInterfacePermissions (..),
    newDescribeNetworkInterfacePermissions,

    -- * Request Lenses
    describeNetworkInterfacePermissions_nextToken,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_filters,

    -- * Destructuring the Response
    DescribeNetworkInterfacePermissionsResponse (..),
    newDescribeNetworkInterfacePermissionsResponse,

    -- * Response Lenses
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'newDescribeNetworkInterfacePermissions' smart constructor.
data DescribeNetworkInterfacePermissions = DescribeNetworkInterfacePermissions'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. If this parameter is not specified, up to 50 results are returned
    -- by default.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more network interface permission IDs.
    networkInterfacePermissionIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @network-interface-permission.network-interface-permission-id@ - The
    --     ID of the permission.
    --
    -- -   @network-interface-permission.network-interface-id@ - The ID of the
    --     network interface.
    --
    -- -   @network-interface-permission.aws-account-id@ - The AWS account ID.
    --
    -- -   @network-interface-permission.aws-service@ - The AWS service.
    --
    -- -   @network-interface-permission.permission@ - The type of permission
    --     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfacePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInterfacePermissions_nextToken' - The token to request the next page of results.
--
-- 'maxResults', 'describeNetworkInterfacePermissions_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. If this parameter is not specified, up to 50 results are returned
-- by default.
--
-- 'networkInterfacePermissionIds', 'describeNetworkInterfacePermissions_networkInterfacePermissionIds' - One or more network interface permission IDs.
--
-- 'filters', 'describeNetworkInterfacePermissions_filters' - One or more filters.
--
-- -   @network-interface-permission.network-interface-permission-id@ - The
--     ID of the permission.
--
-- -   @network-interface-permission.network-interface-id@ - The ID of the
--     network interface.
--
-- -   @network-interface-permission.aws-account-id@ - The AWS account ID.
--
-- -   @network-interface-permission.aws-service@ - The AWS service.
--
-- -   @network-interface-permission.permission@ - The type of permission
--     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
newDescribeNetworkInterfacePermissions ::
  DescribeNetworkInterfacePermissions
newDescribeNetworkInterfacePermissions =
  DescribeNetworkInterfacePermissions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      networkInterfacePermissionIds =
        Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request the next page of results.
describeNetworkInterfacePermissions_nextToken :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe Core.Text)
describeNetworkInterfacePermissions_nextToken = Lens.lens (\DescribeNetworkInterfacePermissions' {nextToken} -> nextToken) (\s@DescribeNetworkInterfacePermissions' {} a -> s {nextToken = a} :: DescribeNetworkInterfacePermissions)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. If this parameter is not specified, up to 50 results are returned
-- by default.
describeNetworkInterfacePermissions_maxResults :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe Core.Natural)
describeNetworkInterfacePermissions_maxResults = Lens.lens (\DescribeNetworkInterfacePermissions' {maxResults} -> maxResults) (\s@DescribeNetworkInterfacePermissions' {} a -> s {maxResults = a} :: DescribeNetworkInterfacePermissions)

-- | One or more network interface permission IDs.
describeNetworkInterfacePermissions_networkInterfacePermissionIds :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe [Core.Text])
describeNetworkInterfacePermissions_networkInterfacePermissionIds = Lens.lens (\DescribeNetworkInterfacePermissions' {networkInterfacePermissionIds} -> networkInterfacePermissionIds) (\s@DescribeNetworkInterfacePermissions' {} a -> s {networkInterfacePermissionIds = a} :: DescribeNetworkInterfacePermissions) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @network-interface-permission.network-interface-permission-id@ - The
--     ID of the permission.
--
-- -   @network-interface-permission.network-interface-id@ - The ID of the
--     network interface.
--
-- -   @network-interface-permission.aws-account-id@ - The AWS account ID.
--
-- -   @network-interface-permission.aws-service@ - The AWS service.
--
-- -   @network-interface-permission.permission@ - The type of permission
--     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
describeNetworkInterfacePermissions_filters :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe [Filter])
describeNetworkInterfacePermissions_filters = Lens.lens (\DescribeNetworkInterfacePermissions' {filters} -> filters) (\s@DescribeNetworkInterfacePermissions' {} a -> s {filters = a} :: DescribeNetworkInterfacePermissions) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeNetworkInterfacePermissions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacePermissionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacePermissionsResponse_networkInterfacePermissions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNetworkInterfacePermissions_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInterfacePermissionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeNetworkInterfacePermissions
  where
  type
    AWSResponse DescribeNetworkInterfacePermissions =
      DescribeNetworkInterfacePermissionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacePermissionsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "networkInterfacePermissions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeNetworkInterfacePermissions

instance
  Core.NFData
    DescribeNetworkInterfacePermissions

instance
  Core.ToHeaders
    DescribeNetworkInterfacePermissions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeNetworkInterfacePermissions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeNetworkInterfacePermissions
  where
  toQuery DescribeNetworkInterfacePermissions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeNetworkInterfacePermissions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "NetworkInterfacePermissionId"
              Core.<$> networkInterfacePermissionIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | Contains the output for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'newDescribeNetworkInterfacePermissionsResponse' smart constructor.
data DescribeNetworkInterfacePermissionsResponse = DescribeNetworkInterfacePermissionsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The network interface permissions.
    networkInterfacePermissions :: Core.Maybe [NetworkInterfacePermission],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfacePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInterfacePermissionsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'networkInterfacePermissions', 'describeNetworkInterfacePermissionsResponse_networkInterfacePermissions' - The network interface permissions.
--
-- 'httpStatus', 'describeNetworkInterfacePermissionsResponse_httpStatus' - The response's http status code.
newDescribeNetworkInterfacePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNetworkInterfacePermissionsResponse
newDescribeNetworkInterfacePermissionsResponse
  pHttpStatus_ =
    DescribeNetworkInterfacePermissionsResponse'
      { nextToken =
          Core.Nothing,
        networkInterfacePermissions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results.
describeNetworkInterfacePermissionsResponse_nextToken :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Core.Maybe Core.Text)
describeNetworkInterfacePermissionsResponse_nextToken = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {nextToken = a} :: DescribeNetworkInterfacePermissionsResponse)

-- | The network interface permissions.
describeNetworkInterfacePermissionsResponse_networkInterfacePermissions :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Core.Maybe [NetworkInterfacePermission])
describeNetworkInterfacePermissionsResponse_networkInterfacePermissions = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {networkInterfacePermissions} -> networkInterfacePermissions) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {networkInterfacePermissions = a} :: DescribeNetworkInterfacePermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNetworkInterfacePermissionsResponse_httpStatus :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse Core.Int
describeNetworkInterfacePermissionsResponse_httpStatus = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInterfacePermissionsResponse)

instance
  Core.NFData
    DescribeNetworkInterfacePermissionsResponse

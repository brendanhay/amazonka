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
-- Module      : Amazonka.EC2.DescribeNetworkInterfacePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for your network interfaces.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNetworkInterfacePermissions
  ( -- * Creating a Request
    DescribeNetworkInterfacePermissions (..),
    newDescribeNetworkInterfacePermissions,

    -- * Request Lenses
    describeNetworkInterfacePermissions_filters,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_nextToken,

    -- * Destructuring the Response
    DescribeNetworkInterfacePermissionsResponse (..),
    newDescribeNetworkInterfacePermissionsResponse,

    -- * Response Lenses
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'newDescribeNetworkInterfacePermissions' smart constructor.
data DescribeNetworkInterfacePermissions = DescribeNetworkInterfacePermissions'
  { -- | One or more filters.
    --
    -- -   @network-interface-permission.network-interface-permission-id@ - The
    --     ID of the permission.
    --
    -- -   @network-interface-permission.network-interface-id@ - The ID of the
    --     network interface.
    --
    -- -   @network-interface-permission.aws-account-id@ - The Amazon Web
    --     Services account ID.
    --
    -- -   @network-interface-permission.aws-service@ - The Amazon Web Service.
    --
    -- -   @network-interface-permission.permission@ - The type of permission
    --     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. If this parameter is not specified, up to 50 results are returned
    -- by default.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The network interface permission IDs.
    networkInterfacePermissionIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfacePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeNetworkInterfacePermissions_filters' - One or more filters.
--
-- -   @network-interface-permission.network-interface-permission-id@ - The
--     ID of the permission.
--
-- -   @network-interface-permission.network-interface-id@ - The ID of the
--     network interface.
--
-- -   @network-interface-permission.aws-account-id@ - The Amazon Web
--     Services account ID.
--
-- -   @network-interface-permission.aws-service@ - The Amazon Web Service.
--
-- -   @network-interface-permission.permission@ - The type of permission
--     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
--
-- 'maxResults', 'describeNetworkInterfacePermissions_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. If this parameter is not specified, up to 50 results are returned
-- by default.
--
-- 'networkInterfacePermissionIds', 'describeNetworkInterfacePermissions_networkInterfacePermissionIds' - The network interface permission IDs.
--
-- 'nextToken', 'describeNetworkInterfacePermissions_nextToken' - The token to request the next page of results.
newDescribeNetworkInterfacePermissions ::
  DescribeNetworkInterfacePermissions
newDescribeNetworkInterfacePermissions =
  DescribeNetworkInterfacePermissions'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInterfacePermissionIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more filters.
--
-- -   @network-interface-permission.network-interface-permission-id@ - The
--     ID of the permission.
--
-- -   @network-interface-permission.network-interface-id@ - The ID of the
--     network interface.
--
-- -   @network-interface-permission.aws-account-id@ - The Amazon Web
--     Services account ID.
--
-- -   @network-interface-permission.aws-service@ - The Amazon Web Service.
--
-- -   @network-interface-permission.permission@ - The type of permission
--     (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@).
describeNetworkInterfacePermissions_filters :: Lens.Lens' DescribeNetworkInterfacePermissions (Prelude.Maybe [Filter])
describeNetworkInterfacePermissions_filters = Lens.lens (\DescribeNetworkInterfacePermissions' {filters} -> filters) (\s@DescribeNetworkInterfacePermissions' {} a -> s {filters = a} :: DescribeNetworkInterfacePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. If this parameter is not specified, up to 50 results are returned
-- by default.
describeNetworkInterfacePermissions_maxResults :: Lens.Lens' DescribeNetworkInterfacePermissions (Prelude.Maybe Prelude.Natural)
describeNetworkInterfacePermissions_maxResults = Lens.lens (\DescribeNetworkInterfacePermissions' {maxResults} -> maxResults) (\s@DescribeNetworkInterfacePermissions' {} a -> s {maxResults = a} :: DescribeNetworkInterfacePermissions)

-- | The network interface permission IDs.
describeNetworkInterfacePermissions_networkInterfacePermissionIds :: Lens.Lens' DescribeNetworkInterfacePermissions (Prelude.Maybe [Prelude.Text])
describeNetworkInterfacePermissions_networkInterfacePermissionIds = Lens.lens (\DescribeNetworkInterfacePermissions' {networkInterfacePermissionIds} -> networkInterfacePermissionIds) (\s@DescribeNetworkInterfacePermissions' {} a -> s {networkInterfacePermissionIds = a} :: DescribeNetworkInterfacePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The token to request the next page of results.
describeNetworkInterfacePermissions_nextToken :: Lens.Lens' DescribeNetworkInterfacePermissions (Prelude.Maybe Prelude.Text)
describeNetworkInterfacePermissions_nextToken = Lens.lens (\DescribeNetworkInterfacePermissions' {nextToken} -> nextToken) (\s@DescribeNetworkInterfacePermissions' {} a -> s {nextToken = a} :: DescribeNetworkInterfacePermissions)

instance
  Core.AWSPager
    DescribeNetworkInterfacePermissions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacePermissionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacePermissionsResponse_networkInterfacePermissions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNetworkInterfacePermissions_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInterfacePermissionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNetworkInterfacePermissions
  where
  type
    AWSResponse DescribeNetworkInterfacePermissions =
      DescribeNetworkInterfacePermissionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacePermissionsResponse'
            Prelude.<$> ( x Data..@? "networkInterfacePermissions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInterfacePermissions
  where
  hashWithSalt
    _salt
    DescribeNetworkInterfacePermissions' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` networkInterfacePermissionIds
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeNetworkInterfacePermissions
  where
  rnf DescribeNetworkInterfacePermissions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf networkInterfacePermissionIds
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeNetworkInterfacePermissions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeNetworkInterfacePermissions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNetworkInterfacePermissions
  where
  toQuery DescribeNetworkInterfacePermissions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNetworkInterfacePermissions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        Data.toQuery
          ( Data.toQueryList "NetworkInterfacePermissionId"
              Prelude.<$> networkInterfacePermissionIds
          ),
        "NextToken" Data.=: nextToken
      ]

-- | Contains the output for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'newDescribeNetworkInterfacePermissionsResponse' smart constructor.
data DescribeNetworkInterfacePermissionsResponse = DescribeNetworkInterfacePermissionsResponse'
  { -- | The network interface permissions.
    networkInterfacePermissions :: Prelude.Maybe [NetworkInterfacePermission],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfacePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfacePermissions', 'describeNetworkInterfacePermissionsResponse_networkInterfacePermissions' - The network interface permissions.
--
-- 'nextToken', 'describeNetworkInterfacePermissionsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'describeNetworkInterfacePermissionsResponse_httpStatus' - The response's http status code.
newDescribeNetworkInterfacePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInterfacePermissionsResponse
newDescribeNetworkInterfacePermissionsResponse
  pHttpStatus_ =
    DescribeNetworkInterfacePermissionsResponse'
      { networkInterfacePermissions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The network interface permissions.
describeNetworkInterfacePermissionsResponse_networkInterfacePermissions :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Prelude.Maybe [NetworkInterfacePermission])
describeNetworkInterfacePermissionsResponse_networkInterfacePermissions = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {networkInterfacePermissions} -> networkInterfacePermissions) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {networkInterfacePermissions = a} :: DescribeNetworkInterfacePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
describeNetworkInterfacePermissionsResponse_nextToken :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Prelude.Maybe Prelude.Text)
describeNetworkInterfacePermissionsResponse_nextToken = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {nextToken = a} :: DescribeNetworkInterfacePermissionsResponse)

-- | The response's http status code.
describeNetworkInterfacePermissionsResponse_httpStatus :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse Prelude.Int
describeNetworkInterfacePermissionsResponse_httpStatus = Lens.lens (\DescribeNetworkInterfacePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInterfacePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInterfacePermissionsResponse)

instance
  Prelude.NFData
    DescribeNetworkInterfacePermissionsResponse
  where
  rnf DescribeNetworkInterfacePermissionsResponse' {..} =
    Prelude.rnf networkInterfacePermissions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

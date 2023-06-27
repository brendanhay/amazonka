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
-- Module      : Amazonka.EC2.DescribeInstanceConnectEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EC2 Instance Connect Endpoints or all EC2
-- Instance Connect Endpoints.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInstanceConnectEndpoints
  ( -- * Creating a Request
    DescribeInstanceConnectEndpoints (..),
    newDescribeInstanceConnectEndpoints,

    -- * Request Lenses
    describeInstanceConnectEndpoints_dryRun,
    describeInstanceConnectEndpoints_filters,
    describeInstanceConnectEndpoints_instanceConnectEndpointIds,
    describeInstanceConnectEndpoints_maxResults,
    describeInstanceConnectEndpoints_nextToken,

    -- * Destructuring the Response
    DescribeInstanceConnectEndpointsResponse (..),
    newDescribeInstanceConnectEndpointsResponse,

    -- * Response Lenses
    describeInstanceConnectEndpointsResponse_instanceConnectEndpoints,
    describeInstanceConnectEndpointsResponse_nextToken,
    describeInstanceConnectEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceConnectEndpoints' smart constructor.
data DescribeInstanceConnectEndpoints = DescribeInstanceConnectEndpoints'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @instance-connect-endpoint-id@ - The ID of the EC2 Instance Connect
    --     Endpoint.
    --
    -- -   @state@ - The state of the EC2 Instance Connect Endpoint
    --     (@create-in-progress@ | @create-complete@ | @create-failed@ |
    --     @delete-in-progress@ | @delete-complete@ | @delete-failed@).
    --
    -- -   @subnet-id@ - The ID of the subnet in which the EC2 Instance Connect
    --     Endpoint was created.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @tag-value@ - The value of a tag assigned to the resource. Use this
    --     filter to find all resources that have a tag with a specific value,
    --     regardless of tag key.
    --
    -- -   @vpc-id@ - The ID of the VPC in which the EC2 Instance Connect
    --     Endpoint was created.
    filters :: Prelude.Maybe [Filter],
    -- | One or more EC2 Instance Connect Endpoint IDs.
    instanceConnectEndpointIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceConnectEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInstanceConnectEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeInstanceConnectEndpoints_filters' - One or more filters.
--
-- -   @instance-connect-endpoint-id@ - The ID of the EC2 Instance Connect
--     Endpoint.
--
-- -   @state@ - The state of the EC2 Instance Connect Endpoint
--     (@create-in-progress@ | @create-complete@ | @create-failed@ |
--     @delete-in-progress@ | @delete-complete@ | @delete-failed@).
--
-- -   @subnet-id@ - The ID of the subnet in which the EC2 Instance Connect
--     Endpoint was created.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific value,
--     regardless of tag key.
--
-- -   @vpc-id@ - The ID of the VPC in which the EC2 Instance Connect
--     Endpoint was created.
--
-- 'instanceConnectEndpointIds', 'describeInstanceConnectEndpoints_instanceConnectEndpointIds' - One or more EC2 Instance Connect Endpoint IDs.
--
-- 'maxResults', 'describeInstanceConnectEndpoints_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeInstanceConnectEndpoints_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
newDescribeInstanceConnectEndpoints ::
  DescribeInstanceConnectEndpoints
newDescribeInstanceConnectEndpoints =
  DescribeInstanceConnectEndpoints'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceConnectEndpointIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceConnectEndpoints_dryRun :: Lens.Lens' DescribeInstanceConnectEndpoints (Prelude.Maybe Prelude.Bool)
describeInstanceConnectEndpoints_dryRun = Lens.lens (\DescribeInstanceConnectEndpoints' {dryRun} -> dryRun) (\s@DescribeInstanceConnectEndpoints' {} a -> s {dryRun = a} :: DescribeInstanceConnectEndpoints)

-- | One or more filters.
--
-- -   @instance-connect-endpoint-id@ - The ID of the EC2 Instance Connect
--     Endpoint.
--
-- -   @state@ - The state of the EC2 Instance Connect Endpoint
--     (@create-in-progress@ | @create-complete@ | @create-failed@ |
--     @delete-in-progress@ | @delete-complete@ | @delete-failed@).
--
-- -   @subnet-id@ - The ID of the subnet in which the EC2 Instance Connect
--     Endpoint was created.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific value,
--     regardless of tag key.
--
-- -   @vpc-id@ - The ID of the VPC in which the EC2 Instance Connect
--     Endpoint was created.
describeInstanceConnectEndpoints_filters :: Lens.Lens' DescribeInstanceConnectEndpoints (Prelude.Maybe [Filter])
describeInstanceConnectEndpoints_filters = Lens.lens (\DescribeInstanceConnectEndpoints' {filters} -> filters) (\s@DescribeInstanceConnectEndpoints' {} a -> s {filters = a} :: DescribeInstanceConnectEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | One or more EC2 Instance Connect Endpoint IDs.
describeInstanceConnectEndpoints_instanceConnectEndpointIds :: Lens.Lens' DescribeInstanceConnectEndpoints (Prelude.Maybe [Prelude.Text])
describeInstanceConnectEndpoints_instanceConnectEndpointIds = Lens.lens (\DescribeInstanceConnectEndpoints' {instanceConnectEndpointIds} -> instanceConnectEndpointIds) (\s@DescribeInstanceConnectEndpoints' {} a -> s {instanceConnectEndpointIds = a} :: DescribeInstanceConnectEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeInstanceConnectEndpoints_maxResults :: Lens.Lens' DescribeInstanceConnectEndpoints (Prelude.Maybe Prelude.Natural)
describeInstanceConnectEndpoints_maxResults = Lens.lens (\DescribeInstanceConnectEndpoints' {maxResults} -> maxResults) (\s@DescribeInstanceConnectEndpoints' {} a -> s {maxResults = a} :: DescribeInstanceConnectEndpoints)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeInstanceConnectEndpoints_nextToken :: Lens.Lens' DescribeInstanceConnectEndpoints (Prelude.Maybe Prelude.Text)
describeInstanceConnectEndpoints_nextToken = Lens.lens (\DescribeInstanceConnectEndpoints' {nextToken} -> nextToken) (\s@DescribeInstanceConnectEndpoints' {} a -> s {nextToken = a} :: DescribeInstanceConnectEndpoints)

instance
  Core.AWSPager
    DescribeInstanceConnectEndpoints
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceConnectEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceConnectEndpointsResponse_instanceConnectEndpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeInstanceConnectEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeInstanceConnectEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstanceConnectEndpoints
  where
  type
    AWSResponse DescribeInstanceConnectEndpoints =
      DescribeInstanceConnectEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceConnectEndpointsResponse'
            Prelude.<$> ( x
                            Data..@? "instanceConnectEndpointSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceConnectEndpoints
  where
  hashWithSalt
    _salt
    DescribeInstanceConnectEndpoints' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` instanceConnectEndpointIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeInstanceConnectEndpoints
  where
  rnf DescribeInstanceConnectEndpoints' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf instanceConnectEndpointIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeInstanceConnectEndpoints
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstanceConnectEndpoints where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeInstanceConnectEndpoints
  where
  toQuery DescribeInstanceConnectEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeInstanceConnectEndpoints" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "InstanceConnectEndpointId"
              Prelude.<$> instanceConnectEndpointIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeInstanceConnectEndpointsResponse' smart constructor.
data DescribeInstanceConnectEndpointsResponse = DescribeInstanceConnectEndpointsResponse'
  { -- | Information about the EC2 Instance Connect Endpoints.
    instanceConnectEndpoints :: Prelude.Maybe [Ec2InstanceConnectEndpoint],
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceConnectEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceConnectEndpoints', 'describeInstanceConnectEndpointsResponse_instanceConnectEndpoints' - Information about the EC2 Instance Connect Endpoints.
--
-- 'nextToken', 'describeInstanceConnectEndpointsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'httpStatus', 'describeInstanceConnectEndpointsResponse_httpStatus' - The response's http status code.
newDescribeInstanceConnectEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceConnectEndpointsResponse
newDescribeInstanceConnectEndpointsResponse
  pHttpStatus_ =
    DescribeInstanceConnectEndpointsResponse'
      { instanceConnectEndpoints =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the EC2 Instance Connect Endpoints.
describeInstanceConnectEndpointsResponse_instanceConnectEndpoints :: Lens.Lens' DescribeInstanceConnectEndpointsResponse (Prelude.Maybe [Ec2InstanceConnectEndpoint])
describeInstanceConnectEndpointsResponse_instanceConnectEndpoints = Lens.lens (\DescribeInstanceConnectEndpointsResponse' {instanceConnectEndpoints} -> instanceConnectEndpoints) (\s@DescribeInstanceConnectEndpointsResponse' {} a -> s {instanceConnectEndpoints = a} :: DescribeInstanceConnectEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeInstanceConnectEndpointsResponse_nextToken :: Lens.Lens' DescribeInstanceConnectEndpointsResponse (Prelude.Maybe Prelude.Text)
describeInstanceConnectEndpointsResponse_nextToken = Lens.lens (\DescribeInstanceConnectEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeInstanceConnectEndpointsResponse' {} a -> s {nextToken = a} :: DescribeInstanceConnectEndpointsResponse)

-- | The response's http status code.
describeInstanceConnectEndpointsResponse_httpStatus :: Lens.Lens' DescribeInstanceConnectEndpointsResponse Prelude.Int
describeInstanceConnectEndpointsResponse_httpStatus = Lens.lens (\DescribeInstanceConnectEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceConnectEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceConnectEndpointsResponse)

instance
  Prelude.NFData
    DescribeInstanceConnectEndpointsResponse
  where
  rnf DescribeInstanceConnectEndpointsResponse' {..} =
    Prelude.rnf instanceConnectEndpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

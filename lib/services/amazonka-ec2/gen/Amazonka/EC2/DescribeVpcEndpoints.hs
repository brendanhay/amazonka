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
-- Module      : Amazonka.EC2.DescribeVpcEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC endpoints.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcEndpoints
  ( -- * Creating a Request
    DescribeVpcEndpoints (..),
    newDescribeVpcEndpoints,

    -- * Request Lenses
    describeVpcEndpoints_dryRun,
    describeVpcEndpoints_filters,
    describeVpcEndpoints_maxResults,
    describeVpcEndpoints_nextToken,
    describeVpcEndpoints_vpcEndpointIds,

    -- * Destructuring the Response
    DescribeVpcEndpointsResponse (..),
    newDescribeVpcEndpointsResponse,

    -- * Response Lenses
    describeVpcEndpointsResponse_nextToken,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeVpcEndpoints.
--
-- /See:/ 'newDescribeVpcEndpoints' smart constructor.
data DescribeVpcEndpoints = DescribeVpcEndpoints'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
    --
    -- -   @service-name@ - The name of the service.
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
    -- -   @vpc-id@ - The ID of the VPC in which the endpoint resides.
    --
    -- -   @vpc-endpoint-id@ - The ID of the endpoint.
    --
    -- -   @vpc-endpoint-state@ - The state of the endpoint
    --     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
    --     @deleted@ | @rejected@ | @failed@).
    --
    -- -   @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ |
    --     @Gateway@ | @GatewayLoadBalancer@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of items to return for this request. The request
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    --
    -- Constraint: If the value is greater than 1,000, we return only 1,000
    -- items.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a prior call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more endpoint IDs.
    vpcEndpointIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcEndpoints_filters' - One or more filters.
--
-- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
--
-- -   @service-name@ - The name of the service.
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
-- -   @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ |
--     @Gateway@ | @GatewayLoadBalancer@).
--
-- 'maxResults', 'describeVpcEndpoints_maxResults' - The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000
-- items.
--
-- 'nextToken', 'describeVpcEndpoints_nextToken' - The token for the next set of items to return. (You received this token
-- from a prior call.)
--
-- 'vpcEndpointIds', 'describeVpcEndpoints_vpcEndpointIds' - One or more endpoint IDs.
newDescribeVpcEndpoints ::
  DescribeVpcEndpoints
newDescribeVpcEndpoints =
  DescribeVpcEndpoints'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcEndpointIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpoints_dryRun :: Lens.Lens' DescribeVpcEndpoints (Prelude.Maybe Prelude.Bool)
describeVpcEndpoints_dryRun = Lens.lens (\DescribeVpcEndpoints' {dryRun} -> dryRun) (\s@DescribeVpcEndpoints' {} a -> s {dryRun = a} :: DescribeVpcEndpoints)

-- | One or more filters.
--
-- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
--
-- -   @service-name@ - The name of the service.
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
-- -   @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ |
--     @Gateway@ | @GatewayLoadBalancer@).
describeVpcEndpoints_filters :: Lens.Lens' DescribeVpcEndpoints (Prelude.Maybe [Filter])
describeVpcEndpoints_filters = Lens.lens (\DescribeVpcEndpoints' {filters} -> filters) (\s@DescribeVpcEndpoints' {} a -> s {filters = a} :: DescribeVpcEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000
-- items.
describeVpcEndpoints_maxResults :: Lens.Lens' DescribeVpcEndpoints (Prelude.Maybe Prelude.Int)
describeVpcEndpoints_maxResults = Lens.lens (\DescribeVpcEndpoints' {maxResults} -> maxResults) (\s@DescribeVpcEndpoints' {} a -> s {maxResults = a} :: DescribeVpcEndpoints)

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
describeVpcEndpoints_nextToken :: Lens.Lens' DescribeVpcEndpoints (Prelude.Maybe Prelude.Text)
describeVpcEndpoints_nextToken = Lens.lens (\DescribeVpcEndpoints' {nextToken} -> nextToken) (\s@DescribeVpcEndpoints' {} a -> s {nextToken = a} :: DescribeVpcEndpoints)

-- | One or more endpoint IDs.
describeVpcEndpoints_vpcEndpointIds :: Lens.Lens' DescribeVpcEndpoints (Prelude.Maybe [Prelude.Text])
describeVpcEndpoints_vpcEndpointIds = Lens.lens (\DescribeVpcEndpoints' {vpcEndpointIds} -> vpcEndpointIds) (\s@DescribeVpcEndpoints' {} a -> s {vpcEndpointIds = a} :: DescribeVpcEndpoints) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeVpcEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointsResponse_vpcEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVpcEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeVpcEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeVpcEndpoints where
  type
    AWSResponse DescribeVpcEndpoints =
      DescribeVpcEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "vpcEndpointSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVpcEndpoints where
  hashWithSalt _salt DescribeVpcEndpoints' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData DescribeVpcEndpoints where
  rnf DescribeVpcEndpoints' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcEndpointIds

instance Data.ToHeaders DescribeVpcEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpcEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcEndpoints where
  toQuery DescribeVpcEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVpcEndpoints" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "VpcEndpointId"
              Prelude.<$> vpcEndpointIds
          )
      ]

-- | Contains the output of DescribeVpcEndpoints.
--
-- /See:/ 'newDescribeVpcEndpointsResponse' smart constructor.
data DescribeVpcEndpointsResponse = DescribeVpcEndpointsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the endpoints.
    vpcEndpoints :: Prelude.Maybe [VpcEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'vpcEndpoints', 'describeVpcEndpointsResponse_vpcEndpoints' - Information about the endpoints.
--
-- 'httpStatus', 'describeVpcEndpointsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcEndpointsResponse
newDescribeVpcEndpointsResponse pHttpStatus_ =
  DescribeVpcEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      vpcEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeVpcEndpointsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointsResponse (Prelude.Maybe Prelude.Text)
describeVpcEndpointsResponse_nextToken = Lens.lens (\DescribeVpcEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointsResponse)

-- | Information about the endpoints.
describeVpcEndpointsResponse_vpcEndpoints :: Lens.Lens' DescribeVpcEndpointsResponse (Prelude.Maybe [VpcEndpoint])
describeVpcEndpointsResponse_vpcEndpoints = Lens.lens (\DescribeVpcEndpointsResponse' {vpcEndpoints} -> vpcEndpoints) (\s@DescribeVpcEndpointsResponse' {} a -> s {vpcEndpoints = a} :: DescribeVpcEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcEndpointsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointsResponse Prelude.Int
describeVpcEndpointsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointsResponse)

instance Prelude.NFData DescribeVpcEndpointsResponse where
  rnf DescribeVpcEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcEndpoints
      `Prelude.seq` Prelude.rnf httpStatus

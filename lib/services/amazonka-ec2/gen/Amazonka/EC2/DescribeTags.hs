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
-- Module      : Amazonka.EC2.DescribeTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for your EC2 resources.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tag your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_dryRun,
    describeTags_filters,
    describeTags_maxResults,
    describeTags_nextToken,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @key@ - The tag key.
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The resource type (@customer-gateway@ |
    --     @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ |
    --     @fpga-image@ | @host-reservation@ | @image@ | @instance@ |
    --     @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ |
    --     @network-acl@ | @network-interface@ | @placement-group@ |
    --     @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@
    --     | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ |
    --     @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ |
    --     @vpn-connection@ | @vpn-gateway@).
    --
    -- -   @tag@:\<key> - The key\/value combination of the tag. For example,
    --     specify \"tag:Owner\" for the filter name and \"TeamA\" for the
    --     filter value to find resources with the tag \"Owner=TeamA\".
    --
    -- -   @value@ - The tag value.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of items to return for this request. This value can
    -- be between 5 and 1000. To get the next page of items, make another
    -- request with the token returned in the output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTags_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTags_filters' - The filters.
--
-- -   @key@ - The tag key.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type (@customer-gateway@ |
--     @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ |
--     @fpga-image@ | @host-reservation@ | @image@ | @instance@ |
--     @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ |
--     @network-acl@ | @network-interface@ | @placement-group@ |
--     @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@
--     | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ |
--     @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ |
--     @vpn-connection@ | @vpn-gateway@).
--
-- -   @tag@:\<key> - The key\/value combination of the tag. For example,
--     specify \"tag:Owner\" for the filter name and \"TeamA\" for the
--     filter value to find resources with the tag \"Owner=TeamA\".
--
-- -   @value@ - The tag value.
--
-- 'maxResults', 'describeTags_maxResults' - The maximum number of items to return for this request. This value can
-- be between 5 and 1000. To get the next page of items, make another
-- request with the token returned in the output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeTags_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTags_dryRun :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Bool)
describeTags_dryRun = Lens.lens (\DescribeTags' {dryRun} -> dryRun) (\s@DescribeTags' {} a -> s {dryRun = a} :: DescribeTags)

-- | The filters.
--
-- -   @key@ - The tag key.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type (@customer-gateway@ |
--     @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ |
--     @fpga-image@ | @host-reservation@ | @image@ | @instance@ |
--     @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ |
--     @network-acl@ | @network-interface@ | @placement-group@ |
--     @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@
--     | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ |
--     @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ |
--     @vpn-connection@ | @vpn-gateway@).
--
-- -   @tag@:\<key> - The key\/value combination of the tag. For example,
--     specify \"tag:Owner\" for the filter name and \"TeamA\" for the
--     filter value to find resources with the tag \"Owner=TeamA\".
--
-- -   @value@ - The tag value.
describeTags_filters :: Lens.Lens' DescribeTags (Prelude.Maybe [Filter])
describeTags_filters = Lens.lens (\DescribeTags' {filters} -> filters) (\s@DescribeTags' {} a -> s {filters = a} :: DescribeTags) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. This value can
-- be between 5 and 1000. To get the next page of items, make another
-- request with the token returned in the output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeTags_maxResults :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Int)
describeTags_maxResults = Lens.lens (\DescribeTags' {maxResults} -> maxResults) (\s@DescribeTags' {} a -> s {maxResults = a} :: DescribeTags)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeTags_nextToken :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_nextToken = Lens.lens (\DescribeTags' {nextToken} -> nextToken) (\s@DescribeTags' {} a -> s {nextToken = a} :: DescribeTags)

instance Core.AWSPager DescribeTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_tags
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeTags_nextToken
          Lens..~ rs
          Lens.^? describeTagsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "tagSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags where
  hashWithSalt _salt DescribeTags' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeTags where
  rnf DescribeTags' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [TagDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTagsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'tags', 'describeTagsResponse_tags' - The tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { nextToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeTagsResponse_nextToken :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe Prelude.Text)
describeTagsResponse_nextToken = Lens.lens (\DescribeTagsResponse' {nextToken} -> nextToken) (\s@DescribeTagsResponse' {} a -> s {nextToken = a} :: DescribeTagsResponse)

-- | The tags.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [TagDescription])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse where
  rnf DescribeTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

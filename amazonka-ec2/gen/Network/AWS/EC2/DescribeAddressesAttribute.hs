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
-- Module      : Network.AWS.EC2.DescribeAddressesAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes of the specified Elastic IP addresses. For
-- requirements, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html#Using_Elastic_Addressing_Reverse_DNS Using reverse DNS for email applications>.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeAddressesAttribute
  ( -- * Creating a Request
    DescribeAddressesAttribute (..),
    newDescribeAddressesAttribute,

    -- * Request Lenses
    describeAddressesAttribute_nextToken,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_allocationIds,

    -- * Destructuring the Response
    DescribeAddressesAttributeResponse (..),
    newDescribeAddressesAttributeResponse,

    -- * Response Lenses
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAddressesAttribute' smart constructor.
data DescribeAddressesAttribute = DescribeAddressesAttribute'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The attribute of the IP address.
    attribute :: Core.Maybe AddressAttributeName,
    -- | [EC2-VPC] The allocation IDs.
    allocationIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddressesAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddressesAttribute_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeAddressesAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeAddressesAttribute_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'attribute', 'describeAddressesAttribute_attribute' - The attribute of the IP address.
--
-- 'allocationIds', 'describeAddressesAttribute_allocationIds' - [EC2-VPC] The allocation IDs.
newDescribeAddressesAttribute ::
  DescribeAddressesAttribute
newDescribeAddressesAttribute =
  DescribeAddressesAttribute'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      attribute = Core.Nothing,
      allocationIds = Core.Nothing
    }

-- | The token for the next page of results.
describeAddressesAttribute_nextToken :: Lens.Lens' DescribeAddressesAttribute (Core.Maybe Core.Text)
describeAddressesAttribute_nextToken = Lens.lens (\DescribeAddressesAttribute' {nextToken} -> nextToken) (\s@DescribeAddressesAttribute' {} a -> s {nextToken = a} :: DescribeAddressesAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAddressesAttribute_dryRun :: Lens.Lens' DescribeAddressesAttribute (Core.Maybe Core.Bool)
describeAddressesAttribute_dryRun = Lens.lens (\DescribeAddressesAttribute' {dryRun} -> dryRun) (\s@DescribeAddressesAttribute' {} a -> s {dryRun = a} :: DescribeAddressesAttribute)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeAddressesAttribute_maxResults :: Lens.Lens' DescribeAddressesAttribute (Core.Maybe Core.Natural)
describeAddressesAttribute_maxResults = Lens.lens (\DescribeAddressesAttribute' {maxResults} -> maxResults) (\s@DescribeAddressesAttribute' {} a -> s {maxResults = a} :: DescribeAddressesAttribute)

-- | The attribute of the IP address.
describeAddressesAttribute_attribute :: Lens.Lens' DescribeAddressesAttribute (Core.Maybe AddressAttributeName)
describeAddressesAttribute_attribute = Lens.lens (\DescribeAddressesAttribute' {attribute} -> attribute) (\s@DescribeAddressesAttribute' {} a -> s {attribute = a} :: DescribeAddressesAttribute)

-- | [EC2-VPC] The allocation IDs.
describeAddressesAttribute_allocationIds :: Lens.Lens' DescribeAddressesAttribute (Core.Maybe [Core.Text])
describeAddressesAttribute_allocationIds = Lens.lens (\DescribeAddressesAttribute' {allocationIds} -> allocationIds) (\s@DescribeAddressesAttribute' {} a -> s {allocationIds = a} :: DescribeAddressesAttribute) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeAddressesAttribute where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddressesAttributeResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddressesAttributeResponse_addresses
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAddressesAttribute_nextToken
          Lens..~ rs
          Lens.^? describeAddressesAttributeResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAddressesAttribute where
  type
    AWSResponse DescribeAddressesAttribute =
      DescribeAddressesAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAddressesAttributeResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "addressSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddressesAttribute

instance Core.NFData DescribeAddressesAttribute

instance Core.ToHeaders DescribeAddressesAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAddressesAttribute where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAddressesAttribute where
  toQuery DescribeAddressesAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAddressesAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "Attribute" Core.=: attribute,
        Core.toQuery
          ( Core.toQueryList "AllocationId"
              Core.<$> allocationIds
          )
      ]

-- | /See:/ 'newDescribeAddressesAttributeResponse' smart constructor.
data DescribeAddressesAttributeResponse = DescribeAddressesAttributeResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the IP addresses.
    addresses :: Core.Maybe [AddressAttribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddressesAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddressesAttributeResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'addresses', 'describeAddressesAttributeResponse_addresses' - Information about the IP addresses.
--
-- 'httpStatus', 'describeAddressesAttributeResponse_httpStatus' - The response's http status code.
newDescribeAddressesAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAddressesAttributeResponse
newDescribeAddressesAttributeResponse pHttpStatus_ =
  DescribeAddressesAttributeResponse'
    { nextToken =
        Core.Nothing,
      addresses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeAddressesAttributeResponse_nextToken :: Lens.Lens' DescribeAddressesAttributeResponse (Core.Maybe Core.Text)
describeAddressesAttributeResponse_nextToken = Lens.lens (\DescribeAddressesAttributeResponse' {nextToken} -> nextToken) (\s@DescribeAddressesAttributeResponse' {} a -> s {nextToken = a} :: DescribeAddressesAttributeResponse)

-- | Information about the IP addresses.
describeAddressesAttributeResponse_addresses :: Lens.Lens' DescribeAddressesAttributeResponse (Core.Maybe [AddressAttribute])
describeAddressesAttributeResponse_addresses = Lens.lens (\DescribeAddressesAttributeResponse' {addresses} -> addresses) (\s@DescribeAddressesAttributeResponse' {} a -> s {addresses = a} :: DescribeAddressesAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAddressesAttributeResponse_httpStatus :: Lens.Lens' DescribeAddressesAttributeResponse Core.Int
describeAddressesAttributeResponse_httpStatus = Lens.lens (\DescribeAddressesAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesAttributeResponse' {} a -> s {httpStatus = a} :: DescribeAddressesAttributeResponse)

instance
  Core.NFData
    DescribeAddressesAttributeResponse

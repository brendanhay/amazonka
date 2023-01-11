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
-- Module      : Amazonka.EC2.DescribeAddressesAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.DescribeAddressesAttribute
  ( -- * Creating a Request
    DescribeAddressesAttribute (..),
    newDescribeAddressesAttribute,

    -- * Request Lenses
    describeAddressesAttribute_allocationIds,
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttribute_nextToken,

    -- * Destructuring the Response
    DescribeAddressesAttributeResponse (..),
    newDescribeAddressesAttributeResponse,

    -- * Response Lenses
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAddressesAttribute' smart constructor.
data DescribeAddressesAttribute = DescribeAddressesAttribute'
  { -- | [EC2-VPC] The allocation IDs.
    allocationIds :: Prelude.Maybe [Prelude.Text],
    -- | The attribute of the IP address.
    attribute :: Prelude.Maybe AddressAttributeName,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddressesAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationIds', 'describeAddressesAttribute_allocationIds' - [EC2-VPC] The allocation IDs.
--
-- 'attribute', 'describeAddressesAttribute_attribute' - The attribute of the IP address.
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
-- 'nextToken', 'describeAddressesAttribute_nextToken' - The token for the next page of results.
newDescribeAddressesAttribute ::
  DescribeAddressesAttribute
newDescribeAddressesAttribute =
  DescribeAddressesAttribute'
    { allocationIds =
        Prelude.Nothing,
      attribute = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | [EC2-VPC] The allocation IDs.
describeAddressesAttribute_allocationIds :: Lens.Lens' DescribeAddressesAttribute (Prelude.Maybe [Prelude.Text])
describeAddressesAttribute_allocationIds = Lens.lens (\DescribeAddressesAttribute' {allocationIds} -> allocationIds) (\s@DescribeAddressesAttribute' {} a -> s {allocationIds = a} :: DescribeAddressesAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The attribute of the IP address.
describeAddressesAttribute_attribute :: Lens.Lens' DescribeAddressesAttribute (Prelude.Maybe AddressAttributeName)
describeAddressesAttribute_attribute = Lens.lens (\DescribeAddressesAttribute' {attribute} -> attribute) (\s@DescribeAddressesAttribute' {} a -> s {attribute = a} :: DescribeAddressesAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAddressesAttribute_dryRun :: Lens.Lens' DescribeAddressesAttribute (Prelude.Maybe Prelude.Bool)
describeAddressesAttribute_dryRun = Lens.lens (\DescribeAddressesAttribute' {dryRun} -> dryRun) (\s@DescribeAddressesAttribute' {} a -> s {dryRun = a} :: DescribeAddressesAttribute)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeAddressesAttribute_maxResults :: Lens.Lens' DescribeAddressesAttribute (Prelude.Maybe Prelude.Natural)
describeAddressesAttribute_maxResults = Lens.lens (\DescribeAddressesAttribute' {maxResults} -> maxResults) (\s@DescribeAddressesAttribute' {} a -> s {maxResults = a} :: DescribeAddressesAttribute)

-- | The token for the next page of results.
describeAddressesAttribute_nextToken :: Lens.Lens' DescribeAddressesAttribute (Prelude.Maybe Prelude.Text)
describeAddressesAttribute_nextToken = Lens.lens (\DescribeAddressesAttribute' {nextToken} -> nextToken) (\s@DescribeAddressesAttribute' {} a -> s {nextToken = a} :: DescribeAddressesAttribute)

instance Core.AWSPager DescribeAddressesAttribute where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddressesAttributeResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddressesAttributeResponse_addresses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAddressesAttribute_nextToken
          Lens..~ rs
          Lens.^? describeAddressesAttributeResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAddressesAttribute where
  type
    AWSResponse DescribeAddressesAttribute =
      DescribeAddressesAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAddressesAttributeResponse'
            Prelude.<$> ( x Data..@? "addressSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddressesAttribute where
  hashWithSalt _salt DescribeAddressesAttribute' {..} =
    _salt `Prelude.hashWithSalt` allocationIds
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAddressesAttribute where
  rnf DescribeAddressesAttribute' {..} =
    Prelude.rnf allocationIds
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeAddressesAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAddressesAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAddressesAttribute where
  toQuery DescribeAddressesAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAddressesAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AllocationId"
              Prelude.<$> allocationIds
          ),
        "Attribute" Data.=: attribute,
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeAddressesAttributeResponse' smart constructor.
data DescribeAddressesAttributeResponse = DescribeAddressesAttributeResponse'
  { -- | Information about the IP addresses.
    addresses :: Prelude.Maybe [AddressAttribute],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddressesAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addresses', 'describeAddressesAttributeResponse_addresses' - Information about the IP addresses.
--
-- 'nextToken', 'describeAddressesAttributeResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeAddressesAttributeResponse_httpStatus' - The response's http status code.
newDescribeAddressesAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAddressesAttributeResponse
newDescribeAddressesAttributeResponse pHttpStatus_ =
  DescribeAddressesAttributeResponse'
    { addresses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IP addresses.
describeAddressesAttributeResponse_addresses :: Lens.Lens' DescribeAddressesAttributeResponse (Prelude.Maybe [AddressAttribute])
describeAddressesAttributeResponse_addresses = Lens.lens (\DescribeAddressesAttributeResponse' {addresses} -> addresses) (\s@DescribeAddressesAttributeResponse' {} a -> s {addresses = a} :: DescribeAddressesAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeAddressesAttributeResponse_nextToken :: Lens.Lens' DescribeAddressesAttributeResponse (Prelude.Maybe Prelude.Text)
describeAddressesAttributeResponse_nextToken = Lens.lens (\DescribeAddressesAttributeResponse' {nextToken} -> nextToken) (\s@DescribeAddressesAttributeResponse' {} a -> s {nextToken = a} :: DescribeAddressesAttributeResponse)

-- | The response's http status code.
describeAddressesAttributeResponse_httpStatus :: Lens.Lens' DescribeAddressesAttributeResponse Prelude.Int
describeAddressesAttributeResponse_httpStatus = Lens.lens (\DescribeAddressesAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesAttributeResponse' {} a -> s {httpStatus = a} :: DescribeAddressesAttributeResponse)

instance
  Prelude.NFData
    DescribeAddressesAttributeResponse
  where
  rnf DescribeAddressesAttributeResponse' {..} =
    Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

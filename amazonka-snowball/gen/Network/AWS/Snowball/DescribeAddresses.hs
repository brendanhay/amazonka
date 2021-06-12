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
-- Module      : Network.AWS.Snowball.DescribeAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a specified number of @ADDRESS@ objects. Calling this API in one
-- of the US regions will return addresses from the list of all addresses
-- associated with this account in all US regions.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.DescribeAddresses
  ( -- * Creating a Request
    DescribeAddresses (..),
    newDescribeAddresses,

    -- * Request Lenses
    describeAddresses_nextToken,
    describeAddresses_maxResults,

    -- * Destructuring the Response
    DescribeAddressesResponse (..),
    newDescribeAddressesResponse,

    -- * Response Lenses
    describeAddressesResponse_nextToken,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @ADDRESS@ objects, you have the option of specifying a value
    -- for @NextToken@ as the starting point for your list of returned
    -- addresses.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of @ADDRESS@ objects to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddresses_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ADDRESS@ objects, you have the option of specifying a value
-- for @NextToken@ as the starting point for your list of returned
-- addresses.
--
-- 'maxResults', 'describeAddresses_maxResults' - The number of @ADDRESS@ objects to return.
newDescribeAddresses ::
  DescribeAddresses
newDescribeAddresses =
  DescribeAddresses'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ADDRESS@ objects, you have the option of specifying a value
-- for @NextToken@ as the starting point for your list of returned
-- addresses.
describeAddresses_nextToken :: Lens.Lens' DescribeAddresses (Core.Maybe Core.Text)
describeAddresses_nextToken = Lens.lens (\DescribeAddresses' {nextToken} -> nextToken) (\s@DescribeAddresses' {} a -> s {nextToken = a} :: DescribeAddresses)

-- | The number of @ADDRESS@ objects to return.
describeAddresses_maxResults :: Lens.Lens' DescribeAddresses (Core.Maybe Core.Natural)
describeAddresses_maxResults = Lens.lens (\DescribeAddresses' {maxResults} -> maxResults) (\s@DescribeAddresses' {} a -> s {maxResults = a} :: DescribeAddresses)

instance Core.AWSPager DescribeAddresses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddressesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddressesResponse_addresses
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAddresses_nextToken
          Lens..~ rs
          Lens.^? describeAddressesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeAddresses where
  type
    AWSResponse DescribeAddresses =
      DescribeAddressesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Addresses" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddresses

instance Core.NFData DescribeAddresses

instance Core.ToHeaders DescribeAddresses where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.DescribeAddresses" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAddresses where
  toJSON DescribeAddresses' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeAddresses where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAddresses where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @DescribeAddresses@ call, your list of
    -- returned addresses will start from this point in the array.
    nextToken :: Core.Maybe Core.Text,
    -- | The Snow device shipping addresses that were created for this account.
    addresses :: Core.Maybe [Address],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAddressesResponse_nextToken' - HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @DescribeAddresses@ call, your list of
-- returned addresses will start from this point in the array.
--
-- 'addresses', 'describeAddressesResponse_addresses' - The Snow device shipping addresses that were created for this account.
--
-- 'httpStatus', 'describeAddressesResponse_httpStatus' - The response's http status code.
newDescribeAddressesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAddressesResponse
newDescribeAddressesResponse pHttpStatus_ =
  DescribeAddressesResponse'
    { nextToken =
        Core.Nothing,
      addresses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @DescribeAddresses@ call, your list of
-- returned addresses will start from this point in the array.
describeAddressesResponse_nextToken :: Lens.Lens' DescribeAddressesResponse (Core.Maybe Core.Text)
describeAddressesResponse_nextToken = Lens.lens (\DescribeAddressesResponse' {nextToken} -> nextToken) (\s@DescribeAddressesResponse' {} a -> s {nextToken = a} :: DescribeAddressesResponse)

-- | The Snow device shipping addresses that were created for this account.
describeAddressesResponse_addresses :: Lens.Lens' DescribeAddressesResponse (Core.Maybe [Address])
describeAddressesResponse_addresses = Lens.lens (\DescribeAddressesResponse' {addresses} -> addresses) (\s@DescribeAddressesResponse' {} a -> s {addresses = a} :: DescribeAddressesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAddressesResponse_httpStatus :: Lens.Lens' DescribeAddressesResponse Core.Int
describeAddressesResponse_httpStatus = Lens.lens (\DescribeAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesResponse' {} a -> s {httpStatus = a} :: DescribeAddressesResponse)

instance Core.NFData DescribeAddressesResponse

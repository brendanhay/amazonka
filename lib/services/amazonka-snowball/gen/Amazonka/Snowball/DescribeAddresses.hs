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
-- Module      : Amazonka.Snowball.DescribeAddresses
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Snowball.DescribeAddresses
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @ADDRESS@ objects, you have the option of specifying a value
    -- for @NextToken@ as the starting point for your list of returned
    -- addresses.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @ADDRESS@ objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ADDRESS@ objects, you have the option of specifying a value
-- for @NextToken@ as the starting point for your list of returned
-- addresses.
describeAddresses_nextToken :: Lens.Lens' DescribeAddresses (Prelude.Maybe Prelude.Text)
describeAddresses_nextToken = Lens.lens (\DescribeAddresses' {nextToken} -> nextToken) (\s@DescribeAddresses' {} a -> s {nextToken = a} :: DescribeAddresses)

-- | The number of @ADDRESS@ objects to return.
describeAddresses_maxResults :: Lens.Lens' DescribeAddresses (Prelude.Maybe Prelude.Natural)
describeAddresses_maxResults = Lens.lens (\DescribeAddresses' {maxResults} -> maxResults) (\s@DescribeAddresses' {} a -> s {maxResults = a} :: DescribeAddresses)

instance Core.AWSPager DescribeAddresses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAddressesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAddressesResponse_addresses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAddresses_nextToken
          Lens..~ rs
          Lens.^? describeAddressesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAddresses where
  type
    AWSResponse DescribeAddresses =
      DescribeAddressesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Addresses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddresses where
  hashWithSalt _salt DescribeAddresses' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeAddresses where
  rnf DescribeAddresses' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeAddresses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.DescribeAddresses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAddresses where
  toJSON DescribeAddresses' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeAddresses where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAddresses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @DescribeAddresses@ call, your list of
    -- returned addresses will start from this point in the array.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Snow device shipping addresses that were created for this account.
    addresses :: Prelude.Maybe [Address],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAddressesResponse
newDescribeAddressesResponse pHttpStatus_ =
  DescribeAddressesResponse'
    { nextToken =
        Prelude.Nothing,
      addresses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @DescribeAddresses@ call, your list of
-- returned addresses will start from this point in the array.
describeAddressesResponse_nextToken :: Lens.Lens' DescribeAddressesResponse (Prelude.Maybe Prelude.Text)
describeAddressesResponse_nextToken = Lens.lens (\DescribeAddressesResponse' {nextToken} -> nextToken) (\s@DescribeAddressesResponse' {} a -> s {nextToken = a} :: DescribeAddressesResponse)

-- | The Snow device shipping addresses that were created for this account.
describeAddressesResponse_addresses :: Lens.Lens' DescribeAddressesResponse (Prelude.Maybe [Address])
describeAddressesResponse_addresses = Lens.lens (\DescribeAddressesResponse' {addresses} -> addresses) (\s@DescribeAddressesResponse' {} a -> s {addresses = a} :: DescribeAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAddressesResponse_httpStatus :: Lens.Lens' DescribeAddressesResponse Prelude.Int
describeAddressesResponse_httpStatus = Lens.lens (\DescribeAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesResponse' {} a -> s {httpStatus = a} :: DescribeAddressesResponse)

instance Prelude.NFData DescribeAddressesResponse where
  rnf DescribeAddressesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf httpStatus

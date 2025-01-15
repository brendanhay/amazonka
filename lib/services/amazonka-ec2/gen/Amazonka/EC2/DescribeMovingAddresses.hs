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
-- Module      : Amazonka.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC
-- platform, or that are being restored to the EC2-Classic platform. This
-- request does not return information about any other Elastic IP addresses
-- in your account.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeMovingAddresses
  ( -- * Creating a Request
    DescribeMovingAddresses (..),
    newDescribeMovingAddresses,

    -- * Request Lenses
    describeMovingAddresses_dryRun,
    describeMovingAddresses_filters,
    describeMovingAddresses_maxResults,
    describeMovingAddresses_nextToken,
    describeMovingAddresses_publicIps,

    -- * Destructuring the Response
    DescribeMovingAddressesResponse (..),
    newDescribeMovingAddressesResponse,

    -- * Response Lenses
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @moving-status@ - The status of the Elastic IP address
    --     (@MovingToVpc@ | @RestoringToClassic@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1000; if @MaxResults@ is given a value outside of
    -- this range, an error is returned.
    --
    -- Default: If no value is provided, the default is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more Elastic IP addresses.
    publicIps :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMovingAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeMovingAddresses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeMovingAddresses_filters' - One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
--
-- 'maxResults', 'describeMovingAddresses_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
--
-- 'nextToken', 'describeMovingAddresses_nextToken' - The token for the next page of results.
--
-- 'publicIps', 'describeMovingAddresses_publicIps' - One or more Elastic IP addresses.
newDescribeMovingAddresses ::
  DescribeMovingAddresses
newDescribeMovingAddresses =
  DescribeMovingAddresses'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      publicIps = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeMovingAddresses_dryRun :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Bool)
describeMovingAddresses_dryRun = Lens.lens (\DescribeMovingAddresses' {dryRun} -> dryRun) (\s@DescribeMovingAddresses' {} a -> s {dryRun = a} :: DescribeMovingAddresses)

-- | One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
describeMovingAddresses_filters :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe [Filter])
describeMovingAddresses_filters = Lens.lens (\DescribeMovingAddresses' {filters} -> filters) (\s@DescribeMovingAddresses' {} a -> s {filters = a} :: DescribeMovingAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
describeMovingAddresses_maxResults :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Natural)
describeMovingAddresses_maxResults = Lens.lens (\DescribeMovingAddresses' {maxResults} -> maxResults) (\s@DescribeMovingAddresses' {} a -> s {maxResults = a} :: DescribeMovingAddresses)

-- | The token for the next page of results.
describeMovingAddresses_nextToken :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Text)
describeMovingAddresses_nextToken = Lens.lens (\DescribeMovingAddresses' {nextToken} -> nextToken) (\s@DescribeMovingAddresses' {} a -> s {nextToken = a} :: DescribeMovingAddresses)

-- | One or more Elastic IP addresses.
describeMovingAddresses_publicIps :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe [Prelude.Text])
describeMovingAddresses_publicIps = Lens.lens (\DescribeMovingAddresses' {publicIps} -> publicIps) (\s@DescribeMovingAddresses' {} a -> s {publicIps = a} :: DescribeMovingAddresses) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeMovingAddresses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_movingAddressStatuses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeMovingAddresses_nextToken
              Lens..~ rs
              Lens.^? describeMovingAddressesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeMovingAddresses where
  type
    AWSResponse DescribeMovingAddresses =
      DescribeMovingAddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeMovingAddressesResponse'
            Prelude.<$> ( x
                            Data..@? "movingAddressStatusSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMovingAddresses where
  hashWithSalt _salt DescribeMovingAddresses' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` publicIps

instance Prelude.NFData DescribeMovingAddresses where
  rnf DescribeMovingAddresses' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf publicIps

instance Data.ToHeaders DescribeMovingAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeMovingAddresses where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMovingAddresses where
  toQuery DescribeMovingAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeMovingAddresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "PublicIp" Prelude.<$> publicIps)
      ]

-- | /See:/ 'newDescribeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { -- | The status for each Elastic IP address.
    movingAddressStatuses :: Prelude.Maybe [MovingAddressStatus],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMovingAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'movingAddressStatuses', 'describeMovingAddressesResponse_movingAddressStatuses' - The status for each Elastic IP address.
--
-- 'nextToken', 'describeMovingAddressesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeMovingAddressesResponse_httpStatus' - The response's http status code.
newDescribeMovingAddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMovingAddressesResponse
newDescribeMovingAddressesResponse pHttpStatus_ =
  DescribeMovingAddressesResponse'
    { movingAddressStatuses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status for each Elastic IP address.
describeMovingAddressesResponse_movingAddressStatuses :: Lens.Lens' DescribeMovingAddressesResponse (Prelude.Maybe [MovingAddressStatus])
describeMovingAddressesResponse_movingAddressStatuses = Lens.lens (\DescribeMovingAddressesResponse' {movingAddressStatuses} -> movingAddressStatuses) (\s@DescribeMovingAddressesResponse' {} a -> s {movingAddressStatuses = a} :: DescribeMovingAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeMovingAddressesResponse_nextToken :: Lens.Lens' DescribeMovingAddressesResponse (Prelude.Maybe Prelude.Text)
describeMovingAddressesResponse_nextToken = Lens.lens (\DescribeMovingAddressesResponse' {nextToken} -> nextToken) (\s@DescribeMovingAddressesResponse' {} a -> s {nextToken = a} :: DescribeMovingAddressesResponse)

-- | The response's http status code.
describeMovingAddressesResponse_httpStatus :: Lens.Lens' DescribeMovingAddressesResponse Prelude.Int
describeMovingAddressesResponse_httpStatus = Lens.lens (\DescribeMovingAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeMovingAddressesResponse' {} a -> s {httpStatus = a} :: DescribeMovingAddressesResponse)

instance
  Prelude.NFData
    DescribeMovingAddressesResponse
  where
  rnf DescribeMovingAddressesResponse' {..} =
    Prelude.rnf movingAddressStatuses `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

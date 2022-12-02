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
-- Module      : Amazonka.EC2.DescribeInstanceTypeOfferings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all instance types offered. The results can be
-- filtered by location (Region or Availability Zone). If no location is
-- specified, the instance types offered in the current Region are
-- returned.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInstanceTypeOfferings
  ( -- * Creating a Request
    DescribeInstanceTypeOfferings (..),
    newDescribeInstanceTypeOfferings,

    -- * Request Lenses
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferings_filters,
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferings_locationType,

    -- * Destructuring the Response
    DescribeInstanceTypeOfferingsResponse (..),
    newDescribeInstanceTypeOfferingsResponse,

    -- * Response Lenses
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceTypeOfferings' smart constructor.
data DescribeInstanceTypeOfferings = DescribeInstanceTypeOfferings'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @location@ - This depends on the location type. For example, if the
    --     location type is @region@ (default), the location is the Region code
    --     (for example, @us-east-2@.)
    --
    -- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the next token value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The location type.
    locationType :: Prelude.Maybe LocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypeOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceTypeOfferings_nextToken' - The token to retrieve the next page of results.
--
-- 'filters', 'describeInstanceTypeOfferings_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @location@ - This depends on the location type. For example, if the
--     location type is @region@ (default), the location is the Region code
--     (for example, @us-east-2@.)
--
-- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
--
-- 'dryRun', 'describeInstanceTypeOfferings_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeInstanceTypeOfferings_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the next token value.
--
-- 'locationType', 'describeInstanceTypeOfferings_locationType' - The location type.
newDescribeInstanceTypeOfferings ::
  DescribeInstanceTypeOfferings
newDescribeInstanceTypeOfferings =
  DescribeInstanceTypeOfferings'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      locationType = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeInstanceTypeOfferings_nextToken :: Lens.Lens' DescribeInstanceTypeOfferings (Prelude.Maybe Prelude.Text)
describeInstanceTypeOfferings_nextToken = Lens.lens (\DescribeInstanceTypeOfferings' {nextToken} -> nextToken) (\s@DescribeInstanceTypeOfferings' {} a -> s {nextToken = a} :: DescribeInstanceTypeOfferings)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @location@ - This depends on the location type. For example, if the
--     location type is @region@ (default), the location is the Region code
--     (for example, @us-east-2@.)
--
-- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
describeInstanceTypeOfferings_filters :: Lens.Lens' DescribeInstanceTypeOfferings (Prelude.Maybe [Filter])
describeInstanceTypeOfferings_filters = Lens.lens (\DescribeInstanceTypeOfferings' {filters} -> filters) (\s@DescribeInstanceTypeOfferings' {} a -> s {filters = a} :: DescribeInstanceTypeOfferings) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceTypeOfferings_dryRun :: Lens.Lens' DescribeInstanceTypeOfferings (Prelude.Maybe Prelude.Bool)
describeInstanceTypeOfferings_dryRun = Lens.lens (\DescribeInstanceTypeOfferings' {dryRun} -> dryRun) (\s@DescribeInstanceTypeOfferings' {} a -> s {dryRun = a} :: DescribeInstanceTypeOfferings)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the next token value.
describeInstanceTypeOfferings_maxResults :: Lens.Lens' DescribeInstanceTypeOfferings (Prelude.Maybe Prelude.Natural)
describeInstanceTypeOfferings_maxResults = Lens.lens (\DescribeInstanceTypeOfferings' {maxResults} -> maxResults) (\s@DescribeInstanceTypeOfferings' {} a -> s {maxResults = a} :: DescribeInstanceTypeOfferings)

-- | The location type.
describeInstanceTypeOfferings_locationType :: Lens.Lens' DescribeInstanceTypeOfferings (Prelude.Maybe LocationType)
describeInstanceTypeOfferings_locationType = Lens.lens (\DescribeInstanceTypeOfferings' {locationType} -> locationType) (\s@DescribeInstanceTypeOfferings' {} a -> s {locationType = a} :: DescribeInstanceTypeOfferings)

instance Core.AWSPager DescribeInstanceTypeOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypeOfferingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypeOfferingsResponse_instanceTypeOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceTypeOfferings_nextToken
          Lens..~ rs
          Lens.^? describeInstanceTypeOfferingsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstanceTypeOfferings
  where
  type
    AWSResponse DescribeInstanceTypeOfferings =
      DescribeInstanceTypeOfferingsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceTypeOfferingsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "instanceTypeOfferingSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceTypeOfferings
  where
  hashWithSalt _salt DescribeInstanceTypeOfferings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` locationType

instance Prelude.NFData DescribeInstanceTypeOfferings where
  rnf DescribeInstanceTypeOfferings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf locationType

instance Data.ToHeaders DescribeInstanceTypeOfferings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstanceTypeOfferings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstanceTypeOfferings where
  toQuery DescribeInstanceTypeOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeInstanceTypeOfferings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "LocationType" Data.=: locationType
      ]

-- | /See:/ 'newDescribeInstanceTypeOfferingsResponse' smart constructor.
data DescribeInstanceTypeOfferingsResponse = DescribeInstanceTypeOfferingsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance types offered.
    instanceTypeOfferings :: Prelude.Maybe [InstanceTypeOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypeOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceTypeOfferingsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'instanceTypeOfferings', 'describeInstanceTypeOfferingsResponse_instanceTypeOfferings' - The instance types offered.
--
-- 'httpStatus', 'describeInstanceTypeOfferingsResponse_httpStatus' - The response's http status code.
newDescribeInstanceTypeOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceTypeOfferingsResponse
newDescribeInstanceTypeOfferingsResponse pHttpStatus_ =
  DescribeInstanceTypeOfferingsResponse'
    { nextToken =
        Prelude.Nothing,
      instanceTypeOfferings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstanceTypeOfferingsResponse_nextToken :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Prelude.Maybe Prelude.Text)
describeInstanceTypeOfferingsResponse_nextToken = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {nextToken = a} :: DescribeInstanceTypeOfferingsResponse)

-- | The instance types offered.
describeInstanceTypeOfferingsResponse_instanceTypeOfferings :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Prelude.Maybe [InstanceTypeOffering])
describeInstanceTypeOfferingsResponse_instanceTypeOfferings = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {instanceTypeOfferings} -> instanceTypeOfferings) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {instanceTypeOfferings = a} :: DescribeInstanceTypeOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceTypeOfferingsResponse_httpStatus :: Lens.Lens' DescribeInstanceTypeOfferingsResponse Prelude.Int
describeInstanceTypeOfferingsResponse_httpStatus = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceTypeOfferingsResponse)

instance
  Prelude.NFData
    DescribeInstanceTypeOfferingsResponse
  where
  rnf DescribeInstanceTypeOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceTypeOfferings
      `Prelude.seq` Prelude.rnf httpStatus

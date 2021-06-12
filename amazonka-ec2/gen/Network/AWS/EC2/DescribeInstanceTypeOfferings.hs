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
-- Module      : Network.AWS.EC2.DescribeInstanceTypeOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.DescribeInstanceTypeOfferings
  ( -- * Creating a Request
    DescribeInstanceTypeOfferings (..),
    newDescribeInstanceTypeOfferings,

    -- * Request Lenses
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferings_locationType,
    describeInstanceTypeOfferings_filters,

    -- * Destructuring the Response
    DescribeInstanceTypeOfferingsResponse (..),
    newDescribeInstanceTypeOfferingsResponse,

    -- * Response Lenses
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceTypeOfferings' smart constructor.
data DescribeInstanceTypeOfferings = DescribeInstanceTypeOfferings'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the next token value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The location type.
    locationType :: Core.Maybe LocationType,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @location@ - This depends on the location type. For example, if the
    --     location type is @region@ (default), the location is the Region code
    --     (for example, @us-east-2@.)
    --
    -- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
--
-- 'filters', 'describeInstanceTypeOfferings_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @location@ - This depends on the location type. For example, if the
--     location type is @region@ (default), the location is the Region code
--     (for example, @us-east-2@.)
--
-- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
newDescribeInstanceTypeOfferings ::
  DescribeInstanceTypeOfferings
newDescribeInstanceTypeOfferings =
  DescribeInstanceTypeOfferings'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      locationType = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to retrieve the next page of results.
describeInstanceTypeOfferings_nextToken :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Core.Text)
describeInstanceTypeOfferings_nextToken = Lens.lens (\DescribeInstanceTypeOfferings' {nextToken} -> nextToken) (\s@DescribeInstanceTypeOfferings' {} a -> s {nextToken = a} :: DescribeInstanceTypeOfferings)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceTypeOfferings_dryRun :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Core.Bool)
describeInstanceTypeOfferings_dryRun = Lens.lens (\DescribeInstanceTypeOfferings' {dryRun} -> dryRun) (\s@DescribeInstanceTypeOfferings' {} a -> s {dryRun = a} :: DescribeInstanceTypeOfferings)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the next token value.
describeInstanceTypeOfferings_maxResults :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Core.Natural)
describeInstanceTypeOfferings_maxResults = Lens.lens (\DescribeInstanceTypeOfferings' {maxResults} -> maxResults) (\s@DescribeInstanceTypeOfferings' {} a -> s {maxResults = a} :: DescribeInstanceTypeOfferings)

-- | The location type.
describeInstanceTypeOfferings_locationType :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe LocationType)
describeInstanceTypeOfferings_locationType = Lens.lens (\DescribeInstanceTypeOfferings' {locationType} -> locationType) (\s@DescribeInstanceTypeOfferings' {} a -> s {locationType = a} :: DescribeInstanceTypeOfferings)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @location@ - This depends on the location type. For example, if the
--     location type is @region@ (default), the location is the Region code
--     (for example, @us-east-2@.)
--
-- -   @instance-type@ - The instance type. For example, @c5.2xlarge@.
describeInstanceTypeOfferings_filters :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe [Filter])
describeInstanceTypeOfferings_filters = Lens.lens (\DescribeInstanceTypeOfferings' {filters} -> filters) (\s@DescribeInstanceTypeOfferings' {} a -> s {filters = a} :: DescribeInstanceTypeOfferings) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeInstanceTypeOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypeOfferingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypeOfferingsResponse_instanceTypeOfferings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInstanceTypeOfferings_nextToken
          Lens..~ rs
          Lens.^? describeInstanceTypeOfferingsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstanceTypeOfferings
  where
  type
    AWSResponse DescribeInstanceTypeOfferings =
      DescribeInstanceTypeOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceTypeOfferingsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "instanceTypeOfferingSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstanceTypeOfferings

instance Core.NFData DescribeInstanceTypeOfferings

instance Core.ToHeaders DescribeInstanceTypeOfferings where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInstanceTypeOfferings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstanceTypeOfferings where
  toQuery DescribeInstanceTypeOfferings' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceTypeOfferings" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "LocationType" Core.=: locationType,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeInstanceTypeOfferingsResponse' smart constructor.
data DescribeInstanceTypeOfferingsResponse = DescribeInstanceTypeOfferingsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The instance types offered.
    instanceTypeOfferings :: Core.Maybe [InstanceTypeOffering],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeInstanceTypeOfferingsResponse
newDescribeInstanceTypeOfferingsResponse pHttpStatus_ =
  DescribeInstanceTypeOfferingsResponse'
    { nextToken =
        Core.Nothing,
      instanceTypeOfferings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstanceTypeOfferingsResponse_nextToken :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Core.Maybe Core.Text)
describeInstanceTypeOfferingsResponse_nextToken = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {nextToken = a} :: DescribeInstanceTypeOfferingsResponse)

-- | The instance types offered.
describeInstanceTypeOfferingsResponse_instanceTypeOfferings :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Core.Maybe [InstanceTypeOffering])
describeInstanceTypeOfferingsResponse_instanceTypeOfferings = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {instanceTypeOfferings} -> instanceTypeOfferings) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {instanceTypeOfferings = a} :: DescribeInstanceTypeOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstanceTypeOfferingsResponse_httpStatus :: Lens.Lens' DescribeInstanceTypeOfferingsResponse Core.Int
describeInstanceTypeOfferingsResponse_httpStatus = Lens.lens (\DescribeInstanceTypeOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceTypeOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceTypeOfferingsResponse)

instance
  Core.NFData
    DescribeInstanceTypeOfferingsResponse

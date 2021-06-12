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
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServicePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the principals (service consumers) that are permitted to
-- discover your VPC endpoint service.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServicePermissions
  ( -- * Creating a Request
    DescribeVpcEndpointServicePermissions (..),
    newDescribeVpcEndpointServicePermissions,

    -- * Request Lenses
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_serviceId,

    -- * Destructuring the Response
    DescribeVpcEndpointServicePermissionsResponse (..),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- * Response Lenses
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcEndpointServicePermissions' smart constructor.
data DescribeVpcEndpointServicePermissions = DescribeVpcEndpointServicePermissions'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Core.Maybe Core.Int,
    -- | One or more filters.
    --
    -- -   @principal@ - The ARN of the principal.
    --
    -- -   @principal-type@ - The principal type (@All@ | @Service@ |
    --     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
    filters :: Core.Maybe [Filter],
    -- | The ID of the service.
    serviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointServicePermissions_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeVpcEndpointServicePermissions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVpcEndpointServicePermissions_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'filters', 'describeVpcEndpointServicePermissions_filters' - One or more filters.
--
-- -   @principal@ - The ARN of the principal.
--
-- -   @principal-type@ - The principal type (@All@ | @Service@ |
--     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
--
-- 'serviceId', 'describeVpcEndpointServicePermissions_serviceId' - The ID of the service.
newDescribeVpcEndpointServicePermissions ::
  -- | 'serviceId'
  Core.Text ->
  DescribeVpcEndpointServicePermissions
newDescribeVpcEndpointServicePermissions pServiceId_ =
  DescribeVpcEndpointServicePermissions'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      serviceId = pServiceId_
    }

-- | The token to retrieve the next page of results.
describeVpcEndpointServicePermissions_nextToken :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Text)
describeVpcEndpointServicePermissions_nextToken = Lens.lens (\DescribeVpcEndpointServicePermissions' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {nextToken = a} :: DescribeVpcEndpointServicePermissions)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServicePermissions_dryRun :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Bool)
describeVpcEndpointServicePermissions_dryRun = Lens.lens (\DescribeVpcEndpointServicePermissions' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {dryRun = a} :: DescribeVpcEndpointServicePermissions)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointServicePermissions_maxResults :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Int)
describeVpcEndpointServicePermissions_maxResults = Lens.lens (\DescribeVpcEndpointServicePermissions' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {maxResults = a} :: DescribeVpcEndpointServicePermissions)

-- | One or more filters.
--
-- -   @principal@ - The ARN of the principal.
--
-- -   @principal-type@ - The principal type (@All@ | @Service@ |
--     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
describeVpcEndpointServicePermissions_filters :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe [Filter])
describeVpcEndpointServicePermissions_filters = Lens.lens (\DescribeVpcEndpointServicePermissions' {filters} -> filters) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {filters = a} :: DescribeVpcEndpointServicePermissions) Core.. Lens.mapping Lens._Coerce

-- | The ID of the service.
describeVpcEndpointServicePermissions_serviceId :: Lens.Lens' DescribeVpcEndpointServicePermissions Core.Text
describeVpcEndpointServicePermissions_serviceId = Lens.lens (\DescribeVpcEndpointServicePermissions' {serviceId} -> serviceId) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {serviceId = a} :: DescribeVpcEndpointServicePermissions)

instance
  Core.AWSPager
    DescribeVpcEndpointServicePermissions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicePermissionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicePermissionsResponse_allowedPrincipals
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcEndpointServicePermissions_nextToken
          Lens..~ rs
            Lens.^? describeVpcEndpointServicePermissionsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointServicePermissions
  where
  type
    AWSResponse
      DescribeVpcEndpointServicePermissions =
      DescribeVpcEndpointServicePermissionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointServicePermissionsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "allowedPrincipals" Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeVpcEndpointServicePermissions

instance
  Core.NFData
    DescribeVpcEndpointServicePermissions

instance
  Core.ToHeaders
    DescribeVpcEndpointServicePermissions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeVpcEndpointServicePermissions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeVpcEndpointServicePermissions
  where
  toQuery DescribeVpcEndpointServicePermissions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeVpcEndpointServicePermissions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "ServiceId" Core.=: serviceId
      ]

-- | /See:/ 'newDescribeVpcEndpointServicePermissionsResponse' smart constructor.
data DescribeVpcEndpointServicePermissionsResponse = DescribeVpcEndpointServicePermissionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more allowed principals.
    allowedPrincipals :: Core.Maybe [AllowedPrincipal],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServicePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointServicePermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'allowedPrincipals', 'describeVpcEndpointServicePermissionsResponse_allowedPrincipals' - Information about one or more allowed principals.
--
-- 'httpStatus', 'describeVpcEndpointServicePermissionsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointServicePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcEndpointServicePermissionsResponse
newDescribeVpcEndpointServicePermissionsResponse
  pHttpStatus_ =
    DescribeVpcEndpointServicePermissionsResponse'
      { nextToken =
          Core.Nothing,
        allowedPrincipals =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointServicePermissionsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Core.Maybe Core.Text)
describeVpcEndpointServicePermissionsResponse_nextToken = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointServicePermissionsResponse)

-- | Information about one or more allowed principals.
describeVpcEndpointServicePermissionsResponse_allowedPrincipals :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Core.Maybe [AllowedPrincipal])
describeVpcEndpointServicePermissionsResponse_allowedPrincipals = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {allowedPrincipals} -> allowedPrincipals) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {allowedPrincipals = a} :: DescribeVpcEndpointServicePermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointServicePermissionsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse Core.Int
describeVpcEndpointServicePermissionsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServicePermissionsResponse)

instance
  Core.NFData
    DescribeVpcEndpointServicePermissionsResponse

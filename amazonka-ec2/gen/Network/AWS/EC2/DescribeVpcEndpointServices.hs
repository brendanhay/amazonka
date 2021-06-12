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
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available services to which you can create a VPC endpoint.
--
-- When the service provider and the consumer have different accounts in
-- multiple Availability Zones, and the consumer views the VPC endpoint
-- service information, the response only includes the common Availability
-- Zones. For example, when the service provider account uses @us-east-1a@
-- and @us-east-1c@ and the consumer uses @us-east-1a@ and @us-east-1b@,
-- the response includes the VPC endpoint services in the common
-- Availability Zone, @us-east-1a@.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServices
  ( -- * Creating a Request
    DescribeVpcEndpointServices (..),
    newDescribeVpcEndpointServices,

    -- * Request Lenses
    describeVpcEndpointServices_nextToken,
    describeVpcEndpointServices_serviceNames,
    describeVpcEndpointServices_dryRun,
    describeVpcEndpointServices_maxResults,
    describeVpcEndpointServices_filters,

    -- * Destructuring the Response
    DescribeVpcEndpointServicesResponse (..),
    newDescribeVpcEndpointServicesResponse,

    -- * Response Lenses
    describeVpcEndpointServicesResponse_serviceDetails,
    describeVpcEndpointServicesResponse_nextToken,
    describeVpcEndpointServicesResponse_serviceNames,
    describeVpcEndpointServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpcEndpointServices.
--
-- /See:/ 'newDescribeVpcEndpointServices' smart constructor.
data DescribeVpcEndpointServices = DescribeVpcEndpointServices'
  { -- | The token for the next set of items to return. (You received this token
    -- from a prior call.)
    nextToken :: Core.Maybe Core.Text,
    -- | One or more service names.
    serviceNames :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return for this request. The request
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    --
    -- Constraint: If the value is greater than 1,000, we return only 1,000
    -- items.
    maxResults :: Core.Maybe Core.Int,
    -- | One or more filters.
    --
    -- -   @service-name@ - The name of the service.
    --
    -- -   @service-type@ - The type of service (@Interface@ | @Gateway@).
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointServices_nextToken' - The token for the next set of items to return. (You received this token
-- from a prior call.)
--
-- 'serviceNames', 'describeVpcEndpointServices_serviceNames' - One or more service names.
--
-- 'dryRun', 'describeVpcEndpointServices_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVpcEndpointServices_maxResults' - The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000
-- items.
--
-- 'filters', 'describeVpcEndpointServices_filters' - One or more filters.
--
-- -   @service-name@ - The name of the service.
--
-- -   @service-type@ - The type of service (@Interface@ | @Gateway@).
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
newDescribeVpcEndpointServices ::
  DescribeVpcEndpointServices
newDescribeVpcEndpointServices =
  DescribeVpcEndpointServices'
    { nextToken =
        Core.Nothing,
      serviceNames = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
describeVpcEndpointServices_nextToken :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Text)
describeVpcEndpointServices_nextToken = Lens.lens (\DescribeVpcEndpointServices' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServices' {} a -> s {nextToken = a} :: DescribeVpcEndpointServices)

-- | One or more service names.
describeVpcEndpointServices_serviceNames :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe [Core.Text])
describeVpcEndpointServices_serviceNames = Lens.lens (\DescribeVpcEndpointServices' {serviceNames} -> serviceNames) (\s@DescribeVpcEndpointServices' {} a -> s {serviceNames = a} :: DescribeVpcEndpointServices) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServices_dryRun :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Bool)
describeVpcEndpointServices_dryRun = Lens.lens (\DescribeVpcEndpointServices' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServices' {} a -> s {dryRun = a} :: DescribeVpcEndpointServices)

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000
-- items.
describeVpcEndpointServices_maxResults :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Int)
describeVpcEndpointServices_maxResults = Lens.lens (\DescribeVpcEndpointServices' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServices' {} a -> s {maxResults = a} :: DescribeVpcEndpointServices)

-- | One or more filters.
--
-- -   @service-name@ - The name of the service.
--
-- -   @service-type@ - The type of service (@Interface@ | @Gateway@).
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
describeVpcEndpointServices_filters :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe [Filter])
describeVpcEndpointServices_filters = Lens.lens (\DescribeVpcEndpointServices' {filters} -> filters) (\s@DescribeVpcEndpointServices' {} a -> s {filters = a} :: DescribeVpcEndpointServices) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeVpcEndpointServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicesResponse_serviceDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicesResponse_serviceNames
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcEndpointServices_nextToken
          Lens..~ rs
          Lens.^? describeVpcEndpointServicesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeVpcEndpointServices where
  type
    AWSResponse DescribeVpcEndpointServices =
      DescribeVpcEndpointServicesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointServicesResponse'
            Core.<$> ( x Core..@? "serviceDetailSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "serviceNameSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcEndpointServices

instance Core.NFData DescribeVpcEndpointServices

instance Core.ToHeaders DescribeVpcEndpointServices where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpcEndpointServices where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcEndpointServices where
  toQuery DescribeVpcEndpointServices' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVpcEndpointServices" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "ServiceName"
              Core.<$> serviceNames
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | Contains the output of DescribeVpcEndpointServices.
--
-- /See:/ 'newDescribeVpcEndpointServicesResponse' smart constructor.
data DescribeVpcEndpointServicesResponse = DescribeVpcEndpointServicesResponse'
  { -- | Information about the service.
    serviceDetails :: Core.Maybe [ServiceDetail],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of supported services.
    serviceNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceDetails', 'describeVpcEndpointServicesResponse_serviceDetails' - Information about the service.
--
-- 'nextToken', 'describeVpcEndpointServicesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'serviceNames', 'describeVpcEndpointServicesResponse_serviceNames' - A list of supported services.
--
-- 'httpStatus', 'describeVpcEndpointServicesResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcEndpointServicesResponse
newDescribeVpcEndpointServicesResponse pHttpStatus_ =
  DescribeVpcEndpointServicesResponse'
    { serviceDetails =
        Core.Nothing,
      nextToken = Core.Nothing,
      serviceNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the service.
describeVpcEndpointServicesResponse_serviceDetails :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe [ServiceDetail])
describeVpcEndpointServicesResponse_serviceDetails = Lens.lens (\DescribeVpcEndpointServicesResponse' {serviceDetails} -> serviceDetails) (\s@DescribeVpcEndpointServicesResponse' {} a -> s {serviceDetails = a} :: DescribeVpcEndpointServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeVpcEndpointServicesResponse_nextToken :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe Core.Text)
describeVpcEndpointServicesResponse_nextToken = Lens.lens (\DescribeVpcEndpointServicesResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServicesResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointServicesResponse)

-- | A list of supported services.
describeVpcEndpointServicesResponse_serviceNames :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe [Core.Text])
describeVpcEndpointServicesResponse_serviceNames = Lens.lens (\DescribeVpcEndpointServicesResponse' {serviceNames} -> serviceNames) (\s@DescribeVpcEndpointServicesResponse' {} a -> s {serviceNames = a} :: DescribeVpcEndpointServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointServicesResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServicesResponse Core.Int
describeVpcEndpointServicesResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServicesResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServicesResponse)

instance
  Core.NFData
    DescribeVpcEndpointServicesResponse

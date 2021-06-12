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
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint service configurations in your account (your
-- services).
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
  ( -- * Creating a Request
    DescribeVpcEndpointServiceConfigurations (..),
    newDescribeVpcEndpointServiceConfigurations,

    -- * Request Lenses
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_serviceIds,
    describeVpcEndpointServiceConfigurations_filters,

    -- * Destructuring the Response
    DescribeVpcEndpointServiceConfigurationsResponse (..),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- * Response Lenses
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcEndpointServiceConfigurations' smart constructor.
data DescribeVpcEndpointServiceConfigurations = DescribeVpcEndpointServiceConfigurations'
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
    -- | The IDs of one or more services.
    serviceIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @service-name@ - The name of the service.
    --
    -- -   @service-id@ - The ID of the service.
    --
    -- -   @service-state@ - The state of the service (@Pending@ | @Available@
    --     | @Deleting@ | @Deleted@ | @Failed@).
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
-- Create a value of 'DescribeVpcEndpointServiceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointServiceConfigurations_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeVpcEndpointServiceConfigurations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVpcEndpointServiceConfigurations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'serviceIds', 'describeVpcEndpointServiceConfigurations_serviceIds' - The IDs of one or more services.
--
-- 'filters', 'describeVpcEndpointServiceConfigurations_filters' - One or more filters.
--
-- -   @service-name@ - The name of the service.
--
-- -   @service-id@ - The ID of the service.
--
-- -   @service-state@ - The state of the service (@Pending@ | @Available@
--     | @Deleting@ | @Deleted@ | @Failed@).
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
newDescribeVpcEndpointServiceConfigurations ::
  DescribeVpcEndpointServiceConfigurations
newDescribeVpcEndpointServiceConfigurations =
  DescribeVpcEndpointServiceConfigurations'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      serviceIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to retrieve the next page of results.
describeVpcEndpointServiceConfigurations_nextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Text)
describeVpcEndpointServiceConfigurations_nextToken = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {nextToken = a} :: DescribeVpcEndpointServiceConfigurations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServiceConfigurations_dryRun :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Bool)
describeVpcEndpointServiceConfigurations_dryRun = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {dryRun = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointServiceConfigurations_maxResults :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Int)
describeVpcEndpointServiceConfigurations_maxResults = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {maxResults = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The IDs of one or more services.
describeVpcEndpointServiceConfigurations_serviceIds :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe [Core.Text])
describeVpcEndpointServiceConfigurations_serviceIds = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {serviceIds} -> serviceIds) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {serviceIds = a} :: DescribeVpcEndpointServiceConfigurations) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @service-name@ - The name of the service.
--
-- -   @service-id@ - The ID of the service.
--
-- -   @service-state@ - The state of the service (@Pending@ | @Available@
--     | @Deleting@ | @Deleted@ | @Failed@).
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
describeVpcEndpointServiceConfigurations_filters :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe [Filter])
describeVpcEndpointServiceConfigurations_filters = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {filters} -> filters) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {filters = a} :: DescribeVpcEndpointServiceConfigurations) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeVpcEndpointServiceConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcEndpointServiceConfigurations_nextToken
          Lens..~ rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointServiceConfigurations
  where
  type
    AWSResponse
      DescribeVpcEndpointServiceConfigurations =
      DescribeVpcEndpointServiceConfigurationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointServiceConfigurationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "serviceConfigurationSet"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeVpcEndpointServiceConfigurations

instance
  Core.NFData
    DescribeVpcEndpointServiceConfigurations

instance
  Core.ToHeaders
    DescribeVpcEndpointServiceConfigurations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeVpcEndpointServiceConfigurations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeVpcEndpointServiceConfigurations
  where
  toQuery DescribeVpcEndpointServiceConfigurations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeVpcEndpointServiceConfigurations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "ServiceId" Core.<$> serviceIds),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeVpcEndpointServiceConfigurationsResponse' smart constructor.
data DescribeVpcEndpointServiceConfigurationsResponse = DescribeVpcEndpointServiceConfigurationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more services.
    serviceConfigurations :: Core.Maybe [ServiceConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServiceConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointServiceConfigurationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'serviceConfigurations', 'describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations' - Information about one or more services.
--
-- 'httpStatus', 'describeVpcEndpointServiceConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointServiceConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcEndpointServiceConfigurationsResponse
newDescribeVpcEndpointServiceConfigurationsResponse
  pHttpStatus_ =
    DescribeVpcEndpointServiceConfigurationsResponse'
      { nextToken =
          Core.Nothing,
        serviceConfigurations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointServiceConfigurationsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Core.Maybe Core.Text)
describeVpcEndpointServiceConfigurationsResponse_nextToken = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointServiceConfigurationsResponse)

-- | Information about one or more services.
describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Core.Maybe [ServiceConfiguration])
describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {serviceConfigurations} -> serviceConfigurations) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {serviceConfigurations = a} :: DescribeVpcEndpointServiceConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointServiceConfigurationsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse Core.Int
describeVpcEndpointServiceConfigurationsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServiceConfigurationsResponse)

instance
  Core.NFData
    DescribeVpcEndpointServiceConfigurationsResponse

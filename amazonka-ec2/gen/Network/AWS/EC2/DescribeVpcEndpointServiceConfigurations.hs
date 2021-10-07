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
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_dryRun,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcEndpointServiceConfigurations' smart constructor.
data DescribeVpcEndpointServiceConfigurations = DescribeVpcEndpointServiceConfigurations'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of one or more services.
    serviceIds :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'describeVpcEndpointServiceConfigurations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'dryRun', 'describeVpcEndpointServiceConfigurations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      serviceIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeVpcEndpointServiceConfigurations_nextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Text)
describeVpcEndpointServiceConfigurations_nextToken = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {nextToken = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointServiceConfigurations_maxResults :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Int)
describeVpcEndpointServiceConfigurations_maxResults = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {maxResults = a} :: DescribeVpcEndpointServiceConfigurations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServiceConfigurations_dryRun :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Bool)
describeVpcEndpointServiceConfigurations_dryRun = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {dryRun = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The IDs of one or more services.
describeVpcEndpointServiceConfigurations_serviceIds :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe [Prelude.Text])
describeVpcEndpointServiceConfigurations_serviceIds = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {serviceIds} -> serviceIds) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {serviceIds = a} :: DescribeVpcEndpointServiceConfigurations) Prelude.. Lens.mapping Lens._Coerce

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
describeVpcEndpointServiceConfigurations_filters :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe [Filter])
describeVpcEndpointServiceConfigurations_filters = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {filters} -> filters) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {filters = a} :: DescribeVpcEndpointServiceConfigurations) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeVpcEndpointServiceConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVpcEndpointServiceConfigurations_nextToken
          Lens..~ rs
            Lens.^? describeVpcEndpointServiceConfigurationsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "serviceConfigurationSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcEndpointServiceConfigurations

instance
  Prelude.NFData
    DescribeVpcEndpointServiceConfigurations

instance
  Core.ToHeaders
    DescribeVpcEndpointServiceConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeVpcEndpointServiceConfigurations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeVpcEndpointServiceConfigurations
  where
  toQuery DescribeVpcEndpointServiceConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeVpcEndpointServiceConfigurations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "ServiceId"
              Prelude.<$> serviceIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeVpcEndpointServiceConfigurationsResponse' smart constructor.
data DescribeVpcEndpointServiceConfigurationsResponse = DescribeVpcEndpointServiceConfigurationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more services.
    serviceConfigurations :: Prelude.Maybe [ServiceConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVpcEndpointServiceConfigurationsResponse
newDescribeVpcEndpointServiceConfigurationsResponse
  pHttpStatus_ =
    DescribeVpcEndpointServiceConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        serviceConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointServiceConfigurationsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeVpcEndpointServiceConfigurationsResponse_nextToken = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointServiceConfigurationsResponse)

-- | Information about one or more services.
describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Prelude.Maybe [ServiceConfiguration])
describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {serviceConfigurations} -> serviceConfigurations) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {serviceConfigurations = a} :: DescribeVpcEndpointServiceConfigurationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointServiceConfigurationsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse Prelude.Int
describeVpcEndpointServiceConfigurationsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServiceConfigurationsResponse)

instance
  Prelude.NFData
    DescribeVpcEndpointServiceConfigurationsResponse

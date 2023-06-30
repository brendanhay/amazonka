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
-- Module      : Amazonka.EC2.DescribeVpcEndpointServiceConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint service configurations in your account (your
-- services).
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcEndpointServiceConfigurations
  ( -- * Creating a Request
    DescribeVpcEndpointServiceConfigurations (..),
    newDescribeVpcEndpointServiceConfigurations,

    -- * Request Lenses
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_filters,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_serviceIds,

    -- * Destructuring the Response
    DescribeVpcEndpointServiceConfigurationsResponse (..),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- * Response Lenses
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcEndpointServiceConfigurations' smart constructor.
data DescribeVpcEndpointServiceConfigurations = DescribeVpcEndpointServiceConfigurations'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @service-name@ - The name of the service.
    --
    -- -   @service-id@ - The ID of the service.
    --
    -- -   @service-state@ - The state of the service (@Pending@ | @Available@
    --     | @Deleting@ | @Deleted@ | @Failed@).
    --
    -- -   @supported-ip-address-types@ - The IP address type (@ipv4@ |
    --     @ipv6@).
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more services.
    serviceIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeVpcEndpointServiceConfigurations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- -   @supported-ip-address-types@ - The IP address type (@ipv4@ |
--     @ipv6@).
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
--
-- 'maxResults', 'describeVpcEndpointServiceConfigurations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'nextToken', 'describeVpcEndpointServiceConfigurations_nextToken' - The token to retrieve the next page of results.
--
-- 'serviceIds', 'describeVpcEndpointServiceConfigurations_serviceIds' - The IDs of one or more services.
newDescribeVpcEndpointServiceConfigurations ::
  DescribeVpcEndpointServiceConfigurations
newDescribeVpcEndpointServiceConfigurations =
  DescribeVpcEndpointServiceConfigurations'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServiceConfigurations_dryRun :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Bool)
describeVpcEndpointServiceConfigurations_dryRun = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {dryRun = a} :: DescribeVpcEndpointServiceConfigurations)

-- | One or more filters.
--
-- -   @service-name@ - The name of the service.
--
-- -   @service-id@ - The ID of the service.
--
-- -   @service-state@ - The state of the service (@Pending@ | @Available@
--     | @Deleting@ | @Deleted@ | @Failed@).
--
-- -   @supported-ip-address-types@ - The IP address type (@ipv4@ |
--     @ipv6@).
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
describeVpcEndpointServiceConfigurations_filters = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {filters} -> filters) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {filters = a} :: DescribeVpcEndpointServiceConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointServiceConfigurations_maxResults :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Int)
describeVpcEndpointServiceConfigurations_maxResults = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {maxResults = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The token to retrieve the next page of results.
describeVpcEndpointServiceConfigurations_nextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Text)
describeVpcEndpointServiceConfigurations_nextToken = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {nextToken = a} :: DescribeVpcEndpointServiceConfigurations)

-- | The IDs of one or more services.
describeVpcEndpointServiceConfigurations_serviceIds :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Prelude.Maybe [Prelude.Text])
describeVpcEndpointServiceConfigurations_serviceIds = Lens.lens (\DescribeVpcEndpointServiceConfigurations' {serviceIds} -> serviceIds) (\s@DescribeVpcEndpointServiceConfigurations' {} a -> s {serviceIds = a} :: DescribeVpcEndpointServiceConfigurations) Prelude.. Lens.mapping Lens.coerced

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
        Prelude.Just
          Prelude.$ rq
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointServiceConfigurationsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "serviceConfigurationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcEndpointServiceConfigurations
  where
  hashWithSalt
    _salt
    DescribeVpcEndpointServiceConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceIds

instance
  Prelude.NFData
    DescribeVpcEndpointServiceConfigurations
  where
  rnf DescribeVpcEndpointServiceConfigurations' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceIds

instance
  Data.ToHeaders
    DescribeVpcEndpointServiceConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeVpcEndpointServiceConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeVpcEndpointServiceConfigurations
  where
  toQuery DescribeVpcEndpointServiceConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVpcEndpointServiceConfigurations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "ServiceId"
              Prelude.<$> serviceIds
          )
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
describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {serviceConfigurations} -> serviceConfigurations) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {serviceConfigurations = a} :: DescribeVpcEndpointServiceConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcEndpointServiceConfigurationsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse Prelude.Int
describeVpcEndpointServiceConfigurationsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServiceConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServiceConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServiceConfigurationsResponse)

instance
  Prelude.NFData
    DescribeVpcEndpointServiceConfigurationsResponse
  where
  rnf
    DescribeVpcEndpointServiceConfigurationsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf serviceConfigurations
        `Prelude.seq` Prelude.rnf httpStatus

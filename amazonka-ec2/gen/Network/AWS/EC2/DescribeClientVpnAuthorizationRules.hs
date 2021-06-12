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
-- Module      : Network.AWS.EC2.DescribeClientVpnAuthorizationRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the authorization rules for a specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnAuthorizationRules
  ( -- * Creating a Request
    DescribeClientVpnAuthorizationRules (..),
    newDescribeClientVpnAuthorizationRules,

    -- * Request Lenses
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,

    -- * Destructuring the Response
    DescribeClientVpnAuthorizationRulesResponse (..),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- * Response Lenses
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnAuthorizationRules' smart constructor.
data DescribeClientVpnAuthorizationRules = DescribeClientVpnAuthorizationRules'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @description@ - The description of the authorization rule.
    --
    -- -   @destination-cidr@ - The CIDR of the network to which the
    --     authorization rule applies.
    --
    -- -   @group-id@ - The ID of the Active Directory group to which the
    --     authorization rule grants access.
    filters :: Core.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnAuthorizationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnAuthorizationRules_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnAuthorizationRules_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnAuthorizationRules_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'filters', 'describeClientVpnAuthorizationRules_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @description@ - The description of the authorization rule.
--
-- -   @destination-cidr@ - The CIDR of the network to which the
--     authorization rule applies.
--
-- -   @group-id@ - The ID of the Active Directory group to which the
--     authorization rule grants access.
--
-- 'clientVpnEndpointId', 'describeClientVpnAuthorizationRules_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newDescribeClientVpnAuthorizationRules ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  DescribeClientVpnAuthorizationRules
newDescribeClientVpnAuthorizationRules
  pClientVpnEndpointId_ =
    DescribeClientVpnAuthorizationRules'
      { nextToken =
          Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | The token to retrieve the next page of results.
describeClientVpnAuthorizationRules_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Core.Text)
describeClientVpnAuthorizationRules_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRules' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRules)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnAuthorizationRules_dryRun :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Core.Bool)
describeClientVpnAuthorizationRules_dryRun = Lens.lens (\DescribeClientVpnAuthorizationRules' {dryRun} -> dryRun) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {dryRun = a} :: DescribeClientVpnAuthorizationRules)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnAuthorizationRules_maxResults :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Core.Natural)
describeClientVpnAuthorizationRules_maxResults = Lens.lens (\DescribeClientVpnAuthorizationRules' {maxResults} -> maxResults) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {maxResults = a} :: DescribeClientVpnAuthorizationRules)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @description@ - The description of the authorization rule.
--
-- -   @destination-cidr@ - The CIDR of the network to which the
--     authorization rule applies.
--
-- -   @group-id@ - The ID of the Active Directory group to which the
--     authorization rule grants access.
describeClientVpnAuthorizationRules_filters :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe [Filter])
describeClientVpnAuthorizationRules_filters = Lens.lens (\DescribeClientVpnAuthorizationRules' {filters} -> filters) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {filters = a} :: DescribeClientVpnAuthorizationRules) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnAuthorizationRules_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnAuthorizationRules Core.Text
describeClientVpnAuthorizationRules_clientVpnEndpointId = Lens.lens (\DescribeClientVpnAuthorizationRules' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnAuthorizationRules)

instance
  Core.AWSPager
    DescribeClientVpnAuthorizationRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_authorizationRules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClientVpnAuthorizationRules_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeClientVpnAuthorizationRules
  where
  type
    AWSResponse DescribeClientVpnAuthorizationRules =
      DescribeClientVpnAuthorizationRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnAuthorizationRulesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "authorizationRule" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeClientVpnAuthorizationRules

instance
  Core.NFData
    DescribeClientVpnAuthorizationRules

instance
  Core.ToHeaders
    DescribeClientVpnAuthorizationRules
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeClientVpnAuthorizationRules
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeClientVpnAuthorizationRules
  where
  toQuery DescribeClientVpnAuthorizationRules' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeClientVpnAuthorizationRules" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnAuthorizationRulesResponse' smart constructor.
data DescribeClientVpnAuthorizationRulesResponse = DescribeClientVpnAuthorizationRulesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the authorization rules.
    authorizationRules :: Core.Maybe [AuthorizationRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnAuthorizationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnAuthorizationRulesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'authorizationRules', 'describeClientVpnAuthorizationRulesResponse_authorizationRules' - Information about the authorization rules.
--
-- 'httpStatus', 'describeClientVpnAuthorizationRulesResponse_httpStatus' - The response's http status code.
newDescribeClientVpnAuthorizationRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClientVpnAuthorizationRulesResponse
newDescribeClientVpnAuthorizationRulesResponse
  pHttpStatus_ =
    DescribeClientVpnAuthorizationRulesResponse'
      { nextToken =
          Core.Nothing,
        authorizationRules =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnAuthorizationRulesResponse_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Core.Maybe Core.Text)
describeClientVpnAuthorizationRulesResponse_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRulesResponse)

-- | Information about the authorization rules.
describeClientVpnAuthorizationRulesResponse_authorizationRules :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Core.Maybe [AuthorizationRule])
describeClientVpnAuthorizationRulesResponse_authorizationRules = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {authorizationRules} -> authorizationRules) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {authorizationRules = a} :: DescribeClientVpnAuthorizationRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientVpnAuthorizationRulesResponse_httpStatus :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse Core.Int
describeClientVpnAuthorizationRulesResponse_httpStatus = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnAuthorizationRulesResponse)

instance
  Core.NFData
    DescribeClientVpnAuthorizationRulesResponse

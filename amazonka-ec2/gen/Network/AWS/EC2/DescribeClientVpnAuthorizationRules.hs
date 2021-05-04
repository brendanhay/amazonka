{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnAuthorizationRules' smart constructor.
data DescribeClientVpnAuthorizationRules = DescribeClientVpnAuthorizationRules'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @description@ - The description of the authorization rule.
    --
    -- -   @destination-cidr@ - The CIDR of the network to which the
    --     authorization rule applies.
    --
    -- -   @group-id@ - The ID of the Active Directory group to which the
    --     authorization rule grants access.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeClientVpnAuthorizationRules
newDescribeClientVpnAuthorizationRules
  pClientVpnEndpointId_ =
    DescribeClientVpnAuthorizationRules'
      { nextToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        filters = Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | The token to retrieve the next page of results.
describeClientVpnAuthorizationRules_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Text)
describeClientVpnAuthorizationRules_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRules' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRules)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnAuthorizationRules_dryRun :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Bool)
describeClientVpnAuthorizationRules_dryRun = Lens.lens (\DescribeClientVpnAuthorizationRules' {dryRun} -> dryRun) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {dryRun = a} :: DescribeClientVpnAuthorizationRules)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnAuthorizationRules_maxResults :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Natural)
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
describeClientVpnAuthorizationRules_filters :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe [Filter])
describeClientVpnAuthorizationRules_filters = Lens.lens (\DescribeClientVpnAuthorizationRules' {filters} -> filters) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {filters = a} :: DescribeClientVpnAuthorizationRules) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnAuthorizationRules_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnAuthorizationRules Prelude.Text
describeClientVpnAuthorizationRules_clientVpnEndpointId = Lens.lens (\DescribeClientVpnAuthorizationRules' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnAuthorizationRules)

instance
  Pager.AWSPager
    DescribeClientVpnAuthorizationRules
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_authorizationRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeClientVpnAuthorizationRules_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeClientVpnAuthorizationRules
  where
  type
    Rs DescribeClientVpnAuthorizationRules =
      DescribeClientVpnAuthorizationRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnAuthorizationRulesResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
              Prelude.<*> ( x Prelude..@? "authorizationRule"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClientVpnAuthorizationRules

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRules

instance
  Prelude.ToHeaders
    DescribeClientVpnAuthorizationRules
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeClientVpnAuthorizationRules
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeClientVpnAuthorizationRules
  where
  toQuery DescribeClientVpnAuthorizationRules' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeClientVpnAuthorizationRules" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnAuthorizationRulesResponse' smart constructor.
data DescribeClientVpnAuthorizationRulesResponse = DescribeClientVpnAuthorizationRulesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the authorization rules.
    authorizationRules :: Prelude.Maybe [AuthorizationRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeClientVpnAuthorizationRulesResponse
newDescribeClientVpnAuthorizationRulesResponse
  pHttpStatus_ =
    DescribeClientVpnAuthorizationRulesResponse'
      { nextToken =
          Prelude.Nothing,
        authorizationRules =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnAuthorizationRulesResponse_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Prelude.Maybe Prelude.Text)
describeClientVpnAuthorizationRulesResponse_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRulesResponse)

-- | Information about the authorization rules.
describeClientVpnAuthorizationRulesResponse_authorizationRules :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Prelude.Maybe [AuthorizationRule])
describeClientVpnAuthorizationRulesResponse_authorizationRules = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {authorizationRules} -> authorizationRules) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {authorizationRules = a} :: DescribeClientVpnAuthorizationRulesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeClientVpnAuthorizationRulesResponse_httpStatus :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse Prelude.Int
describeClientVpnAuthorizationRulesResponse_httpStatus = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnAuthorizationRulesResponse)

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRulesResponse

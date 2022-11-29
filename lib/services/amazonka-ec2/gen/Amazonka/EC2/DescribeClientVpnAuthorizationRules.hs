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
-- Module      : Amazonka.EC2.DescribeClientVpnAuthorizationRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the authorization rules for a specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeClientVpnAuthorizationRules
  ( -- * Creating a Request
    DescribeClientVpnAuthorizationRules (..),
    newDescribeClientVpnAuthorizationRules,

    -- * Request Lenses
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClientVpnAuthorizationRules' smart constructor.
data DescribeClientVpnAuthorizationRules = DescribeClientVpnAuthorizationRules'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dryRun', 'describeClientVpnAuthorizationRules_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnAuthorizationRules_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
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
        filters = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | The token to retrieve the next page of results.
describeClientVpnAuthorizationRules_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Text)
describeClientVpnAuthorizationRules_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRules' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRules)

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
describeClientVpnAuthorizationRules_filters = Lens.lens (\DescribeClientVpnAuthorizationRules' {filters} -> filters) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {filters = a} :: DescribeClientVpnAuthorizationRules) Prelude.. Lens.mapping Lens.coerced

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

-- | The ID of the Client VPN endpoint.
describeClientVpnAuthorizationRules_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnAuthorizationRules Prelude.Text
describeClientVpnAuthorizationRules_clientVpnEndpointId = Lens.lens (\DescribeClientVpnAuthorizationRules' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnAuthorizationRules)

instance
  Core.AWSPager
    DescribeClientVpnAuthorizationRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnAuthorizationRulesResponse_authorizationRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClientVpnAuthorizationRules_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnAuthorizationRulesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeClientVpnAuthorizationRules
  where
  type
    AWSResponse DescribeClientVpnAuthorizationRules =
      DescribeClientVpnAuthorizationRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnAuthorizationRulesResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "authorizationRule"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClientVpnAuthorizationRules
  where
  hashWithSalt
    _salt
    DescribeClientVpnAuthorizationRules' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` clientVpnEndpointId

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRules
  where
  rnf DescribeClientVpnAuthorizationRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance
  Core.ToHeaders
    DescribeClientVpnAuthorizationRules
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeClientVpnAuthorizationRules
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeClientVpnAuthorizationRules
  where
  toQuery DescribeClientVpnAuthorizationRules' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeClientVpnAuthorizationRules" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeClientVpnAuthorizationRulesResponse_authorizationRules = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {authorizationRules} -> authorizationRules) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {authorizationRules = a} :: DescribeClientVpnAuthorizationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClientVpnAuthorizationRulesResponse_httpStatus :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse Prelude.Int
describeClientVpnAuthorizationRulesResponse_httpStatus = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnAuthorizationRulesResponse)

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRulesResponse
  where
  rnf DescribeClientVpnAuthorizationRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf authorizationRules
      `Prelude.seq` Prelude.rnf httpStatus

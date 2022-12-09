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
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,

    -- * Destructuring the Response
    DescribeClientVpnAuthorizationRulesResponse (..),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- * Response Lenses
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClientVpnAuthorizationRules' smart constructor.
data DescribeClientVpnAuthorizationRules = DescribeClientVpnAuthorizationRules'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'dryRun', 'describeClientVpnAuthorizationRules_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'maxResults', 'describeClientVpnAuthorizationRules_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'nextToken', 'describeClientVpnAuthorizationRules_nextToken' - The token to retrieve the next page of results.
--
-- 'clientVpnEndpointId', 'describeClientVpnAuthorizationRules_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newDescribeClientVpnAuthorizationRules ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  DescribeClientVpnAuthorizationRules
newDescribeClientVpnAuthorizationRules
  pClientVpnEndpointId_ =
    DescribeClientVpnAuthorizationRules'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnAuthorizationRules_dryRun :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Bool)
describeClientVpnAuthorizationRules_dryRun = Lens.lens (\DescribeClientVpnAuthorizationRules' {dryRun} -> dryRun) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {dryRun = a} :: DescribeClientVpnAuthorizationRules)

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

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnAuthorizationRules_maxResults :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Natural)
describeClientVpnAuthorizationRules_maxResults = Lens.lens (\DescribeClientVpnAuthorizationRules' {maxResults} -> maxResults) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {maxResults = a} :: DescribeClientVpnAuthorizationRules)

-- | The token to retrieve the next page of results.
describeClientVpnAuthorizationRules_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRules (Prelude.Maybe Prelude.Text)
describeClientVpnAuthorizationRules_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRules' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRules' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRules)

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
            Prelude.<$> ( x Data..@? "authorizationRule"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClientVpnAuthorizationRules
  where
  hashWithSalt
    _salt
    DescribeClientVpnAuthorizationRules' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` clientVpnEndpointId

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRules
  where
  rnf DescribeClientVpnAuthorizationRules' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance
  Data.ToHeaders
    DescribeClientVpnAuthorizationRules
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeClientVpnAuthorizationRules
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeClientVpnAuthorizationRules
  where
  toQuery DescribeClientVpnAuthorizationRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeClientVpnAuthorizationRules" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnAuthorizationRulesResponse' smart constructor.
data DescribeClientVpnAuthorizationRulesResponse = DescribeClientVpnAuthorizationRulesResponse'
  { -- | Information about the authorization rules.
    authorizationRules :: Prelude.Maybe [AuthorizationRule],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'authorizationRules', 'describeClientVpnAuthorizationRulesResponse_authorizationRules' - Information about the authorization rules.
--
-- 'nextToken', 'describeClientVpnAuthorizationRulesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeClientVpnAuthorizationRulesResponse_httpStatus' - The response's http status code.
newDescribeClientVpnAuthorizationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClientVpnAuthorizationRulesResponse
newDescribeClientVpnAuthorizationRulesResponse
  pHttpStatus_ =
    DescribeClientVpnAuthorizationRulesResponse'
      { authorizationRules =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the authorization rules.
describeClientVpnAuthorizationRulesResponse_authorizationRules :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Prelude.Maybe [AuthorizationRule])
describeClientVpnAuthorizationRulesResponse_authorizationRules = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {authorizationRules} -> authorizationRules) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {authorizationRules = a} :: DescribeClientVpnAuthorizationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnAuthorizationRulesResponse_nextToken :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Prelude.Maybe Prelude.Text)
describeClientVpnAuthorizationRulesResponse_nextToken = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {nextToken = a} :: DescribeClientVpnAuthorizationRulesResponse)

-- | The response's http status code.
describeClientVpnAuthorizationRulesResponse_httpStatus :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse Prelude.Int
describeClientVpnAuthorizationRulesResponse_httpStatus = Lens.lens (\DescribeClientVpnAuthorizationRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnAuthorizationRulesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnAuthorizationRulesResponse)

instance
  Prelude.NFData
    DescribeClientVpnAuthorizationRulesResponse
  where
  rnf DescribeClientVpnAuthorizationRulesResponse' {..} =
    Prelude.rnf authorizationRules
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

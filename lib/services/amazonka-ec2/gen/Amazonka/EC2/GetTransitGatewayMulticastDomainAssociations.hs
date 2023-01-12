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
-- Module      : Amazonka.EC2.GetTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the transit gateway
-- multicast domain.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    GetTransitGatewayMulticastDomainAssociations (..),
    newGetTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    GetTransitGatewayMulticastDomainAssociationsResponse (..),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
    --
    -- -   @state@ - The state of the subnet association. Valid values are
    --     @associated@ | @associating@ | @disassociated@ | @disassociating@.
    --
    -- -   @subnet-id@ - The ID of the subnet.
    --
    -- -   @transit-gateway-attachment-id@ - The id of the transit gateway
    --     attachment.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getTransitGatewayMulticastDomainAssociations_filters' - One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associating@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
--
-- 'maxResults', 'getTransitGatewayMulticastDomainAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'getTransitGatewayMulticastDomainAssociations_nextToken' - The token for the next page of results.
--
-- 'transitGatewayMulticastDomainId', 'getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newGetTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
newGetTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Bool)
getTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associating@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
getTransitGatewayMulticastDomainAssociations_filters :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe [Filter])
getTransitGatewayMulticastDomainAssociations_filters = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {filters} -> filters) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {filters = a} :: GetTransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayMulticastDomainAssociations_maxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Natural)
getTransitGatewayMulticastDomainAssociations_maxResults = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The token for the next page of results.
getTransitGatewayMulticastDomainAssociations_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociations_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: GetTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSPager
    GetTransitGatewayMulticastDomainAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayMulticastDomainAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      GetTransitGatewayMulticastDomainAssociations =
      GetTransitGatewayMulticastDomainAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayMulticastDomainAssociationsResponse'
            Prelude.<$> ( x Data..@? "multicastDomainAssociations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayMulticastDomainAssociations
  where
  hashWithSalt
    _salt
    GetTransitGatewayMulticastDomainAssociations' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    GetTransitGatewayMulticastDomainAssociations
  where
  rnf GetTransitGatewayMulticastDomainAssociations' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    GetTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetTransitGatewayMulticastDomainAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetTransitGatewayMulticastDomainAssociations
  where
  toQuery
    GetTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "GetTransitGatewayMulticastDomainAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken,
          "TransitGatewayMulticastDomainId"
            Data.=: transitGatewayMulticastDomainId
        ]

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { -- | Information about the multicast domain associations.
    multicastDomainAssociations :: Prelude.Maybe [TransitGatewayMulticastDomainAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayMulticastDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multicastDomainAssociations', 'getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations' - Information about the multicast domain associations.
--
-- 'nextToken', 'getTransitGatewayMulticastDomainAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getTransitGatewayMulticastDomainAssociationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
newGetTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { multicastDomainAssociations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the multicast domain associations.
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe [TransitGatewayMulticastDomainAssociation])
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {multicastDomainAssociations} -> multicastDomainAssociations) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {multicastDomainAssociations = a} :: GetTransitGatewayMulticastDomainAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayMulticastDomainAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

-- | The response's http status code.
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Prelude.Int
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayMulticastDomainAssociationsResponse
  where
  rnf
    GetTransitGatewayMulticastDomainAssociationsResponse' {..} =
      Prelude.rnf multicastDomainAssociations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus

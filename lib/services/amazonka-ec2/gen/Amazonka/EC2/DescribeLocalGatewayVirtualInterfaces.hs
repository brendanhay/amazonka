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
-- Module      : Amazonka.EC2.DescribeLocalGatewayVirtualInterfaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interfaces.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLocalGatewayVirtualInterfaces
  ( -- * Creating a Request
    DescribeLocalGatewayVirtualInterfaces (..),
    newDescribeLocalGatewayVirtualInterfaces,

    -- * Request Lenses
    describeLocalGatewayVirtualInterfaces_dryRun,
    describeLocalGatewayVirtualInterfaces_filters,
    describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds,
    describeLocalGatewayVirtualInterfaces_maxResults,
    describeLocalGatewayVirtualInterfaces_nextToken,

    -- * Destructuring the Response
    DescribeLocalGatewayVirtualInterfacesResponse (..),
    newDescribeLocalGatewayVirtualInterfacesResponse,

    -- * Response Lenses
    describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces,
    describeLocalGatewayVirtualInterfacesResponse_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaces' smart constructor.
data DescribeLocalGatewayVirtualInterfaces = DescribeLocalGatewayVirtualInterfaces'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @local-address@ - The local address.
    --
    -- -   @local-bgp-asn@ - The Border Gateway Protocol (BGP) Autonomous
    --     System Number (ASN) of the local gateway.
    --
    -- -   @local-gateway-id@ - The ID of the local gateway.
    --
    -- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
    --     interface.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     local gateway virtual interface.
    --
    -- -   @peer-address@ - The peer address.
    --
    -- -   @peer-bgp-asn@ - The peer BGP ASN.
    --
    -- -   @vlan@ - The ID of the VLAN.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the virtual interfaces.
    localGatewayVirtualInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayVirtualInterfaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeLocalGatewayVirtualInterfaces_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeLocalGatewayVirtualInterfaces_filters' - One or more filters.
--
-- -   @local-address@ - The local address.
--
-- -   @local-bgp-asn@ - The Border Gateway Protocol (BGP) Autonomous
--     System Number (ASN) of the local gateway.
--
-- -   @local-gateway-id@ - The ID of the local gateway.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface.
--
-- -   @peer-address@ - The peer address.
--
-- -   @peer-bgp-asn@ - The peer BGP ASN.
--
-- -   @vlan@ - The ID of the VLAN.
--
-- 'localGatewayVirtualInterfaceIds', 'describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
--
-- 'maxResults', 'describeLocalGatewayVirtualInterfaces_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfaces_nextToken' - The token for the next page of results.
newDescribeLocalGatewayVirtualInterfaces ::
  DescribeLocalGatewayVirtualInterfaces
newDescribeLocalGatewayVirtualInterfaces =
  DescribeLocalGatewayVirtualInterfaces'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      localGatewayVirtualInterfaceIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayVirtualInterfaces_dryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Prelude.Maybe Prelude.Bool)
describeLocalGatewayVirtualInterfaces_dryRun = Lens.lens (\DescribeLocalGatewayVirtualInterfaces' {dryRun} -> dryRun) (\s@DescribeLocalGatewayVirtualInterfaces' {} a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaces)

-- | One or more filters.
--
-- -   @local-address@ - The local address.
--
-- -   @local-bgp-asn@ - The Border Gateway Protocol (BGP) Autonomous
--     System Number (ASN) of the local gateway.
--
-- -   @local-gateway-id@ - The ID of the local gateway.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface.
--
-- -   @peer-address@ - The peer address.
--
-- -   @peer-bgp-asn@ - The peer BGP ASN.
--
-- -   @vlan@ - The ID of the VLAN.
describeLocalGatewayVirtualInterfaces_filters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Prelude.Maybe [Filter])
describeLocalGatewayVirtualInterfaces_filters = Lens.lens (\DescribeLocalGatewayVirtualInterfaces' {filters} -> filters) (\s@DescribeLocalGatewayVirtualInterfaces' {} a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaces) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the virtual interfaces.
describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Prelude.Maybe [Prelude.Text])
describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds = Lens.lens (\DescribeLocalGatewayVirtualInterfaces' {localGatewayVirtualInterfaceIds} -> localGatewayVirtualInterfaceIds) (\s@DescribeLocalGatewayVirtualInterfaces' {} a -> s {localGatewayVirtualInterfaceIds = a} :: DescribeLocalGatewayVirtualInterfaces) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayVirtualInterfaces_maxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Prelude.Maybe Prelude.Natural)
describeLocalGatewayVirtualInterfaces_maxResults = Lens.lens (\DescribeLocalGatewayVirtualInterfaces' {maxResults} -> maxResults) (\s@DescribeLocalGatewayVirtualInterfaces' {} a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaces)

-- | The token for the next page of results.
describeLocalGatewayVirtualInterfaces_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfaces_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaces' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaces' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaces)

instance
  Core.AWSPager
    DescribeLocalGatewayVirtualInterfaces
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfacesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeLocalGatewayVirtualInterfaces_nextToken
          Lens..~ rs
          Lens.^? describeLocalGatewayVirtualInterfacesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLocalGatewayVirtualInterfaces
  where
  type
    AWSResponse
      DescribeLocalGatewayVirtualInterfaces =
      DescribeLocalGatewayVirtualInterfacesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfacesResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayVirtualInterfaceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayVirtualInterfaces
  where
  hashWithSalt
    _salt
    DescribeLocalGatewayVirtualInterfaces' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfaces
  where
  rnf DescribeLocalGatewayVirtualInterfaces' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLocalGatewayVirtualInterfaces
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeLocalGatewayVirtualInterfaces
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeLocalGatewayVirtualInterfaces
  where
  toQuery DescribeLocalGatewayVirtualInterfaces' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeLocalGatewayVirtualInterfaces" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "LocalGatewayVirtualInterfaceId"
              Prelude.<$> localGatewayVirtualInterfaceIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfacesResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfacesResponse = DescribeLocalGatewayVirtualInterfacesResponse'
  { -- | Information about the virtual interfaces.
    localGatewayVirtualInterfaces :: Prelude.Maybe [LocalGatewayVirtualInterface],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayVirtualInterfacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayVirtualInterfaces', 'describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces' - Information about the virtual interfaces.
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfacesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeLocalGatewayVirtualInterfacesResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayVirtualInterfacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayVirtualInterfacesResponse
newDescribeLocalGatewayVirtualInterfacesResponse
  pHttpStatus_ =
    DescribeLocalGatewayVirtualInterfacesResponse'
      { localGatewayVirtualInterfaces =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the virtual interfaces.
describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Prelude.Maybe [LocalGatewayVirtualInterface])
describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces = Lens.lens (\DescribeLocalGatewayVirtualInterfacesResponse' {localGatewayVirtualInterfaces} -> localGatewayVirtualInterfaces) (\s@DescribeLocalGatewayVirtualInterfacesResponse' {} a -> s {localGatewayVirtualInterfaces = a} :: DescribeLocalGatewayVirtualInterfacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayVirtualInterfacesResponse_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfacesResponse_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfacesResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfacesResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfacesResponse)

-- | The response's http status code.
describeLocalGatewayVirtualInterfacesResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse Prelude.Int
describeLocalGatewayVirtualInterfacesResponse_httpStatus = Lens.lens (\DescribeLocalGatewayVirtualInterfacesResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayVirtualInterfacesResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayVirtualInterfacesResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfacesResponse
  where
  rnf
    DescribeLocalGatewayVirtualInterfacesResponse' {..} =
      Prelude.rnf localGatewayVirtualInterfaces
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus

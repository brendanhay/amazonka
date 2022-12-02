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
-- Module      : Amazonka.NetworkManager.GetNetworkRoutes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network routes of the specified global network.
module Amazonka.NetworkManager.GetNetworkRoutes
  ( -- * Creating a Request
    GetNetworkRoutes (..),
    newGetNetworkRoutes,

    -- * Request Lenses
    getNetworkRoutes_supernetOfMatches,
    getNetworkRoutes_subnetOfMatches,
    getNetworkRoutes_types,
    getNetworkRoutes_exactCidrMatches,
    getNetworkRoutes_prefixListIds,
    getNetworkRoutes_destinationFilters,
    getNetworkRoutes_longestPrefixMatches,
    getNetworkRoutes_states,
    getNetworkRoutes_globalNetworkId,
    getNetworkRoutes_routeTableIdentifier,

    -- * Destructuring the Response
    GetNetworkRoutesResponse (..),
    newGetNetworkRoutesResponse,

    -- * Response Lenses
    getNetworkRoutesResponse_routeTableArn,
    getNetworkRoutesResponse_networkRoutes,
    getNetworkRoutesResponse_routeTableType,
    getNetworkRoutesResponse_coreNetworkSegmentEdge,
    getNetworkRoutesResponse_routeTableTimestamp,
    getNetworkRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkRoutes' smart constructor.
data GetNetworkRoutes = GetNetworkRoutes'
  { -- | The routes with a CIDR that encompasses the CIDR filter. Example: If you
    -- specify 10.0.1.0\/30, then the result returns 10.0.1.0\/29.
    supernetOfMatches :: Prelude.Maybe [Prelude.Text],
    -- | The routes with a subnet that match the specified CIDR filter.
    subnetOfMatches :: Prelude.Maybe [Prelude.Text],
    -- | The route types.
    types :: Prelude.Maybe [RouteType],
    -- | An exact CIDR block.
    exactCidrMatches :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the prefix lists.
    prefixListIds :: Prelude.Maybe [Prelude.Text],
    -- | Filter by route table destination. Possible Values:
    -- TRANSIT_GATEWAY_ATTACHMENT_ID, RESOURCE_ID, or RESOURCE_TYPE.
    destinationFilters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The most specific route that matches the traffic (longest prefix match).
    longestPrefixMatches :: Prelude.Maybe [Prelude.Text],
    -- | The route states.
    states :: Prelude.Maybe [RouteState],
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the route table.
    routeTableIdentifier :: RouteTableIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supernetOfMatches', 'getNetworkRoutes_supernetOfMatches' - The routes with a CIDR that encompasses the CIDR filter. Example: If you
-- specify 10.0.1.0\/30, then the result returns 10.0.1.0\/29.
--
-- 'subnetOfMatches', 'getNetworkRoutes_subnetOfMatches' - The routes with a subnet that match the specified CIDR filter.
--
-- 'types', 'getNetworkRoutes_types' - The route types.
--
-- 'exactCidrMatches', 'getNetworkRoutes_exactCidrMatches' - An exact CIDR block.
--
-- 'prefixListIds', 'getNetworkRoutes_prefixListIds' - The IDs of the prefix lists.
--
-- 'destinationFilters', 'getNetworkRoutes_destinationFilters' - Filter by route table destination. Possible Values:
-- TRANSIT_GATEWAY_ATTACHMENT_ID, RESOURCE_ID, or RESOURCE_TYPE.
--
-- 'longestPrefixMatches', 'getNetworkRoutes_longestPrefixMatches' - The most specific route that matches the traffic (longest prefix match).
--
-- 'states', 'getNetworkRoutes_states' - The route states.
--
-- 'globalNetworkId', 'getNetworkRoutes_globalNetworkId' - The ID of the global network.
--
-- 'routeTableIdentifier', 'getNetworkRoutes_routeTableIdentifier' - The ID of the route table.
newGetNetworkRoutes ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'routeTableIdentifier'
  RouteTableIdentifier ->
  GetNetworkRoutes
newGetNetworkRoutes
  pGlobalNetworkId_
  pRouteTableIdentifier_ =
    GetNetworkRoutes'
      { supernetOfMatches =
          Prelude.Nothing,
        subnetOfMatches = Prelude.Nothing,
        types = Prelude.Nothing,
        exactCidrMatches = Prelude.Nothing,
        prefixListIds = Prelude.Nothing,
        destinationFilters = Prelude.Nothing,
        longestPrefixMatches = Prelude.Nothing,
        states = Prelude.Nothing,
        globalNetworkId = pGlobalNetworkId_,
        routeTableIdentifier = pRouteTableIdentifier_
      }

-- | The routes with a CIDR that encompasses the CIDR filter. Example: If you
-- specify 10.0.1.0\/30, then the result returns 10.0.1.0\/29.
getNetworkRoutes_supernetOfMatches :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [Prelude.Text])
getNetworkRoutes_supernetOfMatches = Lens.lens (\GetNetworkRoutes' {supernetOfMatches} -> supernetOfMatches) (\s@GetNetworkRoutes' {} a -> s {supernetOfMatches = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The routes with a subnet that match the specified CIDR filter.
getNetworkRoutes_subnetOfMatches :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [Prelude.Text])
getNetworkRoutes_subnetOfMatches = Lens.lens (\GetNetworkRoutes' {subnetOfMatches} -> subnetOfMatches) (\s@GetNetworkRoutes' {} a -> s {subnetOfMatches = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The route types.
getNetworkRoutes_types :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [RouteType])
getNetworkRoutes_types = Lens.lens (\GetNetworkRoutes' {types} -> types) (\s@GetNetworkRoutes' {} a -> s {types = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | An exact CIDR block.
getNetworkRoutes_exactCidrMatches :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [Prelude.Text])
getNetworkRoutes_exactCidrMatches = Lens.lens (\GetNetworkRoutes' {exactCidrMatches} -> exactCidrMatches) (\s@GetNetworkRoutes' {} a -> s {exactCidrMatches = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the prefix lists.
getNetworkRoutes_prefixListIds :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [Prelude.Text])
getNetworkRoutes_prefixListIds = Lens.lens (\GetNetworkRoutes' {prefixListIds} -> prefixListIds) (\s@GetNetworkRoutes' {} a -> s {prefixListIds = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | Filter by route table destination. Possible Values:
-- TRANSIT_GATEWAY_ATTACHMENT_ID, RESOURCE_ID, or RESOURCE_TYPE.
getNetworkRoutes_destinationFilters :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
getNetworkRoutes_destinationFilters = Lens.lens (\GetNetworkRoutes' {destinationFilters} -> destinationFilters) (\s@GetNetworkRoutes' {} a -> s {destinationFilters = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The most specific route that matches the traffic (longest prefix match).
getNetworkRoutes_longestPrefixMatches :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [Prelude.Text])
getNetworkRoutes_longestPrefixMatches = Lens.lens (\GetNetworkRoutes' {longestPrefixMatches} -> longestPrefixMatches) (\s@GetNetworkRoutes' {} a -> s {longestPrefixMatches = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The route states.
getNetworkRoutes_states :: Lens.Lens' GetNetworkRoutes (Prelude.Maybe [RouteState])
getNetworkRoutes_states = Lens.lens (\GetNetworkRoutes' {states} -> states) (\s@GetNetworkRoutes' {} a -> s {states = a} :: GetNetworkRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the global network.
getNetworkRoutes_globalNetworkId :: Lens.Lens' GetNetworkRoutes Prelude.Text
getNetworkRoutes_globalNetworkId = Lens.lens (\GetNetworkRoutes' {globalNetworkId} -> globalNetworkId) (\s@GetNetworkRoutes' {} a -> s {globalNetworkId = a} :: GetNetworkRoutes)

-- | The ID of the route table.
getNetworkRoutes_routeTableIdentifier :: Lens.Lens' GetNetworkRoutes RouteTableIdentifier
getNetworkRoutes_routeTableIdentifier = Lens.lens (\GetNetworkRoutes' {routeTableIdentifier} -> routeTableIdentifier) (\s@GetNetworkRoutes' {} a -> s {routeTableIdentifier = a} :: GetNetworkRoutes)

instance Core.AWSRequest GetNetworkRoutes where
  type
    AWSResponse GetNetworkRoutes =
      GetNetworkRoutesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkRoutesResponse'
            Prelude.<$> (x Data..?> "RouteTableArn")
            Prelude.<*> (x Data..?> "NetworkRoutes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "RouteTableType")
            Prelude.<*> (x Data..?> "CoreNetworkSegmentEdge")
            Prelude.<*> (x Data..?> "RouteTableTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkRoutes where
  hashWithSalt _salt GetNetworkRoutes' {..} =
    _salt `Prelude.hashWithSalt` supernetOfMatches
      `Prelude.hashWithSalt` subnetOfMatches
      `Prelude.hashWithSalt` types
      `Prelude.hashWithSalt` exactCidrMatches
      `Prelude.hashWithSalt` prefixListIds
      `Prelude.hashWithSalt` destinationFilters
      `Prelude.hashWithSalt` longestPrefixMatches
      `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` routeTableIdentifier

instance Prelude.NFData GetNetworkRoutes where
  rnf GetNetworkRoutes' {..} =
    Prelude.rnf supernetOfMatches
      `Prelude.seq` Prelude.rnf subnetOfMatches
      `Prelude.seq` Prelude.rnf types
      `Prelude.seq` Prelude.rnf exactCidrMatches
      `Prelude.seq` Prelude.rnf prefixListIds
      `Prelude.seq` Prelude.rnf destinationFilters
      `Prelude.seq` Prelude.rnf longestPrefixMatches
      `Prelude.seq` Prelude.rnf states
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf routeTableIdentifier

instance Data.ToHeaders GetNetworkRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNetworkRoutes where
  toJSON GetNetworkRoutes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SupernetOfMatches" Data..=)
              Prelude.<$> supernetOfMatches,
            ("SubnetOfMatches" Data..=)
              Prelude.<$> subnetOfMatches,
            ("Types" Data..=) Prelude.<$> types,
            ("ExactCidrMatches" Data..=)
              Prelude.<$> exactCidrMatches,
            ("PrefixListIds" Data..=) Prelude.<$> prefixListIds,
            ("DestinationFilters" Data..=)
              Prelude.<$> destinationFilters,
            ("LongestPrefixMatches" Data..=)
              Prelude.<$> longestPrefixMatches,
            ("States" Data..=) Prelude.<$> states,
            Prelude.Just
              ( "RouteTableIdentifier"
                  Data..= routeTableIdentifier
              )
          ]
      )

instance Data.ToPath GetNetworkRoutes where
  toPath GetNetworkRoutes' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-routes"
      ]

instance Data.ToQuery GetNetworkRoutes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkRoutesResponse' smart constructor.
data GetNetworkRoutesResponse = GetNetworkRoutesResponse'
  { -- | The ARN of the route table.
    routeTableArn :: Prelude.Maybe Prelude.Text,
    -- | The network routes.
    networkRoutes :: Prelude.Maybe [NetworkRoute],
    -- | The route table type.
    routeTableType :: Prelude.Maybe RouteTableType,
    -- | Describes a core network segment edge.
    coreNetworkSegmentEdge :: Prelude.Maybe CoreNetworkSegmentEdgeIdentifier,
    -- | The route table creation time.
    routeTableTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeTableArn', 'getNetworkRoutesResponse_routeTableArn' - The ARN of the route table.
--
-- 'networkRoutes', 'getNetworkRoutesResponse_networkRoutes' - The network routes.
--
-- 'routeTableType', 'getNetworkRoutesResponse_routeTableType' - The route table type.
--
-- 'coreNetworkSegmentEdge', 'getNetworkRoutesResponse_coreNetworkSegmentEdge' - Describes a core network segment edge.
--
-- 'routeTableTimestamp', 'getNetworkRoutesResponse_routeTableTimestamp' - The route table creation time.
--
-- 'httpStatus', 'getNetworkRoutesResponse_httpStatus' - The response's http status code.
newGetNetworkRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkRoutesResponse
newGetNetworkRoutesResponse pHttpStatus_ =
  GetNetworkRoutesResponse'
    { routeTableArn =
        Prelude.Nothing,
      networkRoutes = Prelude.Nothing,
      routeTableType = Prelude.Nothing,
      coreNetworkSegmentEdge = Prelude.Nothing,
      routeTableTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the route table.
getNetworkRoutesResponse_routeTableArn :: Lens.Lens' GetNetworkRoutesResponse (Prelude.Maybe Prelude.Text)
getNetworkRoutesResponse_routeTableArn = Lens.lens (\GetNetworkRoutesResponse' {routeTableArn} -> routeTableArn) (\s@GetNetworkRoutesResponse' {} a -> s {routeTableArn = a} :: GetNetworkRoutesResponse)

-- | The network routes.
getNetworkRoutesResponse_networkRoutes :: Lens.Lens' GetNetworkRoutesResponse (Prelude.Maybe [NetworkRoute])
getNetworkRoutesResponse_networkRoutes = Lens.lens (\GetNetworkRoutesResponse' {networkRoutes} -> networkRoutes) (\s@GetNetworkRoutesResponse' {} a -> s {networkRoutes = a} :: GetNetworkRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The route table type.
getNetworkRoutesResponse_routeTableType :: Lens.Lens' GetNetworkRoutesResponse (Prelude.Maybe RouteTableType)
getNetworkRoutesResponse_routeTableType = Lens.lens (\GetNetworkRoutesResponse' {routeTableType} -> routeTableType) (\s@GetNetworkRoutesResponse' {} a -> s {routeTableType = a} :: GetNetworkRoutesResponse)

-- | Describes a core network segment edge.
getNetworkRoutesResponse_coreNetworkSegmentEdge :: Lens.Lens' GetNetworkRoutesResponse (Prelude.Maybe CoreNetworkSegmentEdgeIdentifier)
getNetworkRoutesResponse_coreNetworkSegmentEdge = Lens.lens (\GetNetworkRoutesResponse' {coreNetworkSegmentEdge} -> coreNetworkSegmentEdge) (\s@GetNetworkRoutesResponse' {} a -> s {coreNetworkSegmentEdge = a} :: GetNetworkRoutesResponse)

-- | The route table creation time.
getNetworkRoutesResponse_routeTableTimestamp :: Lens.Lens' GetNetworkRoutesResponse (Prelude.Maybe Prelude.UTCTime)
getNetworkRoutesResponse_routeTableTimestamp = Lens.lens (\GetNetworkRoutesResponse' {routeTableTimestamp} -> routeTableTimestamp) (\s@GetNetworkRoutesResponse' {} a -> s {routeTableTimestamp = a} :: GetNetworkRoutesResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getNetworkRoutesResponse_httpStatus :: Lens.Lens' GetNetworkRoutesResponse Prelude.Int
getNetworkRoutesResponse_httpStatus = Lens.lens (\GetNetworkRoutesResponse' {httpStatus} -> httpStatus) (\s@GetNetworkRoutesResponse' {} a -> s {httpStatus = a} :: GetNetworkRoutesResponse)

instance Prelude.NFData GetNetworkRoutesResponse where
  rnf GetNetworkRoutesResponse' {..} =
    Prelude.rnf routeTableArn
      `Prelude.seq` Prelude.rnf networkRoutes
      `Prelude.seq` Prelude.rnf routeTableType
      `Prelude.seq` Prelude.rnf coreNetworkSegmentEdge
      `Prelude.seq` Prelude.rnf routeTableTimestamp
      `Prelude.seq` Prelude.rnf httpStatus

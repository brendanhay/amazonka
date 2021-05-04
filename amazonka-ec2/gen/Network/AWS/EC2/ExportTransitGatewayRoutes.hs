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
-- Module      : Network.AWS.EC2.ExportTransitGatewayRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports routes from the specified transit gateway route table to the
-- specified S3 bucket. By default, all routes are exported. Alternatively,
-- you can filter by CIDR range.
--
-- The routes are saved to the specified bucket in a JSON file. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-route-tables.html#tgw-export-route-tables Export Route Tables to Amazon S3>
-- in /Transit Gateways/.
module Network.AWS.EC2.ExportTransitGatewayRoutes
  ( -- * Creating a Request
    ExportTransitGatewayRoutes (..),
    newExportTransitGatewayRoutes,

    -- * Request Lenses
    exportTransitGatewayRoutes_dryRun,
    exportTransitGatewayRoutes_filters,
    exportTransitGatewayRoutes_transitGatewayRouteTableId,
    exportTransitGatewayRoutes_s3Bucket,

    -- * Destructuring the Response
    ExportTransitGatewayRoutesResponse (..),
    newExportTransitGatewayRoutesResponse,

    -- * Response Lenses
    exportTransitGatewayRoutesResponse_s3Location,
    exportTransitGatewayRoutesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportTransitGatewayRoutes' smart constructor.
data ExportTransitGatewayRoutes = ExportTransitGatewayRoutes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @attachment.transit-gateway-attachment-id@ - The id of the transit
    --     gateway attachment.
    --
    -- -   @attachment.resource-id@ - The resource id of the transit gateway
    --     attachment.
    --
    -- -   @route-search.exact-match@ - The exact match of the specified
    --     filter.
    --
    -- -   @route-search.longest-prefix-match@ - The longest prefix that
    --     matches the route.
    --
    -- -   @route-search.subnet-of-match@ - The routes with a subnet that match
    --     the specified CIDR filter.
    --
    -- -   @route-search.supernet-of-match@ - The routes with a CIDR that
    --     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
    --     10.0.1.0\/31 routes in your route table and you specify
    --     supernet-of-match as 10.0.1.0\/30, then the result returns
    --     10.0.1.0\/29.
    --
    -- -   @state@ - The state of the route (@active@ | @blackhole@).
    --
    -- -   @transit-gateway-route-destination-cidr-block@ - The CIDR range.
    --
    -- -   @type@ - The type of route (@propagated@ | @static@).
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The name of the S3 bucket.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTransitGatewayRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'exportTransitGatewayRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'exportTransitGatewayRoutes_filters' - One or more filters. The possible values are:
--
-- -   @attachment.transit-gateway-attachment-id@ - The id of the transit
--     gateway attachment.
--
-- -   @attachment.resource-id@ - The resource id of the transit gateway
--     attachment.
--
-- -   @route-search.exact-match@ - The exact match of the specified
--     filter.
--
-- -   @route-search.longest-prefix-match@ - The longest prefix that
--     matches the route.
--
-- -   @route-search.subnet-of-match@ - The routes with a subnet that match
--     the specified CIDR filter.
--
-- -   @route-search.supernet-of-match@ - The routes with a CIDR that
--     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
--     10.0.1.0\/31 routes in your route table and you specify
--     supernet-of-match as 10.0.1.0\/30, then the result returns
--     10.0.1.0\/29.
--
-- -   @state@ - The state of the route (@active@ | @blackhole@).
--
-- -   @transit-gateway-route-destination-cidr-block@ - The CIDR range.
--
-- -   @type@ - The type of route (@propagated@ | @static@).
--
-- 'transitGatewayRouteTableId', 'exportTransitGatewayRoutes_transitGatewayRouteTableId' - The ID of the route table.
--
-- 's3Bucket', 'exportTransitGatewayRoutes_s3Bucket' - The name of the S3 bucket.
newExportTransitGatewayRoutes ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 's3Bucket'
  Prelude.Text ->
  ExportTransitGatewayRoutes
newExportTransitGatewayRoutes
  pTransitGatewayRouteTableId_
  pS3Bucket_ =
    ExportTransitGatewayRoutes'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        s3Bucket = pS3Bucket_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportTransitGatewayRoutes_dryRun :: Lens.Lens' ExportTransitGatewayRoutes (Prelude.Maybe Prelude.Bool)
exportTransitGatewayRoutes_dryRun = Lens.lens (\ExportTransitGatewayRoutes' {dryRun} -> dryRun) (\s@ExportTransitGatewayRoutes' {} a -> s {dryRun = a} :: ExportTransitGatewayRoutes)

-- | One or more filters. The possible values are:
--
-- -   @attachment.transit-gateway-attachment-id@ - The id of the transit
--     gateway attachment.
--
-- -   @attachment.resource-id@ - The resource id of the transit gateway
--     attachment.
--
-- -   @route-search.exact-match@ - The exact match of the specified
--     filter.
--
-- -   @route-search.longest-prefix-match@ - The longest prefix that
--     matches the route.
--
-- -   @route-search.subnet-of-match@ - The routes with a subnet that match
--     the specified CIDR filter.
--
-- -   @route-search.supernet-of-match@ - The routes with a CIDR that
--     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
--     10.0.1.0\/31 routes in your route table and you specify
--     supernet-of-match as 10.0.1.0\/30, then the result returns
--     10.0.1.0\/29.
--
-- -   @state@ - The state of the route (@active@ | @blackhole@).
--
-- -   @transit-gateway-route-destination-cidr-block@ - The CIDR range.
--
-- -   @type@ - The type of route (@propagated@ | @static@).
exportTransitGatewayRoutes_filters :: Lens.Lens' ExportTransitGatewayRoutes (Prelude.Maybe [Filter])
exportTransitGatewayRoutes_filters = Lens.lens (\ExportTransitGatewayRoutes' {filters} -> filters) (\s@ExportTransitGatewayRoutes' {} a -> s {filters = a} :: ExportTransitGatewayRoutes) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the route table.
exportTransitGatewayRoutes_transitGatewayRouteTableId :: Lens.Lens' ExportTransitGatewayRoutes Prelude.Text
exportTransitGatewayRoutes_transitGatewayRouteTableId = Lens.lens (\ExportTransitGatewayRoutes' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@ExportTransitGatewayRoutes' {} a -> s {transitGatewayRouteTableId = a} :: ExportTransitGatewayRoutes)

-- | The name of the S3 bucket.
exportTransitGatewayRoutes_s3Bucket :: Lens.Lens' ExportTransitGatewayRoutes Prelude.Text
exportTransitGatewayRoutes_s3Bucket = Lens.lens (\ExportTransitGatewayRoutes' {s3Bucket} -> s3Bucket) (\s@ExportTransitGatewayRoutes' {} a -> s {s3Bucket = a} :: ExportTransitGatewayRoutes)

instance
  Prelude.AWSRequest
    ExportTransitGatewayRoutes
  where
  type
    Rs ExportTransitGatewayRoutes =
      ExportTransitGatewayRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ExportTransitGatewayRoutesResponse'
            Prelude.<$> (x Prelude..@? "s3Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportTransitGatewayRoutes

instance Prelude.NFData ExportTransitGatewayRoutes

instance Prelude.ToHeaders ExportTransitGatewayRoutes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ExportTransitGatewayRoutes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ExportTransitGatewayRoutes where
  toQuery ExportTransitGatewayRoutes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ExportTransitGatewayRoutes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "TransitGatewayRouteTableId"
          Prelude.=: transitGatewayRouteTableId,
        "S3Bucket" Prelude.=: s3Bucket
      ]

-- | /See:/ 'newExportTransitGatewayRoutesResponse' smart constructor.
data ExportTransitGatewayRoutesResponse = ExportTransitGatewayRoutesResponse'
  { -- | The URL of the exported file in Amazon S3. For example,
    -- s3:\/\//bucket_name/\/VPCTransitGateway\/TransitGatewayRouteTables\//file_name/.
    s3Location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTransitGatewayRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'exportTransitGatewayRoutesResponse_s3Location' - The URL of the exported file in Amazon S3. For example,
-- s3:\/\//bucket_name/\/VPCTransitGateway\/TransitGatewayRouteTables\//file_name/.
--
-- 'httpStatus', 'exportTransitGatewayRoutesResponse_httpStatus' - The response's http status code.
newExportTransitGatewayRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportTransitGatewayRoutesResponse
newExportTransitGatewayRoutesResponse pHttpStatus_ =
  ExportTransitGatewayRoutesResponse'
    { s3Location =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the exported file in Amazon S3. For example,
-- s3:\/\//bucket_name/\/VPCTransitGateway\/TransitGatewayRouteTables\//file_name/.
exportTransitGatewayRoutesResponse_s3Location :: Lens.Lens' ExportTransitGatewayRoutesResponse (Prelude.Maybe Prelude.Text)
exportTransitGatewayRoutesResponse_s3Location = Lens.lens (\ExportTransitGatewayRoutesResponse' {s3Location} -> s3Location) (\s@ExportTransitGatewayRoutesResponse' {} a -> s {s3Location = a} :: ExportTransitGatewayRoutesResponse)

-- | The response's http status code.
exportTransitGatewayRoutesResponse_httpStatus :: Lens.Lens' ExportTransitGatewayRoutesResponse Prelude.Int
exportTransitGatewayRoutesResponse_httpStatus = Lens.lens (\ExportTransitGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@ExportTransitGatewayRoutesResponse' {} a -> s {httpStatus = a} :: ExportTransitGatewayRoutesResponse)

instance
  Prelude.NFData
    ExportTransitGatewayRoutesResponse

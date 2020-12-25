{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportTransitGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports routes from the specified transit gateway route table to the specified S3 bucket. By default, all routes are exported. Alternatively, you can filter by CIDR range.
--
-- The routes are saved to the specified bucket in a JSON file. For more information, see <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-route-tables.html#tgw-export-route-tables Export Route Tables to Amazon S3> in /Transit Gateways/ .
module Network.AWS.EC2.ExportTransitGatewayRoutes
  ( -- * Creating a request
    ExportTransitGatewayRoutes (..),
    mkExportTransitGatewayRoutes,

    -- ** Request lenses
    etgrTransitGatewayRouteTableId,
    etgrS3Bucket,
    etgrDryRun,
    etgrFilters,

    -- * Destructuring the response
    ExportTransitGatewayRoutesResponse (..),
    mkExportTransitGatewayRoutesResponse,

    -- ** Response lenses
    etgrrrsS3Location,
    etgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportTransitGatewayRoutes' smart constructor.
data ExportTransitGatewayRoutes = ExportTransitGatewayRoutes'
  { -- | The ID of the route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The name of the S3 bucket.
    s3Bucket :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
    --
    --
    --     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
    --
    --
    --     * @route-search.exact-match@ - The exact match of the specified filter.
    --
    --
    --     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
    --
    --
    --     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
    --
    --
    --     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
    --
    --
    --     * @state@ - The state of the route (@active@ | @blackhole@ ).
    --
    --
    --     * @transit-gateway-route-destination-cidr-block@ - The CIDR range.
    --
    --
    --     * @type@ - The type of route (@propagated@ | @static@ ).
    filters :: Core.Maybe [Types.Filter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTransitGatewayRoutes' value with any optional fields omitted.
mkExportTransitGatewayRoutes ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 's3Bucket'
  Types.String ->
  ExportTransitGatewayRoutes
mkExportTransitGatewayRoutes transitGatewayRouteTableId s3Bucket =
  ExportTransitGatewayRoutes'
    { transitGatewayRouteTableId,
      s3Bucket,
      dryRun = Core.Nothing,
      filters = Core.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrTransitGatewayRouteTableId :: Lens.Lens' ExportTransitGatewayRoutes Types.TransitGatewayRouteTableId
etgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED etgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrS3Bucket :: Lens.Lens' ExportTransitGatewayRoutes Types.String
etgrS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED etgrS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrDryRun :: Lens.Lens' ExportTransitGatewayRoutes (Core.Maybe Core.Bool)
etgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED etgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
--
--
--     * @route-search.exact-match@ - The exact match of the specified filter.
--
--
--     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
--
--
--     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
--
--
--     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
--
--
--     * @state@ - The state of the route (@active@ | @blackhole@ ).
--
--
--     * @transit-gateway-route-destination-cidr-block@ - The CIDR range.
--
--
--     * @type@ - The type of route (@propagated@ | @static@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrFilters :: Lens.Lens' ExportTransitGatewayRoutes (Core.Maybe [Types.Filter])
etgrFilters = Lens.field @"filters"
{-# DEPRECATED etgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.AWSRequest ExportTransitGatewayRoutes where
  type
    Rs ExportTransitGatewayRoutes =
      ExportTransitGatewayRoutesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ExportTransitGatewayRoutes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "S3Bucket" s3Bucket)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ExportTransitGatewayRoutesResponse'
            Core.<$> (x Core..@? "s3Location") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExportTransitGatewayRoutesResponse' smart constructor.
data ExportTransitGatewayRoutesResponse = ExportTransitGatewayRoutesResponse'
  { -- | The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
    s3Location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTransitGatewayRoutesResponse' value with any optional fields omitted.
mkExportTransitGatewayRoutesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExportTransitGatewayRoutesResponse
mkExportTransitGatewayRoutesResponse responseStatus =
  ExportTransitGatewayRoutesResponse'
    { s3Location = Core.Nothing,
      responseStatus
    }

-- | The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrrsS3Location :: Lens.Lens' ExportTransitGatewayRoutesResponse (Core.Maybe Types.String)
etgrrrsS3Location = Lens.field @"s3Location"
{-# DEPRECATED etgrrrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrrsResponseStatus :: Lens.Lens' ExportTransitGatewayRoutesResponse Core.Int
etgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED etgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

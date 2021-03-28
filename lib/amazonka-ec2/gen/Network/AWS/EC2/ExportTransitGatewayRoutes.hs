{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ExportTransitGatewayRoutes (..)
    , mkExportTransitGatewayRoutes
    -- ** Request lenses
    , etgrTransitGatewayRouteTableId
    , etgrS3Bucket
    , etgrDryRun
    , etgrFilters

    -- * Destructuring the response
    , ExportTransitGatewayRoutesResponse (..)
    , mkExportTransitGatewayRoutesResponse
    -- ** Response lenses
    , etgrrrsS3Location
    , etgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportTransitGatewayRoutes' smart constructor.
data ExportTransitGatewayRoutes = ExportTransitGatewayRoutes'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the route table.
  , s3Bucket :: Core.Text
    -- ^ The name of the S3 bucket.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTransitGatewayRoutes' value with any optional fields omitted.
mkExportTransitGatewayRoutes
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Core.Text -- ^ 's3Bucket'
    -> ExportTransitGatewayRoutes
mkExportTransitGatewayRoutes transitGatewayRouteTableId s3Bucket
  = ExportTransitGatewayRoutes'{transitGatewayRouteTableId, s3Bucket,
                                dryRun = Core.Nothing, filters = Core.Nothing}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrTransitGatewayRouteTableId :: Lens.Lens' ExportTransitGatewayRoutes Types.TransitGatewayRouteTableId
etgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE etgrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrS3Bucket :: Lens.Lens' ExportTransitGatewayRoutes Core.Text
etgrS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE etgrS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrDryRun :: Lens.Lens' ExportTransitGatewayRoutes (Core.Maybe Core.Bool)
etgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE etgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE etgrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

instance Core.ToQuery ExportTransitGatewayRoutes where
        toQuery ExportTransitGatewayRoutes{..}
          = Core.toQueryPair "Action"
              ("ExportTransitGatewayRoutes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<> Core.toQueryPair "S3Bucket" s3Bucket
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters

instance Core.ToHeaders ExportTransitGatewayRoutes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ExportTransitGatewayRoutes where
        type Rs ExportTransitGatewayRoutes =
             ExportTransitGatewayRoutesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ExportTransitGatewayRoutesResponse' Core.<$>
                   (x Core..@? "s3Location") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExportTransitGatewayRoutesResponse' smart constructor.
data ExportTransitGatewayRoutesResponse = ExportTransitGatewayRoutesResponse'
  { s3Location :: Core.Maybe Core.Text
    -- ^ The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTransitGatewayRoutesResponse' value with any optional fields omitted.
mkExportTransitGatewayRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportTransitGatewayRoutesResponse
mkExportTransitGatewayRoutesResponse responseStatus
  = ExportTransitGatewayRoutesResponse'{s3Location = Core.Nothing,
                                        responseStatus}

-- | The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrrsS3Location :: Lens.Lens' ExportTransitGatewayRoutesResponse (Core.Maybe Core.Text)
etgrrrsS3Location = Lens.field @"s3Location"
{-# INLINEABLE etgrrrsS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrrsResponseStatus :: Lens.Lens' ExportTransitGatewayRoutesResponse Core.Int
etgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE etgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

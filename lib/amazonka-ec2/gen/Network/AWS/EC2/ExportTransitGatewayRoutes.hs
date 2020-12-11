{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    etgrFilters,
    etgrDryRun,
    etgrTransitGatewayRouteTableId,
    etgrS3Bucket,

    -- * Destructuring the response
    ExportTransitGatewayRoutesResponse (..),
    mkExportTransitGatewayRoutesResponse,

    -- ** Response lenses
    etgrrsS3Location,
    etgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportTransitGatewayRoutes' smart constructor.
data ExportTransitGatewayRoutes = ExportTransitGatewayRoutes'
  { filters ::
      Lude.Maybe [Filter],
    dryRun :: Lude.Maybe Lude.Bool,
    transitGatewayRouteTableId ::
      Lude.Text,
    s3Bucket :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTransitGatewayRoutes' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
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
-- * 's3Bucket' - The name of the S3 bucket.
-- * 'transitGatewayRouteTableId' - The ID of the route table.
mkExportTransitGatewayRoutes ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 's3Bucket'
  Lude.Text ->
  ExportTransitGatewayRoutes
mkExportTransitGatewayRoutes
  pTransitGatewayRouteTableId_
  pS3Bucket_ =
    ExportTransitGatewayRoutes'
      { filters = Lude.Nothing,
        dryRun = Lude.Nothing,
        transitGatewayRouteTableId = pTransitGatewayRouteTableId_,
        s3Bucket = pS3Bucket_
      }

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
etgrFilters :: Lens.Lens' ExportTransitGatewayRoutes (Lude.Maybe [Filter])
etgrFilters = Lens.lens (filters :: ExportTransitGatewayRoutes -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: ExportTransitGatewayRoutes)
{-# DEPRECATED etgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrDryRun :: Lens.Lens' ExportTransitGatewayRoutes (Lude.Maybe Lude.Bool)
etgrDryRun = Lens.lens (dryRun :: ExportTransitGatewayRoutes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ExportTransitGatewayRoutes)
{-# DEPRECATED etgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrTransitGatewayRouteTableId :: Lens.Lens' ExportTransitGatewayRoutes Lude.Text
etgrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: ExportTransitGatewayRoutes -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: ExportTransitGatewayRoutes)
{-# DEPRECATED etgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrS3Bucket :: Lens.Lens' ExportTransitGatewayRoutes Lude.Text
etgrS3Bucket = Lens.lens (s3Bucket :: ExportTransitGatewayRoutes -> Lude.Text) (\s a -> s {s3Bucket = a} :: ExportTransitGatewayRoutes)
{-# DEPRECATED etgrS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.AWSRequest ExportTransitGatewayRoutes where
  type
    Rs ExportTransitGatewayRoutes =
      ExportTransitGatewayRoutesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ExportTransitGatewayRoutesResponse'
            Lude.<$> (x Lude..@? "s3Location") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportTransitGatewayRoutes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExportTransitGatewayRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportTransitGatewayRoutes where
  toQuery ExportTransitGatewayRoutes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ExportTransitGatewayRoutes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "S3Bucket" Lude.=: s3Bucket
      ]

-- | /See:/ 'mkExportTransitGatewayRoutesResponse' smart constructor.
data ExportTransitGatewayRoutesResponse = ExportTransitGatewayRoutesResponse'
  { s3Location ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTransitGatewayRoutesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 's3Location' - The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
mkExportTransitGatewayRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportTransitGatewayRoutesResponse
mkExportTransitGatewayRoutesResponse pResponseStatus_ =
  ExportTransitGatewayRoutesResponse'
    { s3Location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrsS3Location :: Lens.Lens' ExportTransitGatewayRoutesResponse (Lude.Maybe Lude.Text)
etgrrsS3Location = Lens.lens (s3Location :: ExportTransitGatewayRoutesResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Location = a} :: ExportTransitGatewayRoutesResponse)
{-# DEPRECATED etgrrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrrsResponseStatus :: Lens.Lens' ExportTransitGatewayRoutesResponse Lude.Int
etgrrsResponseStatus = Lens.lens (responseStatus :: ExportTransitGatewayRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportTransitGatewayRoutesResponse)
{-# DEPRECATED etgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

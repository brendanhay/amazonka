{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default certificate and the certificate list for the specified HTTPS or TLS listener.
--
-- If the default certificate is also in the certificate list, it appears twice in the results (once with @IsDefault@ set to true and once with @IsDefault@ set to false).
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#https-listener-certificates SSL certificates> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#tls-listener-certificate Server certificates> in the /Network Load Balancers Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListenerCertificates
  ( -- * Creating a request
    DescribeListenerCertificates (..),
    mkDescribeListenerCertificates,

    -- ** Request lenses
    dlcListenerArn,
    dlcMarker,
    dlcPageSize,

    -- * Destructuring the response
    DescribeListenerCertificatesResponse (..),
    mkDescribeListenerCertificatesResponse,

    -- ** Response lenses
    dlcrrsCertificates,
    dlcrrsNextMarker,
    dlcrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { -- | The Amazon Resource Names (ARN) of the listener.
    listenerArn :: Types.ListenerArn,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListenerCertificates' value with any optional fields omitted.
mkDescribeListenerCertificates ::
  -- | 'listenerArn'
  Types.ListenerArn ->
  DescribeListenerCertificates
mkDescribeListenerCertificates listenerArn =
  DescribeListenerCertificates'
    { listenerArn,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The Amazon Resource Names (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcListenerArn :: Lens.Lens' DescribeListenerCertificates Types.ListenerArn
dlcListenerArn = Lens.field @"listenerArn"
{-# DEPRECATED dlcListenerArn "Use generic-lens or generic-optics with 'listenerArn' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcMarker :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Types.Marker)
dlcMarker = Lens.field @"marker"
{-# DEPRECATED dlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcPageSize :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Core.Natural)
dlcPageSize = Lens.field @"pageSize"
{-# DEPRECATED dlcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest DescribeListenerCertificates where
  type
    Rs DescribeListenerCertificates =
      DescribeListenerCertificatesResponse
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
            ( Core.pure ("Action", "DescribeListenerCertificates")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "ListenerArn" listenerArn)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "PageSize" Core.<$> pageSize)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeListenerCertificatesResult"
      ( \s h x ->
          DescribeListenerCertificatesResponse'
            Core.<$> (x Core..@? "Certificates" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeListenerCertificates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"certificates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkDescribeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { -- | Information about the certificates.
    certificates :: Core.Maybe [Types.Certificate],
    -- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListenerCertificatesResponse' value with any optional fields omitted.
mkDescribeListenerCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeListenerCertificatesResponse
mkDescribeListenerCertificatesResponse responseStatus =
  DescribeListenerCertificatesResponse'
    { certificates =
        Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | Information about the certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsCertificates :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe [Types.Certificate])
dlcrrsCertificates = Lens.field @"certificates"
{-# DEPRECATED dlcrrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsNextMarker :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe Types.Marker)
dlcrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED dlcrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsResponseStatus :: Lens.Lens' DescribeListenerCertificatesResponse Core.Int
dlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

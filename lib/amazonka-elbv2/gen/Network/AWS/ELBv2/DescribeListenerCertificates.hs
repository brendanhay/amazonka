{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeListenerCertificates (..)
    , mkDescribeListenerCertificates
    -- ** Request lenses
    , dlcListenerArn
    , dlcMarker
    , dlcPageSize

    -- * Destructuring the response
    , DescribeListenerCertificatesResponse (..)
    , mkDescribeListenerCertificatesResponse
    -- ** Response lenses
    , dlcrrsCertificates
    , dlcrrsNextMarker
    , dlcrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { listenerArn :: Types.ListenerArn
    -- ^ The Amazon Resource Names (ARN) of the listener.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with this call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListenerCertificates' value with any optional fields omitted.
mkDescribeListenerCertificates
    :: Types.ListenerArn -- ^ 'listenerArn'
    -> DescribeListenerCertificates
mkDescribeListenerCertificates listenerArn
  = DescribeListenerCertificates'{listenerArn, marker = Core.Nothing,
                                  pageSize = Core.Nothing}

-- | The Amazon Resource Names (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcListenerArn :: Lens.Lens' DescribeListenerCertificates Types.ListenerArn
dlcListenerArn = Lens.field @"listenerArn"
{-# INLINEABLE dlcListenerArn #-}
{-# DEPRECATED listenerArn "Use generic-lens or generic-optics with 'listenerArn' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcMarker :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Types.Marker)
dlcMarker = Lens.field @"marker"
{-# INLINEABLE dlcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcPageSize :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Core.Natural)
dlcPageSize = Lens.field @"pageSize"
{-# INLINEABLE dlcPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery DescribeListenerCertificates where
        toQuery DescribeListenerCertificates{..}
          = Core.toQueryPair "Action"
              ("DescribeListenerCertificates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ListenerArn" listenerArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageSize") pageSize

instance Core.ToHeaders DescribeListenerCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeListenerCertificates where
        type Rs DescribeListenerCertificates =
             DescribeListenerCertificatesResponse
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
          = Response.receiveXMLWrapper "DescribeListenerCertificatesResult"
              (\ s h x ->
                 DescribeListenerCertificatesResponse' Core.<$>
                   (x Core..@? "Certificates" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeListenerCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"certificates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkDescribeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { certificates :: Core.Maybe [Types.Certificate]
    -- ^ Information about the certificates.
  , nextMarker :: Core.Maybe Types.Marker
    -- ^ If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListenerCertificatesResponse' value with any optional fields omitted.
mkDescribeListenerCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeListenerCertificatesResponse
mkDescribeListenerCertificatesResponse responseStatus
  = DescribeListenerCertificatesResponse'{certificates =
                                            Core.Nothing,
                                          nextMarker = Core.Nothing, responseStatus}

-- | Information about the certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsCertificates :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe [Types.Certificate])
dlcrrsCertificates = Lens.field @"certificates"
{-# INLINEABLE dlcrrsCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsNextMarker :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe Types.Marker)
dlcrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE dlcrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsResponseStatus :: Lens.Lens' DescribeListenerCertificatesResponse Core.Int
dlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

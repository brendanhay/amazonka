{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeSSLPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies or all policies used for SSL negotiation.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeSSLPolicies
    (
    -- * Creating a request
      DescribeSSLPolicies (..)
    , mkDescribeSSLPolicies
    -- ** Request lenses
    , dsslpMarker
    , dsslpNames
    , dsslpPageSize

    -- * Destructuring the response
    , DescribeSSLPoliciesResponse (..)
    , mkDescribeSSLPoliciesResponse
    -- ** Response lenses
    , dsslprrsNextMarker
    , dsslprrsSslPolicies
    , dsslprrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSSLPolicies' smart constructor.
data DescribeSSLPolicies = DescribeSSLPolicies'
  { marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , names :: Core.Maybe [Types.SslPolicyName]
    -- ^ The names of the policies.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with this call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSSLPolicies' value with any optional fields omitted.
mkDescribeSSLPolicies
    :: DescribeSSLPolicies
mkDescribeSSLPolicies
  = DescribeSSLPolicies'{marker = Core.Nothing, names = Core.Nothing,
                         pageSize = Core.Nothing}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslpMarker :: Lens.Lens' DescribeSSLPolicies (Core.Maybe Types.Marker)
dsslpMarker = Lens.field @"marker"
{-# INLINEABLE dsslpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The names of the policies.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslpNames :: Lens.Lens' DescribeSSLPolicies (Core.Maybe [Types.SslPolicyName])
dsslpNames = Lens.field @"names"
{-# INLINEABLE dsslpNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslpPageSize :: Lens.Lens' DescribeSSLPolicies (Core.Maybe Core.Natural)
dsslpPageSize = Lens.field @"pageSize"
{-# INLINEABLE dsslpPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery DescribeSSLPolicies where
        toQuery DescribeSSLPolicies{..}
          = Core.toQueryPair "Action" ("DescribeSSLPolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.toQueryPair "Names"
                (Core.maybe Core.mempty (Core.toQueryList "member") names)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageSize") pageSize

instance Core.ToHeaders DescribeSSLPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSSLPolicies where
        type Rs DescribeSSLPolicies = DescribeSSLPoliciesResponse
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
          = Response.receiveXMLWrapper "DescribeSSLPoliciesResult"
              (\ s h x ->
                 DescribeSSLPoliciesResponse' Core.<$>
                   (x Core..@? "NextMarker") Core.<*>
                     x Core..@? "SslPolicies" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSSLPolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"sslPolicies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkDescribeSSLPoliciesResponse' smart constructor.
data DescribeSSLPoliciesResponse = DescribeSSLPoliciesResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
  , sslPolicies :: Core.Maybe [Types.SslPolicy]
    -- ^ Information about the security policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSSLPoliciesResponse' value with any optional fields omitted.
mkDescribeSSLPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSSLPoliciesResponse
mkDescribeSSLPoliciesResponse responseStatus
  = DescribeSSLPoliciesResponse'{nextMarker = Core.Nothing,
                                 sslPolicies = Core.Nothing, responseStatus}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslprrsNextMarker :: Lens.Lens' DescribeSSLPoliciesResponse (Core.Maybe Types.NextMarker)
dsslprrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE dsslprrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | Information about the security policies.
--
-- /Note:/ Consider using 'sslPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslprrsSslPolicies :: Lens.Lens' DescribeSSLPoliciesResponse (Core.Maybe [Types.SslPolicy])
dsslprrsSslPolicies = Lens.field @"sslPolicies"
{-# INLINEABLE dsslprrsSslPolicies #-}
{-# DEPRECATED sslPolicies "Use generic-lens or generic-optics with 'sslPolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsslprrsResponseStatus :: Lens.Lens' DescribeSSLPoliciesResponse Core.Int
dsslprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsslprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

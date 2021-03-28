{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
module Network.AWS.ELB.DescribeTags
    (
    -- * Creating a request
      DescribeTags (..)
    , mkDescribeTags
    -- ** Request lenses
    , dtLoadBalancerNames

    -- * Destructuring the response
    , DescribeTagsResponse (..)
    , mkDescribeTagsResponse
    -- ** Response lenses
    , dtrrsTagDescriptions
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeTags.
--
-- /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { loadBalancerNames :: Core.NonEmpty Types.AccessPointName
    -- ^ The names of the load balancers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTags' value with any optional fields omitted.
mkDescribeTags
    :: Core.NonEmpty Types.AccessPointName -- ^ 'loadBalancerNames'
    -> DescribeTags
mkDescribeTags loadBalancerNames = DescribeTags'{loadBalancerNames}

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLoadBalancerNames :: Lens.Lens' DescribeTags (Core.NonEmpty Types.AccessPointName)
dtLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE dtLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

instance Core.ToQuery DescribeTags where
        toQuery DescribeTags{..}
          = Core.toQueryPair "Action" ("DescribeTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.toQueryList "member" loadBalancerNames)

instance Core.ToHeaders DescribeTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
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
          = Response.receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' Core.<$>
                   (x Core..@? "TagDescriptions" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output for DescribeTags.
--
-- /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { tagDescriptions :: Core.Maybe [Types.TagDescription]
    -- ^ Information about the tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagsResponse' value with any optional fields omitted.
mkDescribeTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTagsResponse
mkDescribeTagsResponse responseStatus
  = DescribeTagsResponse'{tagDescriptions = Core.Nothing,
                          responseStatus}

-- | Information about the tags.
--
-- /Note:/ Consider using 'tagDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTagDescriptions :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Types.TagDescription])
dtrrsTagDescriptions = Lens.field @"tagDescriptions"
{-# INLINEABLE dtrrsTagDescriptions #-}
{-# DEPRECATED tagDescriptions "Use generic-lens or generic-optics with 'tagDescriptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTagsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

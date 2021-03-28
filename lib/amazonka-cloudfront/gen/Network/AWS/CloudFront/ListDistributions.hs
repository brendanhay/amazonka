{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List CloudFront distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Creating a request
      ListDistributions (..)
    , mkListDistributions
    -- ** Request lenses
    , ldMarker
    , ldMaxItems

    -- * Destructuring the response
    , ListDistributionsResponse (..)
    , mkListDistributionsResponse
    -- ** Response lenses
    , ldrrsDistributionList
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list your distributions. 
--
-- /See:/ 'mkListDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of distributions you want in the response body.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributions' value with any optional fields omitted.
mkListDistributions
    :: ListDistributions
mkListDistributions
  = ListDistributions'{marker = Core.Nothing,
                       maxItems = Core.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMarker :: Lens.Lens' ListDistributions (Core.Maybe Core.Text)
ldMarker = Lens.field @"marker"
{-# INLINEABLE ldMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of distributions you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxItems :: Lens.Lens' ListDistributions (Core.Maybe Core.Text)
ldMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ldMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListDistributions where
        toQuery ListDistributions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListDistributions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDistributions where
        type Rs ListDistributions = ListDistributionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/distribution",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListDistributionsResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDistributions where
        page rq rs
          | Pager.stop
              (rs Lens.^.
                 Lens.field @"distributionList" Core.. Lens.field @"isTruncated")
            = Core.Nothing
          | Core.isNothing
              (rs Lens.^.
                 Lens.field @"distributionList" Core.. Lens.field @"nextMarker")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~
                   rs Lens.^.
                     Lens.field @"distributionList" Core.. Lens.field @"nextMarker")

-- | The returned result of the corresponding request. 
--
-- /See:/ 'mkListDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
  { distributionList :: Types.DistributionList
    -- ^ The @DistributionList@ type. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDistributionsResponse' value with any optional fields omitted.
mkListDistributionsResponse
    :: Types.DistributionList -- ^ 'distributionList'
    -> Core.Int -- ^ 'responseStatus'
    -> ListDistributionsResponse
mkListDistributionsResponse distributionList responseStatus
  = ListDistributionsResponse'{distributionList, responseStatus}

-- | The @DistributionList@ type. 
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDistributionList :: Lens.Lens' ListDistributionsResponse Types.DistributionList
ldrrsDistributionList = Lens.field @"distributionList"
{-# INLINEABLE ldrrsDistributionList #-}
{-# DEPRECATED distributionList "Use generic-lens or generic-optics with 'distributionList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDistributionsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that references the specified key group.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByKeyGroup
    (
    -- * Creating a request
      ListDistributionsByKeyGroup (..)
    , mkListDistributionsByKeyGroup
    -- ** Request lenses
    , ldbkgKeyGroupId
    , ldbkgMarker
    , ldbkgMaxItems

    -- * Destructuring the response
    , ListDistributionsByKeyGroupResponse (..)
    , mkListDistributionsByKeyGroupResponse
    -- ** Response lenses
    , ldbkgrrsDistributionIdList
    , ldbkgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByKeyGroup' smart constructor.
data ListDistributionsByKeyGroup = ListDistributionsByKeyGroup'
  { keyGroupId :: Core.Text
    -- ^ The ID of the key group whose associated distribution IDs you are listing.
  , marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of distribution IDs that you want in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByKeyGroup' value with any optional fields omitted.
mkListDistributionsByKeyGroup
    :: Core.Text -- ^ 'keyGroupId'
    -> ListDistributionsByKeyGroup
mkListDistributionsByKeyGroup keyGroupId
  = ListDistributionsByKeyGroup'{keyGroupId, marker = Core.Nothing,
                                 maxItems = Core.Nothing}

-- | The ID of the key group whose associated distribution IDs you are listing.
--
-- /Note:/ Consider using 'keyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgKeyGroupId :: Lens.Lens' ListDistributionsByKeyGroup Core.Text
ldbkgKeyGroupId = Lens.field @"keyGroupId"
{-# INLINEABLE ldbkgKeyGroupId #-}
{-# DEPRECATED keyGroupId "Use generic-lens or generic-optics with 'keyGroupId' instead"  #-}

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgMarker :: Lens.Lens' ListDistributionsByKeyGroup (Core.Maybe Core.Text)
ldbkgMarker = Lens.field @"marker"
{-# INLINEABLE ldbkgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgMaxItems :: Lens.Lens' ListDistributionsByKeyGroup (Core.Maybe Core.Text)
ldbkgMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ldbkgMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListDistributionsByKeyGroup where
        toQuery ListDistributionsByKeyGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListDistributionsByKeyGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDistributionsByKeyGroup where
        type Rs ListDistributionsByKeyGroup =
             ListDistributionsByKeyGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distributionsByKeyGroupId/" Core.<>
                             Core.toText keyGroupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListDistributionsByKeyGroupResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListDistributionsByKeyGroupResponse' smart constructor.
data ListDistributionsByKeyGroupResponse = ListDistributionsByKeyGroupResponse'
  { distributionIdList :: Core.Maybe Types.DistributionIdList
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByKeyGroupResponse' value with any optional fields omitted.
mkListDistributionsByKeyGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDistributionsByKeyGroupResponse
mkListDistributionsByKeyGroupResponse responseStatus
  = ListDistributionsByKeyGroupResponse'{distributionIdList =
                                           Core.Nothing,
                                         responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgrrsDistributionIdList :: Lens.Lens' ListDistributionsByKeyGroupResponse (Core.Maybe Types.DistributionIdList)
ldbkgrrsDistributionIdList = Lens.field @"distributionIdList"
{-# INLINEABLE ldbkgrrsDistributionIdList #-}
{-# DEPRECATED distributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgrrsResponseStatus :: Lens.Lens' ListDistributionsByKeyGroupResponse Core.Int
ldbkgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldbkgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

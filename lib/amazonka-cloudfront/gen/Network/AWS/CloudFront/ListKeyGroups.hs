{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of key groups.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListKeyGroups
    (
    -- * Creating a request
      ListKeyGroups (..)
    , mkListKeyGroups
    -- ** Request lenses
    , lkgMarker
    , lkgMaxItems

    -- * Destructuring the response
    , ListKeyGroupsResponse (..)
    , mkListKeyGroupsResponse
    -- ** Response lenses
    , lkgrrsKeyGroupList
    , lkgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of key groups that you want in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListKeyGroups' value with any optional fields omitted.
mkListKeyGroups
    :: ListKeyGroups
mkListKeyGroups
  = ListKeyGroups'{marker = Core.Nothing, maxItems = Core.Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgMarker :: Lens.Lens' ListKeyGroups (Core.Maybe Core.Text)
lkgMarker = Lens.field @"marker"
{-# INLINEABLE lkgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of key groups that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgMaxItems :: Lens.Lens' ListKeyGroups (Core.Maybe Core.Text)
lkgMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lkgMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListKeyGroups where
        toQuery ListKeyGroups{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListKeyGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListKeyGroups where
        type Rs ListKeyGroups = ListKeyGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/key-group",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListKeyGroupsResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListKeyGroupsResponse' smart constructor.
data ListKeyGroupsResponse = ListKeyGroupsResponse'
  { keyGroupList :: Core.Maybe Types.KeyGroupList
    -- ^ A list of key groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListKeyGroupsResponse' value with any optional fields omitted.
mkListKeyGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListKeyGroupsResponse
mkListKeyGroupsResponse responseStatus
  = ListKeyGroupsResponse'{keyGroupList = Core.Nothing,
                           responseStatus}

-- | A list of key groups.
--
-- /Note:/ Consider using 'keyGroupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgrrsKeyGroupList :: Lens.Lens' ListKeyGroupsResponse (Core.Maybe Types.KeyGroupList)
lkgrrsKeyGroupList = Lens.field @"keyGroupList"
{-# INLINEABLE lkgrrsKeyGroupList #-}
{-# DEPRECATED keyGroupList "Use generic-lens or generic-optics with 'keyGroupList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgrrsResponseStatus :: Lens.Lens' ListKeyGroupsResponse Core.Int
lkgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lkgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

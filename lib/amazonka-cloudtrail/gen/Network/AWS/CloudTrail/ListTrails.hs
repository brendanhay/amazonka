{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListTrails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists trails that are in the current account.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTrails
    (
    -- * Creating a request
      ListTrails (..)
    , mkListTrails
    -- ** Request lenses
    , lNextToken

    -- * Destructuring the response
    , ListTrailsResponse (..)
    , mkListTrailsResponse
    -- ** Response lenses
    , lrsNextToken
    , lrsTrails
    , lrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTrails' smart constructor.
newtype ListTrails = ListTrails'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrails' value with any optional fields omitted.
mkListTrails
    :: ListTrails
mkListTrails = ListTrails'{nextToken = Core.Nothing}

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListTrails (Core.Maybe Core.Text)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTrails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTrails where
        toHeaders ListTrails{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTrails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTrails where
        toJSON ListTrails{..}
          = Core.object
              (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTrails where
        type Rs ListTrails = ListTrailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTrailsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Trails" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTrails where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"trails" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTrailsResponse' smart constructor.
data ListTrailsResponse = ListTrailsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
  , trails :: Core.Maybe [Types.TrailInfo]
    -- ^ Returns the name, ARN, and home region of trails in the current account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrailsResponse' value with any optional fields omitted.
mkListTrailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTrailsResponse
mkListTrailsResponse responseStatus
  = ListTrailsResponse'{nextToken = Core.Nothing,
                        trails = Core.Nothing, responseStatus}

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListTrailsResponse (Core.Maybe Core.Text)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns the name, ARN, and home region of trails in the current account.
--
-- /Note:/ Consider using 'trails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsTrails :: Lens.Lens' ListTrailsResponse (Core.Maybe [Types.TrailInfo])
lrsTrails = Lens.field @"trails"
{-# INLINEABLE lrsTrails #-}
{-# DEPRECATED trails "Use generic-lens or generic-optics with 'trails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTrailsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferings
    (
    -- * Creating a request
      ListOfferings (..)
    , mkListOfferings
    -- ** Request lenses
    , loNextToken

    -- * Destructuring the response
    , ListOfferingsResponse (..)
    , mkListOfferingsResponse
    -- ** Response lenses
    , lorrsNextToken
    , lorrsOfferings
    , lorrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list all offerings.
--
-- /See:/ 'mkListOfferings' smart constructor.
newtype ListOfferings = ListOfferings'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListOfferings' value with any optional fields omitted.
mkListOfferings
    :: ListOfferings
mkListOfferings = ListOfferings'{nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOfferings (Core.Maybe Types.PaginationToken)
loNextToken = Lens.field @"nextToken"
{-# INLINEABLE loNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListOfferings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListOfferings where
        toHeaders ListOfferings{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListOfferings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListOfferings where
        toJSON ListOfferings{..}
          = Core.object
              (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListOfferings where
        type Rs ListOfferings = ListOfferingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOfferingsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "offerings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOfferings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"offerings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the return values of the list of offerings.
--
-- /See:/ 'mkListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , offerings :: Core.Maybe [Types.Offering]
    -- ^ A value that represents the list offering results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOfferingsResponse' value with any optional fields omitted.
mkListOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOfferingsResponse
mkListOfferingsResponse responseStatus
  = ListOfferingsResponse'{nextToken = Core.Nothing,
                           offerings = Core.Nothing, responseStatus}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextToken :: Lens.Lens' ListOfferingsResponse (Core.Maybe Types.PaginationToken)
lorrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lorrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A value that represents the list offering results.
--
-- /Note:/ Consider using 'offerings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsOfferings :: Lens.Lens' ListOfferingsResponse (Core.Maybe [Types.Offering])
lorrsOfferings = Lens.field @"offerings"
{-# INLINEABLE lorrsOfferings #-}
{-# DEPRECATED offerings "Use generic-lens or generic-optics with 'offerings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListOfferingsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

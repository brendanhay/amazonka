{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListHapgs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Lists the high-availability partition groups for the account.
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListHapgs@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListHapgs
    (
    -- * Creating a request
      ListHapgs (..)
    , mkListHapgs
    -- ** Request lenses
    , lhNextToken

    -- * Destructuring the response
    , ListHapgsResponse (..)
    , mkListHapgsResponse
    -- ** Response lenses
    , lrsHapgList
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListHapgs' smart constructor.
newtype ListHapgs = ListHapgs'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The @NextToken@ value from a previous call to @ListHapgs@ . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListHapgs' value with any optional fields omitted.
mkListHapgs
    :: ListHapgs
mkListHapgs = ListHapgs'{nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListHapgs@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNextToken :: Lens.Lens' ListHapgs (Core.Maybe Types.PaginationToken)
lhNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListHapgs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListHapgs where
        toHeaders ListHapgs{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.ListHapgs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListHapgs where
        toJSON ListHapgs{..}
          = Core.object
              (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListHapgs where
        type Rs ListHapgs = ListHapgsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListHapgsResponse' Core.<$>
                   (x Core..:? "HapgList" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListHapgs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"hapgList") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListHapgsResponse' smart constructor.
data ListHapgsResponse = ListHapgsResponse'
  { hapgList :: [Types.HapgArn]
    -- ^ The list of high-availability partition groups.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value to @ListHapgs@ to retrieve the next set of items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHapgsResponse' value with any optional fields omitted.
mkListHapgsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListHapgsResponse
mkListHapgsResponse responseStatus
  = ListHapgsResponse'{hapgList = Core.mempty,
                       nextToken = Core.Nothing, responseStatus}

-- | The list of high-availability partition groups.
--
-- /Note:/ Consider using 'hapgList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsHapgList :: Lens.Lens' ListHapgsResponse [Types.HapgArn]
lrsHapgList = Lens.field @"hapgList"
{-# INLINEABLE lrsHapgList #-}
{-# DEPRECATED hapgList "Use generic-lens or generic-optics with 'hapgList' instead"  #-}

-- | If not null, more results are available. Pass this value to @ListHapgs@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListHapgsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListHapgsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListHsms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves the identifiers of all of the HSMs provisioned for the current customer.
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListHsms@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListHsms
    (
    -- * Creating a request
      ListHsms (..)
    , mkListHsms
    -- ** Request lenses
    , lNextToken

    -- * Destructuring the response
    , ListHsmsResponse (..)
    , mkListHsmsResponse
    -- ** Response lenses
    , lhrrsHsmList
    , lhrrsNextToken
    , lhrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListHsms' smart constructor.
newtype ListHsms = ListHsms'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListHsms' value with any optional fields omitted.
mkListHsms
    :: ListHsms
mkListHsms = ListHsms'{nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListHsms (Core.Maybe Types.PaginationToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListHsms where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListHsms where
        toHeaders ListHsms{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.ListHsms")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListHsms where
        toJSON ListHsms{..}
          = Core.object
              (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListHsms where
        type Rs ListHsms = ListHsmsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListHsmsResponse' Core.<$>
                   (x Core..:? "HsmList") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListHsms where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"hsmList" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of the @ListHsms@ operation.
--
-- /See:/ 'mkListHsmsResponse' smart constructor.
data ListHsmsResponse = ListHsmsResponse'
  { hsmList :: Core.Maybe [Types.HsmArn]
    -- ^ The list of ARNs that identify the HSMs.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHsmsResponse' value with any optional fields omitted.
mkListHsmsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListHsmsResponse
mkListHsmsResponse responseStatus
  = ListHsmsResponse'{hsmList = Core.Nothing,
                      nextToken = Core.Nothing, responseStatus}

-- | The list of ARNs that identify the HSMs.
--
-- /Note:/ Consider using 'hsmList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrrsHsmList :: Lens.Lens' ListHsmsResponse (Core.Maybe [Types.HsmArn])
lhrrsHsmList = Lens.field @"hsmList"
{-# INLINEABLE lhrrsHsmList #-}
{-# DEPRECATED hsmList "Use generic-lens or generic-optics with 'hsmList' instead"  #-}

-- | If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrrsNextToken :: Lens.Lens' ListHsmsResponse (Core.Maybe Types.NextToken)
lhrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrrsResponseStatus :: Lens.Lens' ListHsmsResponse Core.Int
lhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

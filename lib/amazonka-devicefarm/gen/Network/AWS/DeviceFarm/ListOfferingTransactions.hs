{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListOfferingTransactions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferingTransactions
    (
    -- * Creating a request
      ListOfferingTransactions (..)
    , mkListOfferingTransactions
    -- ** Request lenses
    , lotNextToken

    -- * Destructuring the response
    , ListOfferingTransactionsResponse (..)
    , mkListOfferingTransactionsResponse
    -- ** Response lenses
    , lotrrsNextToken
    , lotrrsOfferingTransactions
    , lotrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the offering transaction history.
--
-- /See:/ 'mkListOfferingTransactions' smart constructor.
newtype ListOfferingTransactions = ListOfferingTransactions'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListOfferingTransactions' value with any optional fields omitted.
mkListOfferingTransactions
    :: ListOfferingTransactions
mkListOfferingTransactions
  = ListOfferingTransactions'{nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotNextToken :: Lens.Lens' ListOfferingTransactions (Core.Maybe Types.NextToken)
lotNextToken = Lens.field @"nextToken"
{-# INLINEABLE lotNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListOfferingTransactions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListOfferingTransactions where
        toHeaders ListOfferingTransactions{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListOfferingTransactions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListOfferingTransactions where
        toJSON ListOfferingTransactions{..}
          = Core.object
              (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListOfferingTransactions where
        type Rs ListOfferingTransactions = ListOfferingTransactionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOfferingTransactionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "offeringTransactions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOfferingTransactions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"offeringTransactions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Returns the transaction log of the specified offerings.
--
-- /See:/ 'mkListOfferingTransactionsResponse' smart constructor.
data ListOfferingTransactionsResponse = ListOfferingTransactionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , offeringTransactions :: Core.Maybe [Types.OfferingTransaction]
    -- ^ The audit log of subscriptions you have purchased and modified through AWS Device Farm.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListOfferingTransactionsResponse' value with any optional fields omitted.
mkListOfferingTransactionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOfferingTransactionsResponse
mkListOfferingTransactionsResponse responseStatus
  = ListOfferingTransactionsResponse'{nextToken = Core.Nothing,
                                      offeringTransactions = Core.Nothing, responseStatus}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrrsNextToken :: Lens.Lens' ListOfferingTransactionsResponse (Core.Maybe Types.NextToken)
lotrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lotrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The audit log of subscriptions you have purchased and modified through AWS Device Farm.
--
-- /Note:/ Consider using 'offeringTransactions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrrsOfferingTransactions :: Lens.Lens' ListOfferingTransactionsResponse (Core.Maybe [Types.OfferingTransaction])
lotrrsOfferingTransactions = Lens.field @"offeringTransactions"
{-# INLINEABLE lotrrsOfferingTransactions #-}
{-# DEPRECATED offeringTransactions "Use generic-lens or generic-optics with 'offeringTransactions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrrsResponseStatus :: Lens.Lens' ListOfferingTransactionsResponse Core.Int
lotrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lotrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchContacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches contacts and lists the ones that meet a set of filter and sort criteria.
module Network.AWS.AlexaBusiness.SearchContacts
    (
    -- * Creating a request
      SearchContacts (..)
    , mkSearchContacts
    -- ** Request lenses
    , scFilters
    , scMaxResults
    , scNextToken
    , scSortCriteria

    -- * Destructuring the response
    , SearchContactsResponse (..)
    , mkSearchContactsResponse
    -- ** Response lenses
    , scrrsContacts
    , scrrsNextToken
    , scrrsTotalCount
    , scrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchContacts' smart constructor.
data SearchContacts = SearchContacts'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
  , sortCriteria :: Core.Maybe [Types.Sort]
    -- ^ The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchContacts' value with any optional fields omitted.
mkSearchContacts
    :: SearchContacts
mkSearchContacts
  = SearchContacts'{filters = Core.Nothing,
                    maxResults = Core.Nothing, nextToken = Core.Nothing,
                    sortCriteria = Core.Nothing}

-- | The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFilters :: Lens.Lens' SearchContacts (Core.Maybe [Types.Filter])
scFilters = Lens.field @"filters"
{-# INLINEABLE scFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxResults :: Lens.Lens' SearchContacts (Core.Maybe Core.Natural)
scMaxResults = Lens.field @"maxResults"
{-# INLINEABLE scMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNextToken :: Lens.Lens' SearchContacts (Core.Maybe Types.NextToken)
scNextToken = Lens.field @"nextToken"
{-# INLINEABLE scNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSortCriteria :: Lens.Lens' SearchContacts (Core.Maybe [Types.Sort])
scSortCriteria = Lens.field @"sortCriteria"
{-# INLINEABLE scSortCriteria #-}
{-# DEPRECATED sortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead"  #-}

instance Core.ToQuery SearchContacts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchContacts where
        toHeaders SearchContacts{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.SearchContacts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchContacts where
        toJSON SearchContacts{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortCriteria" Core..=) Core.<$> sortCriteria])

instance Core.AWSRequest SearchContacts where
        type Rs SearchContacts = SearchContactsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchContactsResponse' Core.<$>
                   (x Core..:? "Contacts") Core.<*> x Core..:? "NextToken" Core.<*>
                     x Core..:? "TotalCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchContactsResponse' smart constructor.
data SearchContactsResponse = SearchContactsResponse'
  { contacts :: Core.Maybe [Types.ContactData]
    -- ^ The contacts that meet the specified set of filter criteria, in sort order.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned to indicate that there is more data available.
  , totalCount :: Core.Maybe Core.Int
    -- ^ The total number of contacts returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchContactsResponse' value with any optional fields omitted.
mkSearchContactsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchContactsResponse
mkSearchContactsResponse responseStatus
  = SearchContactsResponse'{contacts = Core.Nothing,
                            nextToken = Core.Nothing, totalCount = Core.Nothing,
                            responseStatus}

-- | The contacts that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'contacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsContacts :: Lens.Lens' SearchContactsResponse (Core.Maybe [Types.ContactData])
scrrsContacts = Lens.field @"contacts"
{-# INLINEABLE scrrsContacts #-}
{-# DEPRECATED contacts "Use generic-lens or generic-optics with 'contacts' instead"  #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsNextToken :: Lens.Lens' SearchContactsResponse (Core.Maybe Types.NextToken)
scrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE scrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The total number of contacts returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsTotalCount :: Lens.Lens' SearchContactsResponse (Core.Maybe Core.Int)
scrrsTotalCount = Lens.field @"totalCount"
{-# INLINEABLE scrrsTotalCount #-}
{-# DEPRECATED totalCount "Use generic-lens or generic-optics with 'totalCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' SearchContactsResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

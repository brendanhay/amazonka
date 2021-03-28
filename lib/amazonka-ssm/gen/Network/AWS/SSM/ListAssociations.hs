{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all State Manager associations in the current AWS account and Region. You can limit the results to a specific State Manager association document or instance by specifying a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociations
    (
    -- * Creating a request
      ListAssociations (..)
    , mkListAssociations
    -- ** Request lenses
    , laAssociationFilterList
    , laMaxResults
    , laNextToken

    -- * Destructuring the response
    , ListAssociationsResponse (..)
    , mkListAssociationsResponse
    -- ** Response lenses
    , larrsAssociations
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { associationFilterList :: Core.Maybe (Core.NonEmpty Types.AssociationFilter)
    -- ^ One or more filters. Use a filter to return a more specific list of results.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociations' value with any optional fields omitted.
mkListAssociations
    :: ListAssociations
mkListAssociations
  = ListAssociations'{associationFilterList = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'associationFilterList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAssociationFilterList :: Lens.Lens' ListAssociations (Core.Maybe (Core.NonEmpty Types.AssociationFilter))
laAssociationFilterList = Lens.field @"associationFilterList"
{-# INLINEABLE laAssociationFilterList #-}
{-# DEPRECATED associationFilterList "Use generic-lens or generic-optics with 'associationFilterList' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAssociations (Core.Maybe Core.Natural)
laMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAssociations (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssociations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssociations where
        toHeaders ListAssociations{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ListAssociations") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssociations where
        toJSON ListAssociations{..}
          = Core.object
              (Core.catMaybes
                 [("AssociationFilterList" Core..=) Core.<$> associationFilterList,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssociations where
        type Rs ListAssociations = ListAssociationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssociationsResponse' Core.<$>
                   (x Core..:? "Associations") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssociations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"associations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { associations :: Core.Maybe [Types.Association]
    -- ^ The associations.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAssociationsResponse' value with any optional fields omitted.
mkListAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssociationsResponse
mkListAssociationsResponse responseStatus
  = ListAssociationsResponse'{associations = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | The associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAssociations :: Lens.Lens' ListAssociationsResponse (Core.Maybe [Types.Association])
larrsAssociations = Lens.field @"associations"
{-# INLINEABLE larrsAssociations #-}
{-# DEPRECATED associations "Use generic-lens or generic-optics with 'associations' instead"  #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAssociationsResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAssociationsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

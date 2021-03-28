{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListAssociationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all versions of an association for a specific association ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociationVersions
    (
    -- * Creating a request
      ListAssociationVersions (..)
    , mkListAssociationVersions
    -- ** Request lenses
    , lavAssociationId
    , lavMaxResults
    , lavNextToken

    -- * Destructuring the response
    , ListAssociationVersionsResponse (..)
    , mkListAssociationVersionsResponse
    -- ** Response lenses
    , lavrrsAssociationVersions
    , lavrrsNextToken
    , lavrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { associationId :: Types.AssociationId
    -- ^ The association ID for which you want to view all versions.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociationVersions' value with any optional fields omitted.
mkListAssociationVersions
    :: Types.AssociationId -- ^ 'associationId'
    -> ListAssociationVersions
mkListAssociationVersions associationId
  = ListAssociationVersions'{associationId,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The association ID for which you want to view all versions.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavAssociationId :: Lens.Lens' ListAssociationVersions Types.AssociationId
lavAssociationId = Lens.field @"associationId"
{-# INLINEABLE lavAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxResults :: Lens.Lens' ListAssociationVersions (Core.Maybe Core.Natural)
lavMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lavMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListAssociationVersions (Core.Maybe Types.NextToken)
lavNextToken = Lens.field @"nextToken"
{-# INLINEABLE lavNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssociationVersions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssociationVersions where
        toHeaders ListAssociationVersions{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ListAssociationVersions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssociationVersions where
        toJSON ListAssociationVersions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssociationId" Core..= associationId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssociationVersions where
        type Rs ListAssociationVersions = ListAssociationVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssociationVersionsResponse' Core.<$>
                   (x Core..:? "AssociationVersions") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssociationVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"associationVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { associationVersions :: Core.Maybe (Core.NonEmpty Types.AssociationVersionInfo)
    -- ^ Information about all versions of the association for the specified association ID.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAssociationVersionsResponse' value with any optional fields omitted.
mkListAssociationVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssociationVersionsResponse
mkListAssociationVersionsResponse responseStatus
  = ListAssociationVersionsResponse'{associationVersions =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | Information about all versions of the association for the specified association ID.
--
-- /Note:/ Consider using 'associationVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsAssociationVersions :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe (Core.NonEmpty Types.AssociationVersionInfo))
lavrrsAssociationVersions = Lens.field @"associationVersions"
{-# INLINEABLE lavrrsAssociationVersions #-}
{-# DEPRECATED associationVersions "Use generic-lens or generic-optics with 'associationVersions' instead"  #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsNextToken :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe Types.NextToken)
lavrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lavrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsResponseStatus :: Lens.Lens' ListAssociationVersionsResponse Core.Int
lavrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lavrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

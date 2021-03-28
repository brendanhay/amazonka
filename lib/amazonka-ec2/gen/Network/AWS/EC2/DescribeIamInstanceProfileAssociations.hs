{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIamInstanceProfileAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IAM instance profile associations.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIamInstanceProfileAssociations
    (
    -- * Creating a request
      DescribeIamInstanceProfileAssociations (..)
    , mkDescribeIamInstanceProfileAssociations
    -- ** Request lenses
    , diipaAssociationIds
    , diipaFilters
    , diipaMaxResults
    , diipaNextToken

    -- * Destructuring the response
    , DescribeIamInstanceProfileAssociationsResponse (..)
    , mkDescribeIamInstanceProfileAssociationsResponse
    -- ** Response lenses
    , diiparrsIamInstanceProfileAssociations
    , diiparrsNextToken
    , diiparrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIamInstanceProfileAssociations' smart constructor.
data DescribeIamInstanceProfileAssociations = DescribeIamInstanceProfileAssociations'
  { associationIds :: Core.Maybe [Types.IamInstanceProfileAssociationId]
    -- ^ The IAM instance profile associations.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIamInstanceProfileAssociations' value with any optional fields omitted.
mkDescribeIamInstanceProfileAssociations
    :: DescribeIamInstanceProfileAssociations
mkDescribeIamInstanceProfileAssociations
  = DescribeIamInstanceProfileAssociations'{associationIds =
                                              Core.Nothing,
                                            filters = Core.Nothing, maxResults = Core.Nothing,
                                            nextToken = Core.Nothing}

-- | The IAM instance profile associations.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diipaAssociationIds :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe [Types.IamInstanceProfileAssociationId])
diipaAssociationIds = Lens.field @"associationIds"
{-# INLINEABLE diipaAssociationIds #-}
{-# DEPRECATED associationIds "Use generic-lens or generic-optics with 'associationIds' instead"  #-}

-- | The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diipaFilters :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe [Types.Filter])
diipaFilters = Lens.field @"filters"
{-# INLINEABLE diipaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diipaMaxResults :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe Core.Natural)
diipaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diipaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diipaNextToken :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe Types.NextToken)
diipaNextToken = Lens.field @"nextToken"
{-# INLINEABLE diipaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeIamInstanceProfileAssociations where
        toQuery DescribeIamInstanceProfileAssociations{..}
          = Core.toQueryPair "Action"
              ("DescribeIamInstanceProfileAssociations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AssociationId")
                associationIds
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeIamInstanceProfileAssociations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeIamInstanceProfileAssociations
         where
        type Rs DescribeIamInstanceProfileAssociations =
             DescribeIamInstanceProfileAssociationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeIamInstanceProfileAssociationsResponse' Core.<$>
                   (x Core..@? "iamInstanceProfileAssociationSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeIamInstanceProfileAssociations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"iamInstanceProfileAssociations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeIamInstanceProfileAssociationsResponse' smart constructor.
data DescribeIamInstanceProfileAssociationsResponse = DescribeIamInstanceProfileAssociationsResponse'
  { iamInstanceProfileAssociations :: Core.Maybe [Types.IamInstanceProfileAssociation]
    -- ^ Information about the IAM instance profile associations.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeIamInstanceProfileAssociationsResponse' value with any optional fields omitted.
mkDescribeIamInstanceProfileAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIamInstanceProfileAssociationsResponse
mkDescribeIamInstanceProfileAssociationsResponse responseStatus
  = DescribeIamInstanceProfileAssociationsResponse'{iamInstanceProfileAssociations
                                                      = Core.Nothing,
                                                    nextToken = Core.Nothing, responseStatus}

-- | Information about the IAM instance profile associations.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiparrsIamInstanceProfileAssociations :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Core.Maybe [Types.IamInstanceProfileAssociation])
diiparrsIamInstanceProfileAssociations = Lens.field @"iamInstanceProfileAssociations"
{-# INLINEABLE diiparrsIamInstanceProfileAssociations #-}
{-# DEPRECATED iamInstanceProfileAssociations "Use generic-lens or generic-optics with 'iamInstanceProfileAssociations' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiparrsNextToken :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Core.Maybe Types.NextToken)
diiparrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diiparrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiparrsResponseStatus :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse Core.Int
diiparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diiparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more association proposals for connection between a virtual private gateway or transit gateway and a Direct Connect gateway. 
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
    (
    -- * Creating a request
      DescribeDirectConnectGatewayAssociationProposals (..)
    , mkDescribeDirectConnectGatewayAssociationProposals
    -- ** Request lenses
    , ddcgapsAssociatedGatewayId
    , ddcgapsDirectConnectGatewayId
    , ddcgapsMaxResults
    , ddcgapsNextToken
    , ddcgapsProposalId

    -- * Destructuring the response
    , DescribeDirectConnectGatewayAssociationProposalsResponse (..)
    , mkDescribeDirectConnectGatewayAssociationProposalsResponse
    -- ** Response lenses
    , ddcgaprfrsDirectConnectGatewayAssociationProposals
    , ddcgaprfrsNextToken
    , ddcgaprfrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociationProposals' smart constructor.
data DescribeDirectConnectGatewayAssociationProposals = DescribeDirectConnectGatewayAssociationProposals'
  { associatedGatewayId :: Core.Maybe Types.AssociatedGatewayId
    -- ^ The ID of the associated gateway.
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token for the next page of results.
  , proposalId :: Core.Maybe Types.DirectConnectGatewayAssociationProposalId
    -- ^ The ID of the proposal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGatewayAssociationProposals' value with any optional fields omitted.
mkDescribeDirectConnectGatewayAssociationProposals
    :: DescribeDirectConnectGatewayAssociationProposals
mkDescribeDirectConnectGatewayAssociationProposals
  = DescribeDirectConnectGatewayAssociationProposals'{associatedGatewayId
                                                        = Core.Nothing,
                                                      directConnectGatewayId = Core.Nothing,
                                                      maxResults = Core.Nothing,
                                                      nextToken = Core.Nothing,
                                                      proposalId = Core.Nothing}

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'associatedGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsAssociatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Core.Maybe Types.AssociatedGatewayId)
ddcgapsAssociatedGatewayId = Lens.field @"associatedGatewayId"
{-# INLINEABLE ddcgapsAssociatedGatewayId #-}
{-# DEPRECATED associatedGatewayId "Use generic-lens or generic-optics with 'associatedGatewayId' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Core.Maybe Types.DirectConnectGatewayId)
ddcgapsDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE ddcgapsDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Core.Maybe Core.Int)
ddcgapsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ddcgapsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Core.Maybe Types.PaginationToken)
ddcgapsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgapsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsProposalId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Core.Maybe Types.DirectConnectGatewayAssociationProposalId)
ddcgapsProposalId = Lens.field @"proposalId"
{-# INLINEABLE ddcgapsProposalId #-}
{-# DEPRECATED proposalId "Use generic-lens or generic-optics with 'proposalId' instead"  #-}

instance Core.ToQuery
           DescribeDirectConnectGatewayAssociationProposals
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DescribeDirectConnectGatewayAssociationProposals
         where
        toHeaders DescribeDirectConnectGatewayAssociationProposals{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.DescribeDirectConnectGatewayAssociationProposals")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           DescribeDirectConnectGatewayAssociationProposals
         where
        toJSON DescribeDirectConnectGatewayAssociationProposals{..}
          = Core.object
              (Core.catMaybes
                 [("associatedGatewayId" Core..=) Core.<$> associatedGatewayId,
                  ("directConnectGatewayId" Core..=) Core.<$> directConnectGatewayId,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("proposalId" Core..=) Core.<$> proposalId])

instance Core.AWSRequest
           DescribeDirectConnectGatewayAssociationProposals
         where
        type Rs DescribeDirectConnectGatewayAssociationProposals =
             DescribeDirectConnectGatewayAssociationProposalsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewayAssociationProposalsResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociationProposals") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociationProposalsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationProposalsResponse = DescribeDirectConnectGatewayAssociationProposalsResponse'
  { directConnectGatewayAssociationProposals :: Core.Maybe [Types.DirectConnectGatewayAssociationProposal]
    -- ^ Describes the Direct Connect gateway association proposals.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGatewayAssociationProposalsResponse' value with any optional fields omitted.
mkDescribeDirectConnectGatewayAssociationProposalsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDirectConnectGatewayAssociationProposalsResponse
mkDescribeDirectConnectGatewayAssociationProposalsResponse
  responseStatus
  = DescribeDirectConnectGatewayAssociationProposalsResponse'{directConnectGatewayAssociationProposals
                                                                = Core.Nothing,
                                                              nextToken = Core.Nothing,
                                                              responseStatus}

-- | Describes the Direct Connect gateway association proposals.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprfrsDirectConnectGatewayAssociationProposals :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Core.Maybe [Types.DirectConnectGatewayAssociationProposal])
ddcgaprfrsDirectConnectGatewayAssociationProposals = Lens.field @"directConnectGatewayAssociationProposals"
{-# INLINEABLE ddcgaprfrsDirectConnectGatewayAssociationProposals #-}
{-# DEPRECATED directConnectGatewayAssociationProposals "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposals' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprfrsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Core.Maybe Types.PaginationToken)
ddcgaprfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgaprfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprfrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse Core.Int
ddcgaprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcgaprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

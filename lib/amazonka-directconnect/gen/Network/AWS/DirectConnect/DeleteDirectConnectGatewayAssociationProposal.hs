{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association proposal request between the specified Direct Connect gateway and virtual private gateway or transit gateway.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
    (
    -- * Creating a request
      DeleteDirectConnectGatewayAssociationProposal (..)
    , mkDeleteDirectConnectGatewayAssociationProposal
    -- ** Request lenses
    , ddcgapProposalId

    -- * Destructuring the response
    , DeleteDirectConnectGatewayAssociationProposalResponse (..)
    , mkDeleteDirectConnectGatewayAssociationProposalResponse
    -- ** Response lenses
    , ddcgaprrsDirectConnectGatewayAssociationProposal
    , ddcgaprrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposal' smart constructor.
newtype DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { proposalId :: Types.DirectConnectGatewayAssociationProposalId
    -- ^ The ID of the proposal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociationProposal' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociationProposal
    :: Types.DirectConnectGatewayAssociationProposalId -- ^ 'proposalId'
    -> DeleteDirectConnectGatewayAssociationProposal
mkDeleteDirectConnectGatewayAssociationProposal proposalId
  = DeleteDirectConnectGatewayAssociationProposal'{proposalId}

-- | The ID of the proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapProposalId :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposal Types.DirectConnectGatewayAssociationProposalId
ddcgapProposalId = Lens.field @"proposalId"
{-# INLINEABLE ddcgapProposalId #-}
{-# DEPRECATED proposalId "Use generic-lens or generic-optics with 'proposalId' instead"  #-}

instance Core.ToQuery DeleteDirectConnectGatewayAssociationProposal
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DeleteDirectConnectGatewayAssociationProposal
         where
        toHeaders DeleteDirectConnectGatewayAssociationProposal{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.DeleteDirectConnectGatewayAssociationProposal")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           DeleteDirectConnectGatewayAssociationProposal
         where
        toJSON DeleteDirectConnectGatewayAssociationProposal{..}
          = Core.object
              (Core.catMaybes [Core.Just ("proposalId" Core..= proposalId)])

instance Core.AWSRequest
           DeleteDirectConnectGatewayAssociationProposal
         where
        type Rs DeleteDirectConnectGatewayAssociationProposal =
             DeleteDirectConnectGatewayAssociationProposalResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDirectConnectGatewayAssociationProposalResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAssociationProposal") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposalResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationProposalResponse = DeleteDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociationProposal :: Core.Maybe Types.DirectConnectGatewayAssociationProposal
    -- ^ The ID of the associated gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociationProposalResponse' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociationProposalResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDirectConnectGatewayAssociationProposalResponse
mkDeleteDirectConnectGatewayAssociationProposalResponse
  responseStatus
  = DeleteDirectConnectGatewayAssociationProposalResponse'{directConnectGatewayAssociationProposal
                                                             = Core.Nothing,
                                                           responseStatus}

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprrsDirectConnectGatewayAssociationProposal :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse (Core.Maybe Types.DirectConnectGatewayAssociationProposal)
ddcgaprrsDirectConnectGatewayAssociationProposal = Lens.field @"directConnectGatewayAssociationProposal"
{-# INLINEABLE ddcgaprrsDirectConnectGatewayAssociationProposal #-}
{-# DEPRECATED directConnectGatewayAssociationProposal "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposal' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprrsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse Core.Int
ddcgaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcgaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

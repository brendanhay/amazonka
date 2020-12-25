{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteDirectConnectGatewayAssociationProposal (..),
    mkDeleteDirectConnectGatewayAssociationProposal,

    -- ** Request lenses
    ddcgapProposalId,

    -- * Destructuring the response
    DeleteDirectConnectGatewayAssociationProposalResponse (..),
    mkDeleteDirectConnectGatewayAssociationProposalResponse,

    -- ** Response lenses
    ddcgaprrsDirectConnectGatewayAssociationProposal,
    ddcgaprrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposal' smart constructor.
newtype DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { -- | The ID of the proposal.
    proposalId :: Types.DirectConnectGatewayAssociationProposalId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociationProposal' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociationProposal ::
  -- | 'proposalId'
  Types.DirectConnectGatewayAssociationProposalId ->
  DeleteDirectConnectGatewayAssociationProposal
mkDeleteDirectConnectGatewayAssociationProposal proposalId =
  DeleteDirectConnectGatewayAssociationProposal' {proposalId}

-- | The ID of the proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapProposalId :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposal Types.DirectConnectGatewayAssociationProposalId
ddcgapProposalId = Lens.field @"proposalId"
{-# DEPRECATED ddcgapProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

instance
  Core.FromJSON
    DeleteDirectConnectGatewayAssociationProposal
  where
  toJSON DeleteDirectConnectGatewayAssociationProposal {..} =
    Core.object
      (Core.catMaybes [Core.Just ("proposalId" Core..= proposalId)])

instance
  Core.AWSRequest
    DeleteDirectConnectGatewayAssociationProposal
  where
  type
    Rs DeleteDirectConnectGatewayAssociationProposal =
      DeleteDirectConnectGatewayAssociationProposalResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "OvertureService.DeleteDirectConnectGatewayAssociationProposal"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationProposalResponse'
            Core.<$> (x Core..:? "directConnectGatewayAssociationProposal")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposalResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationProposalResponse = DeleteDirectConnectGatewayAssociationProposalResponse'
  { -- | The ID of the associated gateway.
    directConnectGatewayAssociationProposal :: Core.Maybe Types.DirectConnectGatewayAssociationProposal,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayAssociationProposalResponse' value with any optional fields omitted.
mkDeleteDirectConnectGatewayAssociationProposalResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDirectConnectGatewayAssociationProposalResponse
mkDeleteDirectConnectGatewayAssociationProposalResponse
  responseStatus =
    DeleteDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Core.Nothing,
        responseStatus
      }

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprrsDirectConnectGatewayAssociationProposal :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse (Core.Maybe Types.DirectConnectGatewayAssociationProposal)
ddcgaprrsDirectConnectGatewayAssociationProposal = Lens.field @"directConnectGatewayAssociationProposal"
{-# DEPRECATED ddcgaprrsDirectConnectGatewayAssociationProposal "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposal' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprrsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse Core.Int
ddcgaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcgaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

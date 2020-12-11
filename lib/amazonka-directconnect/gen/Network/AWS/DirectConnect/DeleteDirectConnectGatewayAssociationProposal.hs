{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ddcgaprsDirectConnectGatewayAssociationProposal,
    ddcgaprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposal' smart constructor.
newtype DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { proposalId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteDirectConnectGatewayAssociationProposal' with the minimum fields required to make a request.
--
-- * 'proposalId' - The ID of the proposal.
mkDeleteDirectConnectGatewayAssociationProposal ::
  -- | 'proposalId'
  Lude.Text ->
  DeleteDirectConnectGatewayAssociationProposal
mkDeleteDirectConnectGatewayAssociationProposal pProposalId_ =
  DeleteDirectConnectGatewayAssociationProposal'
    { proposalId =
        pProposalId_
    }

-- | The ID of the proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapProposalId :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposal Lude.Text
ddcgapProposalId = Lens.lens (proposalId :: DeleteDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {proposalId = a} :: DeleteDirectConnectGatewayAssociationProposal)
{-# DEPRECATED ddcgapProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

instance
  Lude.AWSRequest
    DeleteDirectConnectGatewayAssociationProposal
  where
  type
    Rs DeleteDirectConnectGatewayAssociationProposal =
      DeleteDirectConnectGatewayAssociationProposalResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationProposalResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociationProposal")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DeleteDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DeleteDirectConnectGatewayAssociationProposal" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDirectConnectGatewayAssociationProposal where
  toJSON DeleteDirectConnectGatewayAssociationProposal' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("proposalId" Lude..= proposalId)])

instance Lude.ToPath DeleteDirectConnectGatewayAssociationProposal where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDirectConnectGatewayAssociationProposal where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationProposalResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationProposalResponse = DeleteDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociationProposal ::
      Lude.Maybe
        DirectConnectGatewayAssociationProposal,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteDirectConnectGatewayAssociationProposalResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociationProposal' - The ID of the associated gateway.
-- * 'responseStatus' - The response status code.
mkDeleteDirectConnectGatewayAssociationProposalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDirectConnectGatewayAssociationProposalResponse
mkDeleteDirectConnectGatewayAssociationProposalResponse
  pResponseStatus_ =
    DeleteDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprsDirectConnectGatewayAssociationProposal :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse (Lude.Maybe DirectConnectGatewayAssociationProposal)
ddcgaprsDirectConnectGatewayAssociationProposal = Lens.lens (directConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposalResponse -> Lude.Maybe DirectConnectGatewayAssociationProposal) (\s a -> s {directConnectGatewayAssociationProposal = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED ddcgaprsDirectConnectGatewayAssociationProposal "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposal' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaprsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse Lude.Int
ddcgaprsResponseStatus = Lens.lens (responseStatus :: DeleteDirectConnectGatewayAssociationProposalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED ddcgaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a proposal request to attach a virtual private gateway or transit gateway to a Direct Connect gateway.
module Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
  ( -- * Creating a request
    AcceptDirectConnectGatewayAssociationProposal (..),
    mkAcceptDirectConnectGatewayAssociationProposal,

    -- ** Request lenses
    adcgapOverrideAllowedPrefixesToDirectConnectGateway,
    adcgapDirectConnectGatewayId,
    adcgapProposalId,
    adcgapAssociatedGatewayOwnerAccount,

    -- * Destructuring the response
    AcceptDirectConnectGatewayAssociationProposalResponse (..),
    mkAcceptDirectConnectGatewayAssociationProposalResponse,

    -- ** Response lenses
    adcgaprsDirectConnectGatewayAssociation,
    adcgaprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptDirectConnectGatewayAssociationProposal' smart constructor.
data AcceptDirectConnectGatewayAssociationProposal = AcceptDirectConnectGatewayAssociationProposal'
  { overrideAllowedPrefixesToDirectConnectGateway ::
      Lude.Maybe
        [RouteFilterPrefix],
    directConnectGatewayId ::
      Lude.Text,
    proposalId ::
      Lude.Text,
    associatedGatewayOwnerAccount ::
      Lude.Text
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

-- | Creates a value of 'AcceptDirectConnectGatewayAssociationProposal' with the minimum fields required to make a request.
--
-- * 'associatedGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway or transit gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'overrideAllowedPrefixesToDirectConnectGateway' - Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
-- * 'proposalId' - The ID of the request proposal.
mkAcceptDirectConnectGatewayAssociationProposal ::
  -- | 'directConnectGatewayId'
  Lude.Text ->
  -- | 'proposalId'
  Lude.Text ->
  -- | 'associatedGatewayOwnerAccount'
  Lude.Text ->
  AcceptDirectConnectGatewayAssociationProposal
mkAcceptDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pProposalId_
  pAssociatedGatewayOwnerAccount_ =
    AcceptDirectConnectGatewayAssociationProposal'
      { overrideAllowedPrefixesToDirectConnectGateway =
          Lude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_,
        proposalId = pProposalId_,
        associatedGatewayOwnerAccount =
          pAssociatedGatewayOwnerAccount_
      }

-- | Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway.
--
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- /Note:/ Consider using 'overrideAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapOverrideAllowedPrefixesToDirectConnectGateway :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal (Lude.Maybe [RouteFilterPrefix])
adcgapOverrideAllowedPrefixesToDirectConnectGateway = Lens.lens (overrideAllowedPrefixesToDirectConnectGateway :: AcceptDirectConnectGatewayAssociationProposal -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {overrideAllowedPrefixesToDirectConnectGateway = a} :: AcceptDirectConnectGatewayAssociationProposal)
{-# DEPRECATED adcgapOverrideAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'overrideAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapDirectConnectGatewayId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Lude.Text
adcgapDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: AcceptDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {directConnectGatewayId = a} :: AcceptDirectConnectGatewayAssociationProposal)
{-# DEPRECATED adcgapDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the request proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapProposalId :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Lude.Text
adcgapProposalId = Lens.lens (proposalId :: AcceptDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {proposalId = a} :: AcceptDirectConnectGatewayAssociationProposal)
{-# DEPRECATED adcgapProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

-- | The ID of the AWS account that owns the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'associatedGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgapAssociatedGatewayOwnerAccount :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposal Lude.Text
adcgapAssociatedGatewayOwnerAccount = Lens.lens (associatedGatewayOwnerAccount :: AcceptDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {associatedGatewayOwnerAccount = a} :: AcceptDirectConnectGatewayAssociationProposal)
{-# DEPRECATED adcgapAssociatedGatewayOwnerAccount "Use generic-lens or generic-optics with 'associatedGatewayOwnerAccount' instead." #-}

instance
  Lude.AWSRequest
    AcceptDirectConnectGatewayAssociationProposal
  where
  type
    Rs AcceptDirectConnectGatewayAssociationProposal =
      AcceptDirectConnectGatewayAssociationProposalResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcceptDirectConnectGatewayAssociationProposalResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    AcceptDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.AcceptDirectConnectGatewayAssociationProposal" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptDirectConnectGatewayAssociationProposal where
  toJSON AcceptDirectConnectGatewayAssociationProposal' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("overrideAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> overrideAllowedPrefixesToDirectConnectGateway,
            Lude.Just
              ("directConnectGatewayId" Lude..= directConnectGatewayId),
            Lude.Just ("proposalId" Lude..= proposalId),
            Lude.Just
              ( "associatedGatewayOwnerAccount"
                  Lude..= associatedGatewayOwnerAccount
              )
          ]
      )

instance Lude.ToPath AcceptDirectConnectGatewayAssociationProposal where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptDirectConnectGatewayAssociationProposal where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptDirectConnectGatewayAssociationProposalResponse' smart constructor.
data AcceptDirectConnectGatewayAssociationProposalResponse = AcceptDirectConnectGatewayAssociationProposalResponse'
  { directConnectGatewayAssociation ::
      Lude.Maybe
        DirectConnectGatewayAssociation,
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

-- | Creates a value of 'AcceptDirectConnectGatewayAssociationProposalResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociation' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkAcceptDirectConnectGatewayAssociationProposalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptDirectConnectGatewayAssociationProposalResponse
mkAcceptDirectConnectGatewayAssociationProposalResponse
  pResponseStatus_ =
    AcceptDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociation =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgaprsDirectConnectGatewayAssociation :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse (Lude.Maybe DirectConnectGatewayAssociation)
adcgaprsDirectConnectGatewayAssociation = Lens.lens (directConnectGatewayAssociation :: AcceptDirectConnectGatewayAssociationProposalResponse -> Lude.Maybe DirectConnectGatewayAssociation) (\s a -> s {directConnectGatewayAssociation = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED adcgaprsDirectConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcgaprsResponseStatus :: Lens.Lens' AcceptDirectConnectGatewayAssociationProposalResponse Lude.Int
adcgaprsResponseStatus = Lens.lens (responseStatus :: AcceptDirectConnectGatewayAssociationProposalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED adcgaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

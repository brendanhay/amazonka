{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a proposal to associate the specified virtual private gateway or transit gateway with the specified Direct Connect gateway.
--
-- You can associate a Direct Connect gateway and virtual private gateway or transit gateway that is owned by any AWS account.
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
  ( -- * Creating a request
    CreateDirectConnectGatewayAssociationProposal (..),
    mkCreateDirectConnectGatewayAssociationProposal,

    -- ** Request lenses
    cdcgapAddAllowedPrefixesToDirectConnectGateway,
    cdcgapRemoveAllowedPrefixesToDirectConnectGateway,
    cdcgapDirectConnectGatewayId,
    cdcgapDirectConnectGatewayOwnerAccount,
    cdcgapGatewayId,

    -- * Destructuring the response
    CreateDirectConnectGatewayAssociationProposalResponse (..),
    mkCreateDirectConnectGatewayAssociationProposalResponse,

    -- ** Response lenses
    cdcgaprsDirectConnectGatewayAssociationProposal,
    cdcgaprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationProposal' smart constructor.
data CreateDirectConnectGatewayAssociationProposal = CreateDirectConnectGatewayAssociationProposal'
  { addAllowedPrefixesToDirectConnectGateway ::
      Lude.Maybe
        [RouteFilterPrefix],
    removeAllowedPrefixesToDirectConnectGateway ::
      Lude.Maybe
        [RouteFilterPrefix],
    directConnectGatewayId ::
      Lude.Text,
    directConnectGatewayOwnerAccount ::
      Lude.Text,
    gatewayId ::
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

-- | Creates a value of 'CreateDirectConnectGatewayAssociationProposal' with the minimum fields required to make a request.
--
-- * 'addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'directConnectGatewayOwnerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
-- * 'gatewayId' - The ID of the virtual private gateway or transit gateway.
-- * 'removeAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
mkCreateDirectConnectGatewayAssociationProposal ::
  -- | 'directConnectGatewayId'
  Lude.Text ->
  -- | 'directConnectGatewayOwnerAccount'
  Lude.Text ->
  -- | 'gatewayId'
  Lude.Text ->
  CreateDirectConnectGatewayAssociationProposal
mkCreateDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pDirectConnectGatewayOwnerAccount_
  pGatewayId_ =
    CreateDirectConnectGatewayAssociationProposal'
      { addAllowedPrefixesToDirectConnectGateway =
          Lude.Nothing,
        removeAllowedPrefixesToDirectConnectGateway =
          Lude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_,
        directConnectGatewayOwnerAccount =
          pDirectConnectGatewayOwnerAccount_,
        gatewayId = pGatewayId_
      }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Lude.Maybe [RouteFilterPrefix])
cdcgapAddAllowedPrefixesToDirectConnectGateway = Lens.lens (addAllowedPrefixesToDirectConnectGateway :: CreateDirectConnectGatewayAssociationProposal -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociationProposal)
{-# DEPRECATED cdcgapAddAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'removeAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapRemoveAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Lude.Maybe [RouteFilterPrefix])
cdcgapRemoveAllowedPrefixesToDirectConnectGateway = Lens.lens (removeAllowedPrefixesToDirectConnectGateway :: CreateDirectConnectGatewayAssociationProposal -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {removeAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociationProposal)
{-# DEPRECATED cdcgapRemoveAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'removeAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapDirectConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Lude.Text
cdcgapDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: CreateDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {directConnectGatewayId = a} :: CreateDirectConnectGatewayAssociationProposal)
{-# DEPRECATED cdcgapDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapDirectConnectGatewayOwnerAccount :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Lude.Text
cdcgapDirectConnectGatewayOwnerAccount = Lens.lens (directConnectGatewayOwnerAccount :: CreateDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {directConnectGatewayOwnerAccount = a} :: CreateDirectConnectGatewayAssociationProposal)
{-# DEPRECATED cdcgapDirectConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead." #-}

-- | The ID of the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgapGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Lude.Text
cdcgapGatewayId = Lens.lens (gatewayId :: CreateDirectConnectGatewayAssociationProposal -> Lude.Text) (\s a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociationProposal)
{-# DEPRECATED cdcgapGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

instance
  Lude.AWSRequest
    CreateDirectConnectGatewayAssociationProposal
  where
  type
    Rs CreateDirectConnectGatewayAssociationProposal =
      CreateDirectConnectGatewayAssociationProposalResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayAssociationProposalResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociationProposal")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    CreateDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.CreateDirectConnectGatewayAssociationProposal" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDirectConnectGatewayAssociationProposal where
  toJSON CreateDirectConnectGatewayAssociationProposal' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("addAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("removeAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> removeAllowedPrefixesToDirectConnectGateway,
            Lude.Just
              ("directConnectGatewayId" Lude..= directConnectGatewayId),
            Lude.Just
              ( "directConnectGatewayOwnerAccount"
                  Lude..= directConnectGatewayOwnerAccount
              ),
            Lude.Just ("gatewayId" Lude..= gatewayId)
          ]
      )

instance Lude.ToPath CreateDirectConnectGatewayAssociationProposal where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDirectConnectGatewayAssociationProposal where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationProposalResponse' smart constructor.
data CreateDirectConnectGatewayAssociationProposalResponse = CreateDirectConnectGatewayAssociationProposalResponse'
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

-- | Creates a value of 'CreateDirectConnectGatewayAssociationProposalResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociationProposal' - Information about the Direct Connect gateway proposal.
-- * 'responseStatus' - The response status code.
mkCreateDirectConnectGatewayAssociationProposalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDirectConnectGatewayAssociationProposalResponse
mkCreateDirectConnectGatewayAssociationProposalResponse
  pResponseStatus_ =
    CreateDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the Direct Connect gateway proposal.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaprsDirectConnectGatewayAssociationProposal :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse (Lude.Maybe DirectConnectGatewayAssociationProposal)
cdcgaprsDirectConnectGatewayAssociationProposal = Lens.lens (directConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposalResponse -> Lude.Maybe DirectConnectGatewayAssociationProposal) (\s a -> s {directConnectGatewayAssociationProposal = a} :: CreateDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED cdcgaprsDirectConnectGatewayAssociationProposal "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposal' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaprsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse Lude.Int
cdcgaprsResponseStatus = Lens.lens (responseStatus :: CreateDirectConnectGatewayAssociationProposalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDirectConnectGatewayAssociationProposalResponse)
{-# DEPRECATED cdcgaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

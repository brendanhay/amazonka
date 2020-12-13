{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateClientVPNTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a target network from the specified Client VPN endpoint. When you disassociate the last target network from a Client VPN, the following happens:
--
--
--     * The route that was automatically added for the VPC is deleted
--
--
--     * All active client connections are terminated
--
--
--     * New client connections are disallowed
--
--
--     * The Client VPN endpoint's status changes to @pending-associate@
module Network.AWS.EC2.DisassociateClientVPNTargetNetwork
  ( -- * Creating a request
    DisassociateClientVPNTargetNetwork (..),
    mkDisassociateClientVPNTargetNetwork,

    -- ** Request lenses
    dcvtnAssociationId,
    dcvtnClientVPNEndpointId,
    dcvtnDryRun,

    -- * Destructuring the response
    DisassociateClientVPNTargetNetworkResponse (..),
    mkDisassociateClientVPNTargetNetworkResponse,

    -- ** Response lenses
    dcvpntnrsAssociationId,
    dcvpntnrsStatus,
    dcvpntnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateClientVPNTargetNetwork' smart constructor.
data DisassociateClientVPNTargetNetwork = DisassociateClientVPNTargetNetwork'
  { -- | The ID of the target network association.
    associationId :: Lude.Text,
    -- | The ID of the Client VPN endpoint from which to disassociate the target network.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the target network association.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint from which to disassociate the target network.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisassociateClientVPNTargetNetwork ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DisassociateClientVPNTargetNetwork
mkDisassociateClientVPNTargetNetwork
  pAssociationId_
  pClientVPNEndpointId_ =
    DisassociateClientVPNTargetNetwork'
      { associationId =
          pAssociationId_,
        clientVPNEndpointId = pClientVPNEndpointId_,
        dryRun = Lude.Nothing
      }

-- | The ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnAssociationId :: Lens.Lens' DisassociateClientVPNTargetNetwork Lude.Text
dcvtnAssociationId = Lens.lens (associationId :: DisassociateClientVPNTargetNetwork -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateClientVPNTargetNetwork)
{-# DEPRECATED dcvtnAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the Client VPN endpoint from which to disassociate the target network.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnClientVPNEndpointId :: Lens.Lens' DisassociateClientVPNTargetNetwork Lude.Text
dcvtnClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DisassociateClientVPNTargetNetwork -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DisassociateClientVPNTargetNetwork)
{-# DEPRECATED dcvtnClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnDryRun :: Lens.Lens' DisassociateClientVPNTargetNetwork (Lude.Maybe Lude.Bool)
dcvtnDryRun = Lens.lens (dryRun :: DisassociateClientVPNTargetNetwork -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateClientVPNTargetNetwork)
{-# DEPRECATED dcvtnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisassociateClientVPNTargetNetwork where
  type
    Rs DisassociateClientVPNTargetNetwork =
      DisassociateClientVPNTargetNetworkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateClientVPNTargetNetworkResponse'
            Lude.<$> (x Lude..@? "associationId")
            Lude.<*> (x Lude..@? "status")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateClientVPNTargetNetwork where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateClientVPNTargetNetwork where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateClientVPNTargetNetwork where
  toQuery DisassociateClientVPNTargetNetwork' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateClientVpnTargetNetwork" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisassociateClientVPNTargetNetworkResponse' smart constructor.
data DisassociateClientVPNTargetNetworkResponse = DisassociateClientVPNTargetNetworkResponse'
  { -- | The ID of the target network association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The current state of the target network association.
    status :: Lude.Maybe AssociationStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the target network association.
-- * 'status' - The current state of the target network association.
-- * 'responseStatus' - The response status code.
mkDisassociateClientVPNTargetNetworkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateClientVPNTargetNetworkResponse
mkDisassociateClientVPNTargetNetworkResponse pResponseStatus_ =
  DisassociateClientVPNTargetNetworkResponse'
    { associationId =
        Lude.Nothing,
      status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpntnrsAssociationId :: Lens.Lens' DisassociateClientVPNTargetNetworkResponse (Lude.Maybe Lude.Text)
dcvpntnrsAssociationId = Lens.lens (associationId :: DisassociateClientVPNTargetNetworkResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DisassociateClientVPNTargetNetworkResponse)
{-# DEPRECATED dcvpntnrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpntnrsStatus :: Lens.Lens' DisassociateClientVPNTargetNetworkResponse (Lude.Maybe AssociationStatus)
dcvpntnrsStatus = Lens.lens (status :: DisassociateClientVPNTargetNetworkResponse -> Lude.Maybe AssociationStatus) (\s a -> s {status = a} :: DisassociateClientVPNTargetNetworkResponse)
{-# DEPRECATED dcvpntnrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpntnrsResponseStatus :: Lens.Lens' DisassociateClientVPNTargetNetworkResponse Lude.Int
dcvpntnrsResponseStatus = Lens.lens (responseStatus :: DisassociateClientVPNTargetNetworkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateClientVPNTargetNetworkResponse)
{-# DEPRECATED dcvpntnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

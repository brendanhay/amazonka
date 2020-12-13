{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateClientVPNTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a target network with a Client VPN endpoint. A target network is a subnet in a VPC. You can associate multiple subnets from the same VPC with a Client VPN endpoint. You can associate only one subnet in each Availability Zone. We recommend that you associate at least two subnets to provide Availability Zone redundancy.
--
-- If you specified a VPC when you created the Client VPN endpoint or if you have previous subnet associations, the specified subnet must be in the same VPC. To specify a subnet that's in a different VPC, you must first modify the Client VPN endpoint ('ModifyClientVpnEndpoint' ) and change the VPC that's associated with it.
module Network.AWS.EC2.AssociateClientVPNTargetNetwork
  ( -- * Creating a request
    AssociateClientVPNTargetNetwork (..),
    mkAssociateClientVPNTargetNetwork,

    -- ** Request lenses
    acvtnClientToken,
    acvtnSubnetId,
    acvtnClientVPNEndpointId,
    acvtnDryRun,

    -- * Destructuring the response
    AssociateClientVPNTargetNetworkResponse (..),
    mkAssociateClientVPNTargetNetworkResponse,

    -- ** Response lenses
    acvtnrsAssociationId,
    acvtnrsStatus,
    acvtnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateClientVPNTargetNetwork' smart constructor.
data AssociateClientVPNTargetNetwork = AssociateClientVPNTargetNetwork'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet to associate with the Client VPN endpoint.
    subnetId :: Lude.Text,
    -- | The ID of the Client VPN endpoint.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'subnetId' - The ID of the subnet to associate with the Client VPN endpoint.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAssociateClientVPNTargetNetwork ::
  -- | 'subnetId'
  Lude.Text ->
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  AssociateClientVPNTargetNetwork
mkAssociateClientVPNTargetNetwork pSubnetId_ pClientVPNEndpointId_ =
  AssociateClientVPNTargetNetwork'
    { clientToken = Lude.Nothing,
      subnetId = pSubnetId_,
      clientVPNEndpointId = pClientVPNEndpointId_,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnClientToken :: Lens.Lens' AssociateClientVPNTargetNetwork (Lude.Maybe Lude.Text)
acvtnClientToken = Lens.lens (clientToken :: AssociateClientVPNTargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: AssociateClientVPNTargetNetwork)
{-# DEPRECATED acvtnClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the subnet to associate with the Client VPN endpoint.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnSubnetId :: Lens.Lens' AssociateClientVPNTargetNetwork Lude.Text
acvtnSubnetId = Lens.lens (subnetId :: AssociateClientVPNTargetNetwork -> Lude.Text) (\s a -> s {subnetId = a} :: AssociateClientVPNTargetNetwork)
{-# DEPRECATED acvtnSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnClientVPNEndpointId :: Lens.Lens' AssociateClientVPNTargetNetwork Lude.Text
acvtnClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: AssociateClientVPNTargetNetwork -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: AssociateClientVPNTargetNetwork)
{-# DEPRECATED acvtnClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnDryRun :: Lens.Lens' AssociateClientVPNTargetNetwork (Lude.Maybe Lude.Bool)
acvtnDryRun = Lens.lens (dryRun :: AssociateClientVPNTargetNetwork -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateClientVPNTargetNetwork)
{-# DEPRECATED acvtnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateClientVPNTargetNetwork where
  type
    Rs AssociateClientVPNTargetNetwork =
      AssociateClientVPNTargetNetworkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateClientVPNTargetNetworkResponse'
            Lude.<$> (x Lude..@? "associationId")
            Lude.<*> (x Lude..@? "status")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateClientVPNTargetNetwork where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateClientVPNTargetNetwork where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateClientVPNTargetNetwork where
  toQuery AssociateClientVPNTargetNetwork' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateClientVpnTargetNetwork" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "SubnetId" Lude.=: subnetId,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAssociateClientVPNTargetNetworkResponse' smart constructor.
data AssociateClientVPNTargetNetworkResponse = AssociateClientVPNTargetNetworkResponse'
  { -- | The unique ID of the target network association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The current state of the target network association.
    status :: Lude.Maybe AssociationStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - The unique ID of the target network association.
-- * 'status' - The current state of the target network association.
-- * 'responseStatus' - The response status code.
mkAssociateClientVPNTargetNetworkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateClientVPNTargetNetworkResponse
mkAssociateClientVPNTargetNetworkResponse pResponseStatus_ =
  AssociateClientVPNTargetNetworkResponse'
    { associationId =
        Lude.Nothing,
      status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrsAssociationId :: Lens.Lens' AssociateClientVPNTargetNetworkResponse (Lude.Maybe Lude.Text)
acvtnrsAssociationId = Lens.lens (associationId :: AssociateClientVPNTargetNetworkResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociateClientVPNTargetNetworkResponse)
{-# DEPRECATED acvtnrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrsStatus :: Lens.Lens' AssociateClientVPNTargetNetworkResponse (Lude.Maybe AssociationStatus)
acvtnrsStatus = Lens.lens (status :: AssociateClientVPNTargetNetworkResponse -> Lude.Maybe AssociationStatus) (\s a -> s {status = a} :: AssociateClientVPNTargetNetworkResponse)
{-# DEPRECATED acvtnrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrsResponseStatus :: Lens.Lens' AssociateClientVPNTargetNetworkResponse Lude.Int
acvtnrsResponseStatus = Lens.lens (responseStatus :: AssociateClientVPNTargetNetworkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateClientVPNTargetNetworkResponse)
{-# DEPRECATED acvtnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

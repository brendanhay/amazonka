{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a VPC peering connection between a virtual private cloud (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. You can peer with VPCs in any AWS account that you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different Regions. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- Before calling this operation to establish the peering connection, you first need to call 'CreateVpcPeeringAuthorization' and identify the VPC you want to peer with. Once the authorization for the specified VPC is issued, you have 24 hours to establish the connection. These two operations handle all tasks necessary to peer the two VPCs, including acceptance, updating routing tables, etc.
-- To establish the connection, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the following values: (1) The ID of the fleet you want to be enable a VPC peering connection for; (2) The AWS account with the VPC that you want to peer with; and (3) The ID of the VPC you want to peer with. This operation is asynchronous. If successful, a 'VpcPeeringConnection' request is created. You can use continuous polling to track the request's status using 'DescribeVpcPeeringConnections' , or by monitoring fleet events for success or failure using 'DescribeFleetEvents' .
--
--     * 'CreateVpcPeeringAuthorization'
--
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--
--     * 'DeleteVpcPeeringAuthorization'
--
--
--     * 'CreateVpcPeeringConnection'
--
--
--     * 'DescribeVpcPeeringConnections'
--
--
--     * 'DeleteVpcPeeringConnection'
module Network.AWS.GameLift.CreateVPCPeeringConnection
  ( -- * Creating a request
    CreateVPCPeeringConnection (..),
    mkCreateVPCPeeringConnection,

    -- ** Request lenses
    cvpcFleetId,
    cvpcPeerVPCAWSAccountId,
    cvpcPeerVPCId,

    -- * Destructuring the response
    CreateVPCPeeringConnectionResponse (..),
    mkCreateVPCPeeringConnectionResponse,

    -- ** Response lenses
    cvpcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateVPCPeeringConnection' smart constructor.
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
  { fleetId ::
      Lude.Text,
    peerVPCAWSAccountId :: Lude.Text,
    peerVPCId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with.
-- * 'peerVPCAWSAccountId' - A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
mkCreateVPCPeeringConnection ::
  -- | 'fleetId'
  Lude.Text ->
  -- | 'peerVPCAWSAccountId'
  Lude.Text ->
  -- | 'peerVPCId'
  Lude.Text ->
  CreateVPCPeeringConnection
mkCreateVPCPeeringConnection
  pFleetId_
  pPeerVPCAWSAccountId_
  pPeerVPCId_ =
    CreateVPCPeeringConnection'
      { fleetId = pFleetId_,
        peerVPCAWSAccountId = pPeerVPCAWSAccountId_,
        peerVPCId = pPeerVPCId_
      }

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcFleetId :: Lens.Lens' CreateVPCPeeringConnection Lude.Text
cvpcFleetId = Lens.lens (fleetId :: CreateVPCPeeringConnection -> Lude.Text) (\s a -> s {fleetId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'peerVPCAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVPCAWSAccountId :: Lens.Lens' CreateVPCPeeringConnection Lude.Text
cvpcPeerVPCAWSAccountId = Lens.lens (peerVPCAWSAccountId :: CreateVPCPeeringConnection -> Lude.Text) (\s a -> s {peerVPCAWSAccountId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcPeerVPCAWSAccountId "Use generic-lens or generic-optics with 'peerVPCAWSAccountId' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVPCId :: Lens.Lens' CreateVPCPeeringConnection Lude.Text
cvpcPeerVPCId = Lens.lens (peerVPCId :: CreateVPCPeeringConnection -> Lude.Text) (\s a -> s {peerVPCId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

instance Lude.AWSRequest CreateVPCPeeringConnection where
  type
    Rs CreateVPCPeeringConnection =
      CreateVPCPeeringConnectionResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateVPCPeeringConnectionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCPeeringConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateVpcPeeringConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVPCPeeringConnection where
  toJSON CreateVPCPeeringConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FleetId" Lude..= fleetId),
            Lude.Just ("PeerVpcAwsAccountId" Lude..= peerVPCAWSAccountId),
            Lude.Just ("PeerVpcId" Lude..= peerVPCId)
          ]
      )

instance Lude.ToPath CreateVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCPeeringConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateVPCPeeringConnectionResponse' smart constructor.
newtype CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCPeeringConnectionResponse
mkCreateVPCPeeringConnectionResponse pResponseStatus_ =
  CreateVPCPeeringConnectionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrsResponseStatus :: Lens.Lens' CreateVPCPeeringConnectionResponse Lude.Int
cvpcrsResponseStatus = Lens.lens (responseStatus :: CreateVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCPeeringConnectionResponse)
{-# DEPRECATED cvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

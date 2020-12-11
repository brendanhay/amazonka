{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateVPCPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests authorization to create or delete a peer connection between the VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in your AWS account. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. Once you've received authorization, call 'CreateVpcPeeringConnection' to establish the peering connection. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- You can peer with VPCs that are owned by any AWS account you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different Regions.
-- To request authorization to create a connection, call this operation from the AWS account with the VPC that you want to peer to your Amazon GameLift fleet. For example, to enable your game servers to retrieve data from a DynamoDB table, use the account that manages that DynamoDB resource. Identify the following values: (1) The ID of the VPC that you want to peer with, and (2) the ID of the AWS account that you use to manage Amazon GameLift. If successful, VPC peering is authorized for the specified VPC.
-- To request authorization to delete a connection, call this operation from the AWS account with the VPC that is peered with your Amazon GameLift fleet. Identify the following values: (1) VPC ID that you want to delete the peering connection for, and (2) ID of the AWS account that you use to manage Amazon GameLift.
-- The authorization remains valid for 24 hours unless it is canceled by a call to 'DeleteVpcPeeringAuthorization' . You must create or delete the peering connection while the authorization is valid.
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
module Network.AWS.GameLift.CreateVPCPeeringAuthorization
  ( -- * Creating a request
    CreateVPCPeeringAuthorization (..),
    mkCreateVPCPeeringAuthorization,

    -- ** Request lenses
    cvpaGameLiftAWSAccountId,
    cvpaPeerVPCId,

    -- * Destructuring the response
    CreateVPCPeeringAuthorizationResponse (..),
    mkCreateVPCPeeringAuthorizationResponse,

    -- ** Response lenses
    cvparsVPCPeeringAuthorization,
    cvparsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateVPCPeeringAuthorization' smart constructor.
data CreateVPCPeeringAuthorization = CreateVPCPeeringAuthorization'
  { gameLiftAWSAccountId ::
      Lude.Text,
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

-- | Creates a value of 'CreateVPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- * 'gameLiftAWSAccountId' - A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
mkCreateVPCPeeringAuthorization ::
  -- | 'gameLiftAWSAccountId'
  Lude.Text ->
  -- | 'peerVPCId'
  Lude.Text ->
  CreateVPCPeeringAuthorization
mkCreateVPCPeeringAuthorization pGameLiftAWSAccountId_ pPeerVPCId_ =
  CreateVPCPeeringAuthorization'
    { gameLiftAWSAccountId =
        pGameLiftAWSAccountId_,
      peerVPCId = pPeerVPCId_
    }

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpaGameLiftAWSAccountId :: Lens.Lens' CreateVPCPeeringAuthorization Lude.Text
cvpaGameLiftAWSAccountId = Lens.lens (gameLiftAWSAccountId :: CreateVPCPeeringAuthorization -> Lude.Text) (\s a -> s {gameLiftAWSAccountId = a} :: CreateVPCPeeringAuthorization)
{-# DEPRECATED cvpaGameLiftAWSAccountId "Use generic-lens or generic-optics with 'gameLiftAWSAccountId' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpaPeerVPCId :: Lens.Lens' CreateVPCPeeringAuthorization Lude.Text
cvpaPeerVPCId = Lens.lens (peerVPCId :: CreateVPCPeeringAuthorization -> Lude.Text) (\s a -> s {peerVPCId = a} :: CreateVPCPeeringAuthorization)
{-# DEPRECATED cvpaPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

instance Lude.AWSRequest CreateVPCPeeringAuthorization where
  type
    Rs CreateVPCPeeringAuthorization =
      CreateVPCPeeringAuthorizationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateVPCPeeringAuthorizationResponse'
            Lude.<$> (x Lude..?> "VpcPeeringAuthorization")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCPeeringAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateVpcPeeringAuthorization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVPCPeeringAuthorization where
  toJSON CreateVPCPeeringAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameLiftAwsAccountId" Lude..= gameLiftAWSAccountId),
            Lude.Just ("PeerVpcId" Lude..= peerVPCId)
          ]
      )

instance Lude.ToPath CreateVPCPeeringAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCPeeringAuthorization where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateVPCPeeringAuthorizationResponse' smart constructor.
data CreateVPCPeeringAuthorizationResponse = CreateVPCPeeringAuthorizationResponse'
  { vpcPeeringAuthorization ::
      Lude.Maybe
        VPCPeeringAuthorization,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCPeeringAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpcPeeringAuthorization' - Details on the requested VPC peering authorization, including expiration.
mkCreateVPCPeeringAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCPeeringAuthorizationResponse
mkCreateVPCPeeringAuthorizationResponse pResponseStatus_ =
  CreateVPCPeeringAuthorizationResponse'
    { vpcPeeringAuthorization =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Details on the requested VPC peering authorization, including expiration.
--
-- /Note:/ Consider using 'vpcPeeringAuthorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvparsVPCPeeringAuthorization :: Lens.Lens' CreateVPCPeeringAuthorizationResponse (Lude.Maybe VPCPeeringAuthorization)
cvparsVPCPeeringAuthorization = Lens.lens (vpcPeeringAuthorization :: CreateVPCPeeringAuthorizationResponse -> Lude.Maybe VPCPeeringAuthorization) (\s a -> s {vpcPeeringAuthorization = a} :: CreateVPCPeeringAuthorizationResponse)
{-# DEPRECATED cvparsVPCPeeringAuthorization "Use generic-lens or generic-optics with 'vpcPeeringAuthorization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvparsResponseStatus :: Lens.Lens' CreateVPCPeeringAuthorizationResponse Lude.Int
cvparsResponseStatus = Lens.lens (responseStatus :: CreateVPCPeeringAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCPeeringAuthorizationResponse)
{-# DEPRECATED cvparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

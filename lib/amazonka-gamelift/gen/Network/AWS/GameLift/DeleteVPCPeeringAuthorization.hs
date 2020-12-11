{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteVPCPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending VPC peering authorization for the specified VPC. If you need to delete an existing VPC peering connection, call 'DeleteVpcPeeringConnection' .
--
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
module Network.AWS.GameLift.DeleteVPCPeeringAuthorization
  ( -- * Creating a request
    DeleteVPCPeeringAuthorization (..),
    mkDeleteVPCPeeringAuthorization,

    -- ** Request lenses
    dvpaGameLiftAWSAccountId,
    dvpaPeerVPCId,

    -- * Destructuring the response
    DeleteVPCPeeringAuthorizationResponse (..),
    mkDeleteVPCPeeringAuthorizationResponse,

    -- ** Response lenses
    dvparsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteVPCPeeringAuthorization' smart constructor.
data DeleteVPCPeeringAuthorization = DeleteVPCPeeringAuthorization'
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

-- | Creates a value of 'DeleteVPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- * 'gameLiftAWSAccountId' - A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
mkDeleteVPCPeeringAuthorization ::
  -- | 'gameLiftAWSAccountId'
  Lude.Text ->
  -- | 'peerVPCId'
  Lude.Text ->
  DeleteVPCPeeringAuthorization
mkDeleteVPCPeeringAuthorization pGameLiftAWSAccountId_ pPeerVPCId_ =
  DeleteVPCPeeringAuthorization'
    { gameLiftAWSAccountId =
        pGameLiftAWSAccountId_,
      peerVPCId = pPeerVPCId_
    }

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpaGameLiftAWSAccountId :: Lens.Lens' DeleteVPCPeeringAuthorization Lude.Text
dvpaGameLiftAWSAccountId = Lens.lens (gameLiftAWSAccountId :: DeleteVPCPeeringAuthorization -> Lude.Text) (\s a -> s {gameLiftAWSAccountId = a} :: DeleteVPCPeeringAuthorization)
{-# DEPRECATED dvpaGameLiftAWSAccountId "Use generic-lens or generic-optics with 'gameLiftAWSAccountId' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpaPeerVPCId :: Lens.Lens' DeleteVPCPeeringAuthorization Lude.Text
dvpaPeerVPCId = Lens.lens (peerVPCId :: DeleteVPCPeeringAuthorization -> Lude.Text) (\s a -> s {peerVPCId = a} :: DeleteVPCPeeringAuthorization)
{-# DEPRECATED dvpaPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

instance Lude.AWSRequest DeleteVPCPeeringAuthorization where
  type
    Rs DeleteVPCPeeringAuthorization =
      DeleteVPCPeeringAuthorizationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteVPCPeeringAuthorizationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCPeeringAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteVpcPeeringAuthorization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVPCPeeringAuthorization where
  toJSON DeleteVPCPeeringAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameLiftAwsAccountId" Lude..= gameLiftAWSAccountId),
            Lude.Just ("PeerVpcId" Lude..= peerVPCId)
          ]
      )

instance Lude.ToPath DeleteVPCPeeringAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCPeeringAuthorization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVPCPeeringAuthorizationResponse' smart constructor.
newtype DeleteVPCPeeringAuthorizationResponse = DeleteVPCPeeringAuthorizationResponse'
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

-- | Creates a value of 'DeleteVPCPeeringAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteVPCPeeringAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCPeeringAuthorizationResponse
mkDeleteVPCPeeringAuthorizationResponse pResponseStatus_ =
  DeleteVPCPeeringAuthorizationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvparsResponseStatus :: Lens.Lens' DeleteVPCPeeringAuthorizationResponse Lude.Int
dvparsResponseStatus = Lens.lens (responseStatus :: DeleteVPCPeeringAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCPeeringAuthorizationResponse)
{-# DEPRECATED dvparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

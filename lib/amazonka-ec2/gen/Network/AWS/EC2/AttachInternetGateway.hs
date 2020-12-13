{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an internet gateway or a virtual private gateway to a VPC, enabling connectivity between the internet and the VPC. For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.AttachInternetGateway
  ( -- * Creating a request
    AttachInternetGateway (..),
    mkAttachInternetGateway,

    -- ** Request lenses
    aigVPCId,
    aigDryRun,
    aigInternetGatewayId,

    -- * Destructuring the response
    AttachInternetGatewayResponse (..),
    mkAttachInternetGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachInternetGateway' smart constructor.
data AttachInternetGateway = AttachInternetGateway'
  { -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The ID of the internet gateway.
    internetGatewayId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachInternetGateway' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'internetGatewayId' - The ID of the internet gateway.
mkAttachInternetGateway ::
  -- | 'vpcId'
  Lude.Text ->
  -- | 'internetGatewayId'
  Lude.Text ->
  AttachInternetGateway
mkAttachInternetGateway pVPCId_ pInternetGatewayId_ =
  AttachInternetGateway'
    { vpcId = pVPCId_,
      dryRun = Lude.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigVPCId :: Lens.Lens' AttachInternetGateway Lude.Text
aigVPCId = Lens.lens (vpcId :: AttachInternetGateway -> Lude.Text) (\s a -> s {vpcId = a} :: AttachInternetGateway)
{-# DEPRECATED aigVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigDryRun :: Lens.Lens' AttachInternetGateway (Lude.Maybe Lude.Bool)
aigDryRun = Lens.lens (dryRun :: AttachInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AttachInternetGateway)
{-# DEPRECATED aigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigInternetGatewayId :: Lens.Lens' AttachInternetGateway Lude.Text
aigInternetGatewayId = Lens.lens (internetGatewayId :: AttachInternetGateway -> Lude.Text) (\s a -> s {internetGatewayId = a} :: AttachInternetGateway)
{-# DEPRECATED aigInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

instance Lude.AWSRequest AttachInternetGateway where
  type Rs AttachInternetGateway = AttachInternetGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull AttachInternetGatewayResponse'

instance Lude.ToHeaders AttachInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachInternetGateway where
  toQuery AttachInternetGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId,
        "DryRun" Lude.=: dryRun,
        "InternetGatewayId" Lude.=: internetGatewayId
      ]

-- | /See:/ 'mkAttachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse = AttachInternetGatewayResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachInternetGatewayResponse' with the minimum fields required to make a request.
mkAttachInternetGatewayResponse ::
  AttachInternetGatewayResponse
mkAttachInternetGatewayResponse = AttachInternetGatewayResponse'

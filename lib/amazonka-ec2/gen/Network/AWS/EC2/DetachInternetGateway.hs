{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an internet gateway from a VPC, disabling connectivity between the internet and the VPC. The VPC must not contain any running instances with Elastic IP addresses or public IPv4 addresses.
module Network.AWS.EC2.DetachInternetGateway
  ( -- * Creating a request
    DetachInternetGateway (..),
    mkDetachInternetGateway,

    -- ** Request lenses
    digDryRun,
    digInternetGatewayId,
    digVPCId,

    -- * Destructuring the response
    DetachInternetGatewayResponse (..),
    mkDetachInternetGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachInternetGateway' smart constructor.
data DetachInternetGateway = DetachInternetGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    internetGatewayId :: Lude.Text,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInternetGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'internetGatewayId' - The ID of the internet gateway.
-- * 'vpcId' - The ID of the VPC.
mkDetachInternetGateway ::
  -- | 'internetGatewayId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  DetachInternetGateway
mkDetachInternetGateway pInternetGatewayId_ pVPCId_ =
  DetachInternetGateway'
    { dryRun = Lude.Nothing,
      internetGatewayId = pInternetGatewayId_,
      vpcId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digDryRun :: Lens.Lens' DetachInternetGateway (Lude.Maybe Lude.Bool)
digDryRun = Lens.lens (dryRun :: DetachInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DetachInternetGateway)
{-# DEPRECATED digDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digInternetGatewayId :: Lens.Lens' DetachInternetGateway Lude.Text
digInternetGatewayId = Lens.lens (internetGatewayId :: DetachInternetGateway -> Lude.Text) (\s a -> s {internetGatewayId = a} :: DetachInternetGateway)
{-# DEPRECATED digInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digVPCId :: Lens.Lens' DetachInternetGateway Lude.Text
digVPCId = Lens.lens (vpcId :: DetachInternetGateway -> Lude.Text) (\s a -> s {vpcId = a} :: DetachInternetGateway)
{-# DEPRECATED digVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest DetachInternetGateway where
  type Rs DetachInternetGateway = DetachInternetGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DetachInternetGatewayResponse'

instance Lude.ToHeaders DetachInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachInternetGateway where
  toQuery DetachInternetGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InternetGatewayId" Lude.=: internetGatewayId,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkDetachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse = DetachInternetGatewayResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInternetGatewayResponse' with the minimum fields required to make a request.
mkDetachInternetGatewayResponse ::
  DetachInternetGatewayResponse
mkDetachInternetGatewayResponse = DetachInternetGatewayResponse'

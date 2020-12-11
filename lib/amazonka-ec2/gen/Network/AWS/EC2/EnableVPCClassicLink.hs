{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVPCClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC for ClassicLink. You can then link EC2-Classic instances to your ClassicLink-enabled VPC to allow communication over private IP addresses. You cannot enable your VPC for ClassicLink if any of your VPC route tables have existing routes for address ranges within the @10.0.0.0/8@ IP address range, excluding local routes for VPCs in the @10.0.0.0/16@ and @10.1.0.0/16@ IP address ranges. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.EnableVPCClassicLink
  ( -- * Creating a request
    EnableVPCClassicLink (..),
    mkEnableVPCClassicLink,

    -- ** Request lenses
    evclDryRun,
    evclVPCId,

    -- * Destructuring the response
    EnableVPCClassicLinkResponse (..),
    mkEnableVPCClassicLinkResponse,

    -- ** Response lenses
    evclrsReturn,
    evclrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableVPCClassicLink' smart constructor.
data EnableVPCClassicLink = EnableVPCClassicLink'
  { dryRun ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'EnableVPCClassicLink' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcId' - The ID of the VPC.
mkEnableVPCClassicLink ::
  -- | 'vpcId'
  Lude.Text ->
  EnableVPCClassicLink
mkEnableVPCClassicLink pVPCId_ =
  EnableVPCClassicLink' {dryRun = Lude.Nothing, vpcId = pVPCId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclDryRun :: Lens.Lens' EnableVPCClassicLink (Lude.Maybe Lude.Bool)
evclDryRun = Lens.lens (dryRun :: EnableVPCClassicLink -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableVPCClassicLink)
{-# DEPRECATED evclDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclVPCId :: Lens.Lens' EnableVPCClassicLink Lude.Text
evclVPCId = Lens.lens (vpcId :: EnableVPCClassicLink -> Lude.Text) (\s a -> s {vpcId = a} :: EnableVPCClassicLink)
{-# DEPRECATED evclVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest EnableVPCClassicLink where
  type Rs EnableVPCClassicLink = EnableVPCClassicLinkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          EnableVPCClassicLinkResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableVPCClassicLink where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableVPCClassicLink where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableVPCClassicLink where
  toQuery EnableVPCClassicLink' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableVpcClassicLink" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkEnableVPCClassicLinkResponse' smart constructor.
data EnableVPCClassicLinkResponse = EnableVPCClassicLinkResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableVPCClassicLinkResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkEnableVPCClassicLinkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableVPCClassicLinkResponse
mkEnableVPCClassicLinkResponse pResponseStatus_ =
  EnableVPCClassicLinkResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclrsReturn :: Lens.Lens' EnableVPCClassicLinkResponse (Lude.Maybe Lude.Bool)
evclrsReturn = Lens.lens (return :: EnableVPCClassicLinkResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: EnableVPCClassicLinkResponse)
{-# DEPRECATED evclrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclrsResponseStatus :: Lens.Lens' EnableVPCClassicLinkResponse Lude.Int
evclrsResponseStatus = Lens.lens (responseStatus :: EnableVPCClassicLinkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableVPCClassicLinkResponse)
{-# DEPRECATED evclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

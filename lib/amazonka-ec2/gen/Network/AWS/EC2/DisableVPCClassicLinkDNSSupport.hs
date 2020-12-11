{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVPCClassicLinkDNSSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames resolve to public IP addresses when addressed between a linked EC2-Classic instance and instances in the VPC to which it's linked. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.DisableVPCClassicLinkDNSSupport
  ( -- * Creating a request
    DisableVPCClassicLinkDNSSupport (..),
    mkDisableVPCClassicLinkDNSSupport,

    -- ** Request lenses
    dvcldsVPCId,

    -- * Destructuring the response
    DisableVPCClassicLinkDNSSupportResponse (..),
    mkDisableVPCClassicLinkDNSSupportResponse,

    -- ** Response lenses
    dvcldsrsReturn,
    dvcldsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableVPCClassicLinkDNSSupport' smart constructor.
newtype DisableVPCClassicLinkDNSSupport = DisableVPCClassicLinkDNSSupport'
  { vpcId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableVPCClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
mkDisableVPCClassicLinkDNSSupport ::
  DisableVPCClassicLinkDNSSupport
mkDisableVPCClassicLinkDNSSupport =
  DisableVPCClassicLinkDNSSupport' {vpcId = Lude.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsVPCId :: Lens.Lens' DisableVPCClassicLinkDNSSupport (Lude.Maybe Lude.Text)
dvcldsVPCId = Lens.lens (vpcId :: DisableVPCClassicLinkDNSSupport -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DisableVPCClassicLinkDNSSupport)
{-# DEPRECATED dvcldsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest DisableVPCClassicLinkDNSSupport where
  type
    Rs DisableVPCClassicLinkDNSSupport =
      DisableVPCClassicLinkDNSSupportResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisableVPCClassicLinkDNSSupportResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableVPCClassicLinkDNSSupport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableVPCClassicLinkDNSSupport where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableVPCClassicLinkDNSSupport where
  toQuery DisableVPCClassicLinkDNSSupport' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableVpcClassicLinkDnsSupport" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkDisableVPCClassicLinkDNSSupportResponse' smart constructor.
data DisableVPCClassicLinkDNSSupportResponse = DisableVPCClassicLinkDNSSupportResponse'
  { return ::
      Lude.Maybe
        Lude.Bool,
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

-- | Creates a value of 'DisableVPCClassicLinkDNSSupportResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkDisableVPCClassicLinkDNSSupportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableVPCClassicLinkDNSSupportResponse
mkDisableVPCClassicLinkDNSSupportResponse pResponseStatus_ =
  DisableVPCClassicLinkDNSSupportResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrsReturn :: Lens.Lens' DisableVPCClassicLinkDNSSupportResponse (Lude.Maybe Lude.Bool)
dvcldsrsReturn = Lens.lens (return :: DisableVPCClassicLinkDNSSupportResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DisableVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED dvcldsrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrsResponseStatus :: Lens.Lens' DisableVPCClassicLinkDNSSupportResponse Lude.Int
dvcldsrsResponseStatus = Lens.lens (responseStatus :: DisableVPCClassicLinkDNSSupportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED dvcldsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

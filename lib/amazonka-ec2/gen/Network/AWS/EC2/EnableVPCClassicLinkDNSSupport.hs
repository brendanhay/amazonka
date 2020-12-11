{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVPCClassicLinkDNSSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC to support DNS hostname resolution for ClassicLink. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.EnableVPCClassicLinkDNSSupport
  ( -- * Creating a request
    EnableVPCClassicLinkDNSSupport (..),
    mkEnableVPCClassicLinkDNSSupport,

    -- ** Request lenses
    evcldsVPCId,

    -- * Destructuring the response
    EnableVPCClassicLinkDNSSupportResponse (..),
    mkEnableVPCClassicLinkDNSSupportResponse,

    -- ** Response lenses
    evcldsrsReturn,
    evcldsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableVPCClassicLinkDNSSupport' smart constructor.
newtype EnableVPCClassicLinkDNSSupport = EnableVPCClassicLinkDNSSupport'
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

-- | Creates a value of 'EnableVPCClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
mkEnableVPCClassicLinkDNSSupport ::
  EnableVPCClassicLinkDNSSupport
mkEnableVPCClassicLinkDNSSupport =
  EnableVPCClassicLinkDNSSupport' {vpcId = Lude.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsVPCId :: Lens.Lens' EnableVPCClassicLinkDNSSupport (Lude.Maybe Lude.Text)
evcldsVPCId = Lens.lens (vpcId :: EnableVPCClassicLinkDNSSupport -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: EnableVPCClassicLinkDNSSupport)
{-# DEPRECATED evcldsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest EnableVPCClassicLinkDNSSupport where
  type
    Rs EnableVPCClassicLinkDNSSupport =
      EnableVPCClassicLinkDNSSupportResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          EnableVPCClassicLinkDNSSupportResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableVPCClassicLinkDNSSupport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableVPCClassicLinkDNSSupport where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableVPCClassicLinkDNSSupport where
  toQuery EnableVPCClassicLinkDNSSupport' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("EnableVpcClassicLinkDnsSupport" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkEnableVPCClassicLinkDNSSupportResponse' smart constructor.
data EnableVPCClassicLinkDNSSupportResponse = EnableVPCClassicLinkDNSSupportResponse'
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

-- | Creates a value of 'EnableVPCClassicLinkDNSSupportResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkEnableVPCClassicLinkDNSSupportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableVPCClassicLinkDNSSupportResponse
mkEnableVPCClassicLinkDNSSupportResponse pResponseStatus_ =
  EnableVPCClassicLinkDNSSupportResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsrsReturn :: Lens.Lens' EnableVPCClassicLinkDNSSupportResponse (Lude.Maybe Lude.Bool)
evcldsrsReturn = Lens.lens (return :: EnableVPCClassicLinkDNSSupportResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: EnableVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED evcldsrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsrsResponseStatus :: Lens.Lens' EnableVPCClassicLinkDNSSupportResponse Lude.Int
evcldsrsResponseStatus = Lens.lens (responseStatus :: EnableVPCClassicLinkDNSSupportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED evcldsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

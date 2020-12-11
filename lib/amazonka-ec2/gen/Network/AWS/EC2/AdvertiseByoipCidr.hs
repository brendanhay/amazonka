{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AdvertiseByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises an IPv4 or IPv6 address range that is provisioned for use with your AWS resources through bring your own IP addresses (BYOIP).
--
-- You can perform this operation at most once every 10 seconds, even if you specify different address ranges each time.
-- We recommend that you stop advertising the BYOIP CIDR from other locations when you advertise it from AWS. To minimize down time, you can configure your AWS resources to use an address from a BYOIP CIDR before it is advertised, and then simultaneously stop advertising it from the current location and start advertising it through AWS.
-- It can take a few minutes before traffic to the specified addresses starts routing to AWS because of BGP propagation delays.
-- To stop advertising the BYOIP CIDR, use 'WithdrawByoipCidr' .
module Network.AWS.EC2.AdvertiseByoipCidr
  ( -- * Creating a request
    AdvertiseByoipCidr (..),
    mkAdvertiseByoipCidr,

    -- ** Request lenses
    abcDryRun,
    abcCidr,

    -- * Destructuring the response
    AdvertiseByoipCidrResponse (..),
    mkAdvertiseByoipCidrResponse,

    -- ** Response lenses
    abcrsByoipCidr,
    abcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdvertiseByoipCidr' smart constructor.
data AdvertiseByoipCidr = AdvertiseByoipCidr'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    cidr :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvertiseByoipCidr' with the minimum fields required to make a request.
--
-- * 'cidr' - The address range, in CIDR notation. This must be the exact range that you provisioned. You can't advertise only a portion of the provisioned range.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAdvertiseByoipCidr ::
  -- | 'cidr'
  Lude.Text ->
  AdvertiseByoipCidr
mkAdvertiseByoipCidr pCidr_ =
  AdvertiseByoipCidr' {dryRun = Lude.Nothing, cidr = pCidr_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abcDryRun :: Lens.Lens' AdvertiseByoipCidr (Lude.Maybe Lude.Bool)
abcDryRun = Lens.lens (dryRun :: AdvertiseByoipCidr -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AdvertiseByoipCidr)
{-# DEPRECATED abcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The address range, in CIDR notation. This must be the exact range that you provisioned. You can't advertise only a portion of the provisioned range.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abcCidr :: Lens.Lens' AdvertiseByoipCidr Lude.Text
abcCidr = Lens.lens (cidr :: AdvertiseByoipCidr -> Lude.Text) (\s a -> s {cidr = a} :: AdvertiseByoipCidr)
{-# DEPRECATED abcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.AWSRequest AdvertiseByoipCidr where
  type Rs AdvertiseByoipCidr = AdvertiseByoipCidrResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AdvertiseByoipCidrResponse'
            Lude.<$> (x Lude..@? "byoipCidr") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdvertiseByoipCidr where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AdvertiseByoipCidr where
  toPath = Lude.const "/"

instance Lude.ToQuery AdvertiseByoipCidr where
  toQuery AdvertiseByoipCidr' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AdvertiseByoipCidr" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Cidr" Lude.=: cidr
      ]

-- | /See:/ 'mkAdvertiseByoipCidrResponse' smart constructor.
data AdvertiseByoipCidrResponse = AdvertiseByoipCidrResponse'
  { byoipCidr ::
      Lude.Maybe ByoipCidr,
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

-- | Creates a value of 'AdvertiseByoipCidrResponse' with the minimum fields required to make a request.
--
-- * 'byoipCidr' - Information about the address range.
-- * 'responseStatus' - The response status code.
mkAdvertiseByoipCidrResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdvertiseByoipCidrResponse
mkAdvertiseByoipCidrResponse pResponseStatus_ =
  AdvertiseByoipCidrResponse'
    { byoipCidr = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abcrsByoipCidr :: Lens.Lens' AdvertiseByoipCidrResponse (Lude.Maybe ByoipCidr)
abcrsByoipCidr = Lens.lens (byoipCidr :: AdvertiseByoipCidrResponse -> Lude.Maybe ByoipCidr) (\s a -> s {byoipCidr = a} :: AdvertiseByoipCidrResponse)
{-# DEPRECATED abcrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abcrsResponseStatus :: Lens.Lens' AdvertiseByoipCidrResponse Lude.Int
abcrsResponseStatus = Lens.lens (responseStatus :: AdvertiseByoipCidrResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdvertiseByoipCidrResponse)
{-# DEPRECATED abcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

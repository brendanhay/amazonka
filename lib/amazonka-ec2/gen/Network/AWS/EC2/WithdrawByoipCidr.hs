{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.WithdrawByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an address range that is provisioned as an address pool.
--
-- You can perform this operation at most once every 10 seconds, even if you specify different address ranges each time.
-- It can take a few minutes before traffic to the specified addresses stops routing to AWS because of BGP propagation delays.
module Network.AWS.EC2.WithdrawByoipCidr
  ( -- * Creating a request
    WithdrawByoipCidr (..),
    mkWithdrawByoipCidr,

    -- ** Request lenses
    wbcCidr,
    wbcDryRun,

    -- * Destructuring the response
    WithdrawByoipCidrResponse (..),
    mkWithdrawByoipCidrResponse,

    -- ** Response lenses
    wbcrsByoipCidr,
    wbcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { -- | The address range, in CIDR notation.
    cidr :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WithdrawByoipCidr' with the minimum fields required to make a request.
--
-- * 'cidr' - The address range, in CIDR notation.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkWithdrawByoipCidr ::
  -- | 'cidr'
  Lude.Text ->
  WithdrawByoipCidr
mkWithdrawByoipCidr pCidr_ =
  WithdrawByoipCidr' {cidr = pCidr_, dryRun = Lude.Nothing}

-- | The address range, in CIDR notation.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcCidr :: Lens.Lens' WithdrawByoipCidr Lude.Text
wbcCidr = Lens.lens (cidr :: WithdrawByoipCidr -> Lude.Text) (\s a -> s {cidr = a} :: WithdrawByoipCidr)
{-# DEPRECATED wbcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcDryRun :: Lens.Lens' WithdrawByoipCidr (Lude.Maybe Lude.Bool)
wbcDryRun = Lens.lens (dryRun :: WithdrawByoipCidr -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: WithdrawByoipCidr)
{-# DEPRECATED wbcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest WithdrawByoipCidr where
  type Rs WithdrawByoipCidr = WithdrawByoipCidrResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          WithdrawByoipCidrResponse'
            Lude.<$> (x Lude..@? "byoipCidr") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders WithdrawByoipCidr where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath WithdrawByoipCidr where
  toPath = Lude.const "/"

instance Lude.ToQuery WithdrawByoipCidr where
  toQuery WithdrawByoipCidr' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("WithdrawByoipCidr" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Cidr" Lude.=: cidr,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { -- | Information about the address pool.
    byoipCidr :: Lude.Maybe ByoipCidr,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WithdrawByoipCidrResponse' with the minimum fields required to make a request.
--
-- * 'byoipCidr' - Information about the address pool.
-- * 'responseStatus' - The response status code.
mkWithdrawByoipCidrResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  WithdrawByoipCidrResponse
mkWithdrawByoipCidrResponse pResponseStatus_ =
  WithdrawByoipCidrResponse'
    { byoipCidr = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address pool.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrsByoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Lude.Maybe ByoipCidr)
wbcrsByoipCidr = Lens.lens (byoipCidr :: WithdrawByoipCidrResponse -> Lude.Maybe ByoipCidr) (\s a -> s {byoipCidr = a} :: WithdrawByoipCidrResponse)
{-# DEPRECATED wbcrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrsResponseStatus :: Lens.Lens' WithdrawByoipCidrResponse Lude.Int
wbcrsResponseStatus = Lens.lens (responseStatus :: WithdrawByoipCidrResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: WithdrawByoipCidrResponse)
{-# DEPRECATED wbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

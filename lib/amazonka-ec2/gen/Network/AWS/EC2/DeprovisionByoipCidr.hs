{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeprovisionByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified address range that you provisioned for use with your AWS resources through bring your own IP addresses (BYOIP) and deletes the corresponding address pool.
--
-- Before you can release an address range, you must stop advertising it using 'WithdrawByoipCidr' and you must not have any IP addresses allocated from its address range.
module Network.AWS.EC2.DeprovisionByoipCidr
  ( -- * Creating a request
    DeprovisionByoipCidr (..),
    mkDeprovisionByoipCidr,

    -- ** Request lenses
    depDryRun,
    depCidr,

    -- * Destructuring the response
    DeprovisionByoipCidrResponse (..),
    mkDeprovisionByoipCidrResponse,

    -- ** Response lenses
    deprsByoipCidr,
    deprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeprovisionByoipCidr' smart constructor.
data DeprovisionByoipCidr = DeprovisionByoipCidr'
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

-- | Creates a value of 'DeprovisionByoipCidr' with the minimum fields required to make a request.
--
-- * 'cidr' - The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeprovisionByoipCidr ::
  -- | 'cidr'
  Lude.Text ->
  DeprovisionByoipCidr
mkDeprovisionByoipCidr pCidr_ =
  DeprovisionByoipCidr' {dryRun = Lude.Nothing, cidr = pCidr_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depDryRun :: Lens.Lens' DeprovisionByoipCidr (Lude.Maybe Lude.Bool)
depDryRun = Lens.lens (dryRun :: DeprovisionByoipCidr -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeprovisionByoipCidr)
{-# DEPRECATED depDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depCidr :: Lens.Lens' DeprovisionByoipCidr Lude.Text
depCidr = Lens.lens (cidr :: DeprovisionByoipCidr -> Lude.Text) (\s a -> s {cidr = a} :: DeprovisionByoipCidr)
{-# DEPRECATED depCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.AWSRequest DeprovisionByoipCidr where
  type Rs DeprovisionByoipCidr = DeprovisionByoipCidrResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeprovisionByoipCidrResponse'
            Lude.<$> (x Lude..@? "byoipCidr") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeprovisionByoipCidr where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeprovisionByoipCidr where
  toPath = Lude.const "/"

instance Lude.ToQuery DeprovisionByoipCidr where
  toQuery DeprovisionByoipCidr' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeprovisionByoipCidr" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Cidr" Lude.=: cidr
      ]

-- | /See:/ 'mkDeprovisionByoipCidrResponse' smart constructor.
data DeprovisionByoipCidrResponse = DeprovisionByoipCidrResponse'
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

-- | Creates a value of 'DeprovisionByoipCidrResponse' with the minimum fields required to make a request.
--
-- * 'byoipCidr' - Information about the address range.
-- * 'responseStatus' - The response status code.
mkDeprovisionByoipCidrResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeprovisionByoipCidrResponse
mkDeprovisionByoipCidrResponse pResponseStatus_ =
  DeprovisionByoipCidrResponse'
    { byoipCidr = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprsByoipCidr :: Lens.Lens' DeprovisionByoipCidrResponse (Lude.Maybe ByoipCidr)
deprsByoipCidr = Lens.lens (byoipCidr :: DeprovisionByoipCidrResponse -> Lude.Maybe ByoipCidr) (\s a -> s {byoipCidr = a} :: DeprovisionByoipCidrResponse)
{-# DEPRECATED deprsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprsResponseStatus :: Lens.Lens' DeprovisionByoipCidrResponse Lude.Int
deprsResponseStatus = Lens.lens (responseStatus :: DeprovisionByoipCidrResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeprovisionByoipCidrResponse)
{-# DEPRECATED deprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

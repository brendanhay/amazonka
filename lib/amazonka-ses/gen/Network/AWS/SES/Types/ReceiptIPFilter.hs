{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptIPFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptIPFilter
  ( ReceiptIPFilter (..),

    -- * Smart constructor
    mkReceiptIPFilter,

    -- * Lenses
    rifPolicy,
    rifCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.ReceiptFilterPolicy

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptIPFilter' smart constructor.
data ReceiptIPFilter = ReceiptIPFilter'
  { policy ::
      ReceiptFilterPolicy,
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

-- | Creates a value of 'ReceiptIPFilter' with the minimum fields required to make a request.
--
-- * 'cidr' - A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
-- * 'policy' - Indicates whether to block or allow incoming mail from the specified IP addresses.
mkReceiptIPFilter ::
  -- | 'policy'
  ReceiptFilterPolicy ->
  -- | 'cidr'
  Lude.Text ->
  ReceiptIPFilter
mkReceiptIPFilter pPolicy_ pCidr_ =
  ReceiptIPFilter' {policy = pPolicy_, cidr = pCidr_}

-- | Indicates whether to block or allow incoming mail from the specified IP addresses.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rifPolicy :: Lens.Lens' ReceiptIPFilter ReceiptFilterPolicy
rifPolicy = Lens.lens (policy :: ReceiptIPFilter -> ReceiptFilterPolicy) (\s a -> s {policy = a} :: ReceiptIPFilter)
{-# DEPRECATED rifPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rifCidr :: Lens.Lens' ReceiptIPFilter Lude.Text
rifCidr = Lens.lens (cidr :: ReceiptIPFilter -> Lude.Text) (\s a -> s {cidr = a} :: ReceiptIPFilter)
{-# DEPRECATED rifCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.FromXML ReceiptIPFilter where
  parseXML x =
    ReceiptIPFilter'
      Lude.<$> (x Lude..@ "Policy") Lude.<*> (x Lude..@ "Cidr")

instance Lude.ToQuery ReceiptIPFilter where
  toQuery ReceiptIPFilter' {..} =
    Lude.mconcat ["Policy" Lude.=: policy, "Cidr" Lude.=: cidr]

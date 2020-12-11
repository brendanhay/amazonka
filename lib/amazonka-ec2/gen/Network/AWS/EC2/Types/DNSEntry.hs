-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSEntry
  ( DNSEntry (..),

    -- * Smart constructor
    mkDNSEntry,

    -- * Lenses
    deHostedZoneId,
    deDNSName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a DNS entry.
--
-- /See:/ 'mkDNSEntry' smart constructor.
data DNSEntry = DNSEntry'
  { hostedZoneId :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSEntry' with the minimum fields required to make a request.
--
-- * 'dnsName' - The DNS name.
-- * 'hostedZoneId' - The ID of the private hosted zone.
mkDNSEntry ::
  DNSEntry
mkDNSEntry =
  DNSEntry' {hostedZoneId = Lude.Nothing, dnsName = Lude.Nothing}

-- | The ID of the private hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deHostedZoneId :: Lens.Lens' DNSEntry (Lude.Maybe Lude.Text)
deHostedZoneId = Lens.lens (hostedZoneId :: DNSEntry -> Lude.Maybe Lude.Text) (\s a -> s {hostedZoneId = a} :: DNSEntry)
{-# DEPRECATED deHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The DNS name.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDNSName :: Lens.Lens' DNSEntry (Lude.Maybe Lude.Text)
deDNSName = Lens.lens (dnsName :: DNSEntry -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: DNSEntry)
{-# DEPRECATED deDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

instance Lude.FromXML DNSEntry where
  parseXML x =
    DNSEntry'
      Lude.<$> (x Lude..@? "hostedZoneId") Lude.<*> (x Lude..@? "dnsName")

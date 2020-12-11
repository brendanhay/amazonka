-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSConfigChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSConfigChange
  ( DNSConfigChange (..),

    -- * Smart constructor
    mkDNSConfigChange,

    -- * Lenses
    dccDNSRecords,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSRecord

-- | A complex type that contains information about changes to the Route 53 DNS records that AWS Cloud Map creates when you register an instance.
--
-- /See:/ 'mkDNSConfigChange' smart constructor.
newtype DNSConfigChange = DNSConfigChange'
  { dnsRecords ::
      [DNSRecord]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSConfigChange' with the minimum fields required to make a request.
--
-- * 'dnsRecords' - An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
mkDNSConfigChange ::
  DNSConfigChange
mkDNSConfigChange = DNSConfigChange' {dnsRecords = Lude.mempty}

-- | An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccDNSRecords :: Lens.Lens' DNSConfigChange [DNSRecord]
dccDNSRecords = Lens.lens (dnsRecords :: DNSConfigChange -> [DNSRecord]) (\s a -> s {dnsRecords = a} :: DNSConfigChange)
{-# DEPRECATED dccDNSRecords "Use generic-lens or generic-optics with 'dnsRecords' instead." #-}

instance Lude.ToJSON DNSConfigChange where
  toJSON DNSConfigChange' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DnsRecords" Lude..= dnsRecords)])

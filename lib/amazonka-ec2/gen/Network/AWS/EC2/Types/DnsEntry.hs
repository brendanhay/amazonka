{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DnsEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DnsEntry
  ( DnsEntry (..)
  -- * Smart constructor
  , mkDnsEntry
  -- * Lenses
  , deDnsName
  , deHostedZoneId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a DNS entry.
--
-- /See:/ 'mkDnsEntry' smart constructor.
data DnsEntry = DnsEntry'
  { dnsName :: Core.Maybe Core.Text
    -- ^ The DNS name.
  , hostedZoneId :: Core.Maybe Core.Text
    -- ^ The ID of the private hosted zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DnsEntry' value with any optional fields omitted.
mkDnsEntry
    :: DnsEntry
mkDnsEntry
  = DnsEntry'{dnsName = Core.Nothing, hostedZoneId = Core.Nothing}

-- | The DNS name.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDnsName :: Lens.Lens' DnsEntry (Core.Maybe Core.Text)
deDnsName = Lens.field @"dnsName"
{-# INLINEABLE deDnsName #-}
{-# DEPRECATED dnsName "Use generic-lens or generic-optics with 'dnsName' instead"  #-}

-- | The ID of the private hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deHostedZoneId :: Lens.Lens' DnsEntry (Core.Maybe Core.Text)
deHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE deHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

instance Core.FromXML DnsEntry where
        parseXML x
          = DnsEntry' Core.<$>
              (x Core..@? "dnsName") Core.<*> x Core..@? "hostedZoneId"

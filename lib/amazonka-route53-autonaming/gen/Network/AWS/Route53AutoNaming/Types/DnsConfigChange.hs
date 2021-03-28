{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsConfigChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.DnsConfigChange
  ( DnsConfigChange (..)
  -- * Smart constructor
  , mkDnsConfigChange
  -- * Lenses
  , dccDnsRecords
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.DnsRecord as Types

-- | A complex type that contains information about changes to the Route 53 DNS records that AWS Cloud Map creates when you register an instance.
--
-- /See:/ 'mkDnsConfigChange' smart constructor.
newtype DnsConfigChange = DnsConfigChange'
  { dnsRecords :: [Types.DnsRecord]
    -- ^ An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DnsConfigChange' value with any optional fields omitted.
mkDnsConfigChange
    :: DnsConfigChange
mkDnsConfigChange = DnsConfigChange'{dnsRecords = Core.mempty}

-- | An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccDnsRecords :: Lens.Lens' DnsConfigChange [Types.DnsRecord]
dccDnsRecords = Lens.field @"dnsRecords"
{-# INLINEABLE dccDnsRecords #-}
{-# DEPRECATED dnsRecords "Use generic-lens or generic-optics with 'dnsRecords' instead"  #-}

instance Core.FromJSON DnsConfigChange where
        toJSON DnsConfigChange{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DnsRecords" Core..= dnsRecords)])

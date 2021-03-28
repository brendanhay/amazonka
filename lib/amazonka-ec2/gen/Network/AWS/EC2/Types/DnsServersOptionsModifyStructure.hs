{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DnsServersOptionsModifyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DnsServersOptionsModifyStructure
  ( DnsServersOptionsModifyStructure (..)
  -- * Smart constructor
  , mkDnsServersOptionsModifyStructure
  -- * Lenses
  , dsomsCustomDnsServers
  , dsomsEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the DNS server to be used.
--
-- /See:/ 'mkDnsServersOptionsModifyStructure' smart constructor.
data DnsServersOptionsModifyStructure = DnsServersOptionsModifyStructure'
  { customDnsServers :: Core.Maybe [Core.Text]
    -- ^ The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DnsServersOptionsModifyStructure' value with any optional fields omitted.
mkDnsServersOptionsModifyStructure
    :: DnsServersOptionsModifyStructure
mkDnsServersOptionsModifyStructure
  = DnsServersOptionsModifyStructure'{customDnsServers =
                                        Core.Nothing,
                                      enabled = Core.Nothing}

-- | The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
--
-- /Note:/ Consider using 'customDnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsomsCustomDnsServers :: Lens.Lens' DnsServersOptionsModifyStructure (Core.Maybe [Core.Text])
dsomsCustomDnsServers = Lens.field @"customDnsServers"
{-# INLINEABLE dsomsCustomDnsServers #-}
{-# DEPRECATED customDnsServers "Use generic-lens or generic-optics with 'customDnsServers' instead"  #-}

-- | Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsomsEnabled :: Lens.Lens' DnsServersOptionsModifyStructure (Core.Maybe Core.Bool)
dsomsEnabled = Lens.field @"enabled"
{-# INLINEABLE dsomsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery DnsServersOptionsModifyStructure where
        toQuery DnsServersOptionsModifyStructure{..}
          = Core.maybe Core.mempty (Core.toQueryList "CustomDnsServers")
              customDnsServers
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled

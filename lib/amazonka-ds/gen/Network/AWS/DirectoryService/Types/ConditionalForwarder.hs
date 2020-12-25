{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ConditionalForwarder
  ( ConditionalForwarder (..),

    -- * Smart constructor
    mkConditionalForwarder,

    -- * Lenses
    cfDnsIpAddrs,
    cfRemoteDomainName,
    cfReplicationScope,
  )
where

import qualified Network.AWS.DirectoryService.Types.IpAddr as Types
import qualified Network.AWS.DirectoryService.Types.RemoteDomainName as Types
import qualified Network.AWS.DirectoryService.Types.ReplicationScope as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
-- /See:/ 'mkConditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { -- | The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
    dnsIpAddrs :: Core.Maybe [Types.IpAddr],
    -- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
    remoteDomainName :: Core.Maybe Types.RemoteDomainName,
    -- | The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
    replicationScope :: Core.Maybe Types.ReplicationScope
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConditionalForwarder' value with any optional fields omitted.
mkConditionalForwarder ::
  ConditionalForwarder
mkConditionalForwarder =
  ConditionalForwarder'
    { dnsIpAddrs = Core.Nothing,
      remoteDomainName = Core.Nothing,
      replicationScope = Core.Nothing
    }

-- | The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDnsIpAddrs :: Lens.Lens' ConditionalForwarder (Core.Maybe [Types.IpAddr])
cfDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# DEPRECATED cfDnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRemoteDomainName :: Lens.Lens' ConditionalForwarder (Core.Maybe Types.RemoteDomainName)
cfRemoteDomainName = Lens.field @"remoteDomainName"
{-# DEPRECATED cfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
--
-- /Note:/ Consider using 'replicationScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReplicationScope :: Lens.Lens' ConditionalForwarder (Core.Maybe Types.ReplicationScope)
cfReplicationScope = Lens.field @"replicationScope"
{-# DEPRECATED cfReplicationScope "Use generic-lens or generic-optics with 'replicationScope' instead." #-}

instance Core.FromJSON ConditionalForwarder where
  parseJSON =
    Core.withObject "ConditionalForwarder" Core.$
      \x ->
        ConditionalForwarder'
          Core.<$> (x Core..:? "DnsIpAddrs")
          Core.<*> (x Core..:? "RemoteDomainName")
          Core.<*> (x Core..:? "ReplicationScope")

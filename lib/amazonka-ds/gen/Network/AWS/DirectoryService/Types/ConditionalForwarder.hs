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
    cfDNSIPAddrs,
    cfRemoteDomainName,
    cfReplicationScope,
  )
where

import Network.AWS.DirectoryService.Types.ReplicationScope
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
-- /See:/ 'mkConditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { dnsIPAddrs ::
      Lude.Maybe [Lude.Text],
    remoteDomainName :: Lude.Maybe Lude.Text,
    replicationScope :: Lude.Maybe ReplicationScope
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConditionalForwarder' with the minimum fields required to make a request.
--
-- * 'dnsIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
-- * 'remoteDomainName' - The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
-- * 'replicationScope' - The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
mkConditionalForwarder ::
  ConditionalForwarder
mkConditionalForwarder =
  ConditionalForwarder'
    { dnsIPAddrs = Lude.Nothing,
      remoteDomainName = Lude.Nothing,
      replicationScope = Lude.Nothing
    }

-- | The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
--
-- /Note:/ Consider using 'dnsIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDNSIPAddrs :: Lens.Lens' ConditionalForwarder (Lude.Maybe [Lude.Text])
cfDNSIPAddrs = Lens.lens (dnsIPAddrs :: ConditionalForwarder -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsIPAddrs = a} :: ConditionalForwarder)
{-# DEPRECATED cfDNSIPAddrs "Use generic-lens or generic-optics with 'dnsIPAddrs' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRemoteDomainName :: Lens.Lens' ConditionalForwarder (Lude.Maybe Lude.Text)
cfRemoteDomainName = Lens.lens (remoteDomainName :: ConditionalForwarder -> Lude.Maybe Lude.Text) (\s a -> s {remoteDomainName = a} :: ConditionalForwarder)
{-# DEPRECATED cfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
--
-- /Note:/ Consider using 'replicationScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReplicationScope :: Lens.Lens' ConditionalForwarder (Lude.Maybe ReplicationScope)
cfReplicationScope = Lens.lens (replicationScope :: ConditionalForwarder -> Lude.Maybe ReplicationScope) (\s a -> s {replicationScope = a} :: ConditionalForwarder)
{-# DEPRECATED cfReplicationScope "Use generic-lens or generic-optics with 'replicationScope' instead." #-}

instance Lude.FromJSON ConditionalForwarder where
  parseJSON =
    Lude.withObject
      "ConditionalForwarder"
      ( \x ->
          ConditionalForwarder'
            Lude.<$> (x Lude..:? "DnsIpAddrs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RemoteDomainName")
            Lude.<*> (x Lude..:? "ReplicationScope")
      )

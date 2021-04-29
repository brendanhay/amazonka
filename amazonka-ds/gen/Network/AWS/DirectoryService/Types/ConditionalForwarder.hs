{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ConditionalForwarder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ConditionalForwarder where

import Network.AWS.DirectoryService.Types.ReplicationScope
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Points to a remote domain with which you are setting up a trust
-- relationship. Conditional forwarders are required in order to set up a
-- trust relationship with another domain.
--
-- /See:/ 'newConditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { -- | The replication scope of the conditional forwarder. The only allowed
    -- value is @Domain@, which will replicate the conditional forwarder to all
    -- of the domain controllers for your AWS directory.
    replicationScope :: Prelude.Maybe ReplicationScope,
    -- | The fully qualified domain name (FQDN) of the remote domains pointed to
    -- by the conditional forwarder.
    remoteDomainName :: Prelude.Maybe Prelude.Text,
    -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName. This is the IP address of the DNS server that your
    -- conditional forwarder points to.
    dnsIpAddrs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationScope', 'conditionalForwarder_replicationScope' - The replication scope of the conditional forwarder. The only allowed
-- value is @Domain@, which will replicate the conditional forwarder to all
-- of the domain controllers for your AWS directory.
--
-- 'remoteDomainName', 'conditionalForwarder_remoteDomainName' - The fully qualified domain name (FQDN) of the remote domains pointed to
-- by the conditional forwarder.
--
-- 'dnsIpAddrs', 'conditionalForwarder_dnsIpAddrs' - The IP addresses of the remote DNS server associated with
-- RemoteDomainName. This is the IP address of the DNS server that your
-- conditional forwarder points to.
newConditionalForwarder ::
  ConditionalForwarder
newConditionalForwarder =
  ConditionalForwarder'
    { replicationScope =
        Prelude.Nothing,
      remoteDomainName = Prelude.Nothing,
      dnsIpAddrs = Prelude.Nothing
    }

-- | The replication scope of the conditional forwarder. The only allowed
-- value is @Domain@, which will replicate the conditional forwarder to all
-- of the domain controllers for your AWS directory.
conditionalForwarder_replicationScope :: Lens.Lens' ConditionalForwarder (Prelude.Maybe ReplicationScope)
conditionalForwarder_replicationScope = Lens.lens (\ConditionalForwarder' {replicationScope} -> replicationScope) (\s@ConditionalForwarder' {} a -> s {replicationScope = a} :: ConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domains pointed to
-- by the conditional forwarder.
conditionalForwarder_remoteDomainName :: Lens.Lens' ConditionalForwarder (Prelude.Maybe Prelude.Text)
conditionalForwarder_remoteDomainName = Lens.lens (\ConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@ConditionalForwarder' {} a -> s {remoteDomainName = a} :: ConditionalForwarder)

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName. This is the IP address of the DNS server that your
-- conditional forwarder points to.
conditionalForwarder_dnsIpAddrs :: Lens.Lens' ConditionalForwarder (Prelude.Maybe [Prelude.Text])
conditionalForwarder_dnsIpAddrs = Lens.lens (\ConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@ConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: ConditionalForwarder) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ConditionalForwarder where
  parseJSON =
    Prelude.withObject
      "ConditionalForwarder"
      ( \x ->
          ConditionalForwarder'
            Prelude.<$> (x Prelude..:? "ReplicationScope")
            Prelude.<*> (x Prelude..:? "RemoteDomainName")
            Prelude.<*> ( x Prelude..:? "DnsIpAddrs"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConditionalForwarder

instance Prelude.NFData ConditionalForwarder

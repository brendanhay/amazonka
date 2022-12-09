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
-- Module      : Amazonka.DirectoryService.Types.ConditionalForwarder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.ConditionalForwarder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.ReplicationScope
import qualified Amazonka.Prelude as Prelude

-- | Points to a remote domain with which you are setting up a trust
-- relationship. Conditional forwarders are required in order to set up a
-- trust relationship with another domain.
--
-- /See:/ 'newConditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName. This is the IP address of the DNS server that your
    -- conditional forwarder points to.
    dnsIpAddrs :: Prelude.Maybe [Prelude.Text],
    -- | The fully qualified domain name (FQDN) of the remote domains pointed to
    -- by the conditional forwarder.
    remoteDomainName :: Prelude.Maybe Prelude.Text,
    -- | The replication scope of the conditional forwarder. The only allowed
    -- value is @Domain@, which will replicate the conditional forwarder to all
    -- of the domain controllers for your Amazon Web Services directory.
    replicationScope :: Prelude.Maybe ReplicationScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsIpAddrs', 'conditionalForwarder_dnsIpAddrs' - The IP addresses of the remote DNS server associated with
-- RemoteDomainName. This is the IP address of the DNS server that your
-- conditional forwarder points to.
--
-- 'remoteDomainName', 'conditionalForwarder_remoteDomainName' - The fully qualified domain name (FQDN) of the remote domains pointed to
-- by the conditional forwarder.
--
-- 'replicationScope', 'conditionalForwarder_replicationScope' - The replication scope of the conditional forwarder. The only allowed
-- value is @Domain@, which will replicate the conditional forwarder to all
-- of the domain controllers for your Amazon Web Services directory.
newConditionalForwarder ::
  ConditionalForwarder
newConditionalForwarder =
  ConditionalForwarder'
    { dnsIpAddrs = Prelude.Nothing,
      remoteDomainName = Prelude.Nothing,
      replicationScope = Prelude.Nothing
    }

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName. This is the IP address of the DNS server that your
-- conditional forwarder points to.
conditionalForwarder_dnsIpAddrs :: Lens.Lens' ConditionalForwarder (Prelude.Maybe [Prelude.Text])
conditionalForwarder_dnsIpAddrs = Lens.lens (\ConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@ConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: ConditionalForwarder) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified domain name (FQDN) of the remote domains pointed to
-- by the conditional forwarder.
conditionalForwarder_remoteDomainName :: Lens.Lens' ConditionalForwarder (Prelude.Maybe Prelude.Text)
conditionalForwarder_remoteDomainName = Lens.lens (\ConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@ConditionalForwarder' {} a -> s {remoteDomainName = a} :: ConditionalForwarder)

-- | The replication scope of the conditional forwarder. The only allowed
-- value is @Domain@, which will replicate the conditional forwarder to all
-- of the domain controllers for your Amazon Web Services directory.
conditionalForwarder_replicationScope :: Lens.Lens' ConditionalForwarder (Prelude.Maybe ReplicationScope)
conditionalForwarder_replicationScope = Lens.lens (\ConditionalForwarder' {replicationScope} -> replicationScope) (\s@ConditionalForwarder' {} a -> s {replicationScope = a} :: ConditionalForwarder)

instance Data.FromJSON ConditionalForwarder where
  parseJSON =
    Data.withObject
      "ConditionalForwarder"
      ( \x ->
          ConditionalForwarder'
            Prelude.<$> (x Data..:? "DnsIpAddrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RemoteDomainName")
            Prelude.<*> (x Data..:? "ReplicationScope")
      )

instance Prelude.Hashable ConditionalForwarder where
  hashWithSalt _salt ConditionalForwarder' {..} =
    _salt `Prelude.hashWithSalt` dnsIpAddrs
      `Prelude.hashWithSalt` remoteDomainName
      `Prelude.hashWithSalt` replicationScope

instance Prelude.NFData ConditionalForwarder where
  rnf ConditionalForwarder' {..} =
    Prelude.rnf dnsIpAddrs
      `Prelude.seq` Prelude.rnf remoteDomainName
      `Prelude.seq` Prelude.rnf replicationScope

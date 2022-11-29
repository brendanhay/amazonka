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
-- Module      : Amazonka.AppMesh.Types.DnsServiceDiscovery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.DnsServiceDiscovery where

import Amazonka.AppMesh.Types.DnsResponseType
import Amazonka.AppMesh.Types.IpPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the DNS service discovery information for your
-- virtual node.
--
-- /See:/ 'newDnsServiceDiscovery' smart constructor.
data DnsServiceDiscovery = DnsServiceDiscovery'
  { -- | Specifies the DNS response type for the virtual node.
    responseType :: Prelude.Maybe DnsResponseType,
    -- | The preferred IP version that this virtual node uses. Setting the IP
    -- preference on the virtual node only overrides the IP preference set for
    -- the mesh on this specific node.
    ipPreference :: Prelude.Maybe IpPreference,
    -- | Specifies the DNS service discovery hostname for the virtual node.
    hostname :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsServiceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseType', 'dnsServiceDiscovery_responseType' - Specifies the DNS response type for the virtual node.
--
-- 'ipPreference', 'dnsServiceDiscovery_ipPreference' - The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
--
-- 'hostname', 'dnsServiceDiscovery_hostname' - Specifies the DNS service discovery hostname for the virtual node.
newDnsServiceDiscovery ::
  -- | 'hostname'
  Prelude.Text ->
  DnsServiceDiscovery
newDnsServiceDiscovery pHostname_ =
  DnsServiceDiscovery'
    { responseType =
        Prelude.Nothing,
      ipPreference = Prelude.Nothing,
      hostname = pHostname_
    }

-- | Specifies the DNS response type for the virtual node.
dnsServiceDiscovery_responseType :: Lens.Lens' DnsServiceDiscovery (Prelude.Maybe DnsResponseType)
dnsServiceDiscovery_responseType = Lens.lens (\DnsServiceDiscovery' {responseType} -> responseType) (\s@DnsServiceDiscovery' {} a -> s {responseType = a} :: DnsServiceDiscovery)

-- | The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
dnsServiceDiscovery_ipPreference :: Lens.Lens' DnsServiceDiscovery (Prelude.Maybe IpPreference)
dnsServiceDiscovery_ipPreference = Lens.lens (\DnsServiceDiscovery' {ipPreference} -> ipPreference) (\s@DnsServiceDiscovery' {} a -> s {ipPreference = a} :: DnsServiceDiscovery)

-- | Specifies the DNS service discovery hostname for the virtual node.
dnsServiceDiscovery_hostname :: Lens.Lens' DnsServiceDiscovery Prelude.Text
dnsServiceDiscovery_hostname = Lens.lens (\DnsServiceDiscovery' {hostname} -> hostname) (\s@DnsServiceDiscovery' {} a -> s {hostname = a} :: DnsServiceDiscovery)

instance Core.FromJSON DnsServiceDiscovery where
  parseJSON =
    Core.withObject
      "DnsServiceDiscovery"
      ( \x ->
          DnsServiceDiscovery'
            Prelude.<$> (x Core..:? "responseType")
            Prelude.<*> (x Core..:? "ipPreference")
            Prelude.<*> (x Core..: "hostname")
      )

instance Prelude.Hashable DnsServiceDiscovery where
  hashWithSalt _salt DnsServiceDiscovery' {..} =
    _salt `Prelude.hashWithSalt` responseType
      `Prelude.hashWithSalt` ipPreference
      `Prelude.hashWithSalt` hostname

instance Prelude.NFData DnsServiceDiscovery where
  rnf DnsServiceDiscovery' {..} =
    Prelude.rnf responseType
      `Prelude.seq` Prelude.rnf ipPreference
      `Prelude.seq` Prelude.rnf hostname

instance Core.ToJSON DnsServiceDiscovery where
  toJSON DnsServiceDiscovery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("responseType" Core..=) Prelude.<$> responseType,
            ("ipPreference" Core..=) Prelude.<$> ipPreference,
            Prelude.Just ("hostname" Core..= hostname)
          ]
      )

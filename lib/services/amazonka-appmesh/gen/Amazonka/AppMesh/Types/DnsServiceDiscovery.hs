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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.DnsServiceDiscovery where

import Amazonka.AppMesh.Types.DnsResponseType
import Amazonka.AppMesh.Types.IpPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the DNS service discovery information for your
-- virtual node.
--
-- /See:/ 'newDnsServiceDiscovery' smart constructor.
data DnsServiceDiscovery = DnsServiceDiscovery'
  { -- | The preferred IP version that this virtual node uses. Setting the IP
    -- preference on the virtual node only overrides the IP preference set for
    -- the mesh on this specific node.
    ipPreference :: Prelude.Maybe IpPreference,
    -- | Specifies the DNS response type for the virtual node.
    responseType :: Prelude.Maybe DnsResponseType,
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
-- 'ipPreference', 'dnsServiceDiscovery_ipPreference' - The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
--
-- 'responseType', 'dnsServiceDiscovery_responseType' - Specifies the DNS response type for the virtual node.
--
-- 'hostname', 'dnsServiceDiscovery_hostname' - Specifies the DNS service discovery hostname for the virtual node.
newDnsServiceDiscovery ::
  -- | 'hostname'
  Prelude.Text ->
  DnsServiceDiscovery
newDnsServiceDiscovery pHostname_ =
  DnsServiceDiscovery'
    { ipPreference =
        Prelude.Nothing,
      responseType = Prelude.Nothing,
      hostname = pHostname_
    }

-- | The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
dnsServiceDiscovery_ipPreference :: Lens.Lens' DnsServiceDiscovery (Prelude.Maybe IpPreference)
dnsServiceDiscovery_ipPreference = Lens.lens (\DnsServiceDiscovery' {ipPreference} -> ipPreference) (\s@DnsServiceDiscovery' {} a -> s {ipPreference = a} :: DnsServiceDiscovery)

-- | Specifies the DNS response type for the virtual node.
dnsServiceDiscovery_responseType :: Lens.Lens' DnsServiceDiscovery (Prelude.Maybe DnsResponseType)
dnsServiceDiscovery_responseType = Lens.lens (\DnsServiceDiscovery' {responseType} -> responseType) (\s@DnsServiceDiscovery' {} a -> s {responseType = a} :: DnsServiceDiscovery)

-- | Specifies the DNS service discovery hostname for the virtual node.
dnsServiceDiscovery_hostname :: Lens.Lens' DnsServiceDiscovery Prelude.Text
dnsServiceDiscovery_hostname = Lens.lens (\DnsServiceDiscovery' {hostname} -> hostname) (\s@DnsServiceDiscovery' {} a -> s {hostname = a} :: DnsServiceDiscovery)

instance Data.FromJSON DnsServiceDiscovery where
  parseJSON =
    Data.withObject
      "DnsServiceDiscovery"
      ( \x ->
          DnsServiceDiscovery'
            Prelude.<$> (x Data..:? "ipPreference")
            Prelude.<*> (x Data..:? "responseType")
            Prelude.<*> (x Data..: "hostname")
      )

instance Prelude.Hashable DnsServiceDiscovery where
  hashWithSalt _salt DnsServiceDiscovery' {..} =
    _salt `Prelude.hashWithSalt` ipPreference
      `Prelude.hashWithSalt` responseType
      `Prelude.hashWithSalt` hostname

instance Prelude.NFData DnsServiceDiscovery where
  rnf DnsServiceDiscovery' {..} =
    Prelude.rnf ipPreference
      `Prelude.seq` Prelude.rnf responseType
      `Prelude.seq` Prelude.rnf hostname

instance Data.ToJSON DnsServiceDiscovery where
  toJSON DnsServiceDiscovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ipPreference" Data..=) Prelude.<$> ipPreference,
            ("responseType" Data..=) Prelude.<$> responseType,
            Prelude.Just ("hostname" Data..= hostname)
          ]
      )

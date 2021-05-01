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
-- Module      : Network.AWS.EC2.Types.DnsServersOptionsModifyStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DnsServersOptionsModifyStructure where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the DNS server to be used.
--
-- /See:/ 'newDnsServersOptionsModifyStructure' smart constructor.
data DnsServersOptionsModifyStructure = DnsServersOptionsModifyStructure'
  { -- | Indicates whether DNS servers should be used. Specify @False@ to delete
    -- the existing DNS servers.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 address range, in CIDR notation, of the DNS servers to be used.
    -- You can specify up to two DNS servers. Ensure that the DNS servers can
    -- be reached by the clients. The specified values overwrite the existing
    -- values.
    customDnsServers :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DnsServersOptionsModifyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'dnsServersOptionsModifyStructure_enabled' - Indicates whether DNS servers should be used. Specify @False@ to delete
-- the existing DNS servers.
--
-- 'customDnsServers', 'dnsServersOptionsModifyStructure_customDnsServers' - The IPv4 address range, in CIDR notation, of the DNS servers to be used.
-- You can specify up to two DNS servers. Ensure that the DNS servers can
-- be reached by the clients. The specified values overwrite the existing
-- values.
newDnsServersOptionsModifyStructure ::
  DnsServersOptionsModifyStructure
newDnsServersOptionsModifyStructure =
  DnsServersOptionsModifyStructure'
    { enabled =
        Prelude.Nothing,
      customDnsServers = Prelude.Nothing
    }

-- | Indicates whether DNS servers should be used. Specify @False@ to delete
-- the existing DNS servers.
dnsServersOptionsModifyStructure_enabled :: Lens.Lens' DnsServersOptionsModifyStructure (Prelude.Maybe Prelude.Bool)
dnsServersOptionsModifyStructure_enabled = Lens.lens (\DnsServersOptionsModifyStructure' {enabled} -> enabled) (\s@DnsServersOptionsModifyStructure' {} a -> s {enabled = a} :: DnsServersOptionsModifyStructure)

-- | The IPv4 address range, in CIDR notation, of the DNS servers to be used.
-- You can specify up to two DNS servers. Ensure that the DNS servers can
-- be reached by the clients. The specified values overwrite the existing
-- values.
dnsServersOptionsModifyStructure_customDnsServers :: Lens.Lens' DnsServersOptionsModifyStructure (Prelude.Maybe [Prelude.Text])
dnsServersOptionsModifyStructure_customDnsServers = Lens.lens (\DnsServersOptionsModifyStructure' {customDnsServers} -> customDnsServers) (\s@DnsServersOptionsModifyStructure' {} a -> s {customDnsServers = a} :: DnsServersOptionsModifyStructure) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    DnsServersOptionsModifyStructure

instance
  Prelude.NFData
    DnsServersOptionsModifyStructure

instance
  Prelude.ToQuery
    DnsServersOptionsModifyStructure
  where
  toQuery DnsServersOptionsModifyStructure' {..} =
    Prelude.mconcat
      [ "Enabled" Prelude.=: enabled,
        Prelude.toQuery
          ( Prelude.toQueryList "CustomDnsServers"
              Prelude.<$> customDnsServers
          )
      ]

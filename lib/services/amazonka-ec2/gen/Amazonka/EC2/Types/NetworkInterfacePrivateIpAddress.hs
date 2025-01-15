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
-- Module      : Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import qualified Amazonka.Prelude as Prelude

-- | Describes the private IPv4 address of a network interface.
--
-- /See:/ 'newNetworkInterfacePrivateIpAddress' smart constructor.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress'
  { -- | The association information for an Elastic IP address (IPv4) associated
    -- with the network interface.
    association :: Prelude.Maybe NetworkInterfaceAssociation,
    -- | Indicates whether this IPv4 address is the primary private IPv4 address
    -- of the network interface.
    primary :: Prelude.Maybe Prelude.Bool,
    -- | The private DNS name.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 address.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfacePrivateIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'networkInterfacePrivateIpAddress_association' - The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
--
-- 'primary', 'networkInterfacePrivateIpAddress_primary' - Indicates whether this IPv4 address is the primary private IPv4 address
-- of the network interface.
--
-- 'privateDnsName', 'networkInterfacePrivateIpAddress_privateDnsName' - The private DNS name.
--
-- 'privateIpAddress', 'networkInterfacePrivateIpAddress_privateIpAddress' - The private IPv4 address.
newNetworkInterfacePrivateIpAddress ::
  NetworkInterfacePrivateIpAddress
newNetworkInterfacePrivateIpAddress =
  NetworkInterfacePrivateIpAddress'
    { association =
        Prelude.Nothing,
      primary = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
networkInterfacePrivateIpAddress_association :: Lens.Lens' NetworkInterfacePrivateIpAddress (Prelude.Maybe NetworkInterfaceAssociation)
networkInterfacePrivateIpAddress_association = Lens.lens (\NetworkInterfacePrivateIpAddress' {association} -> association) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {association = a} :: NetworkInterfacePrivateIpAddress)

-- | Indicates whether this IPv4 address is the primary private IPv4 address
-- of the network interface.
networkInterfacePrivateIpAddress_primary :: Lens.Lens' NetworkInterfacePrivateIpAddress (Prelude.Maybe Prelude.Bool)
networkInterfacePrivateIpAddress_primary = Lens.lens (\NetworkInterfacePrivateIpAddress' {primary} -> primary) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {primary = a} :: NetworkInterfacePrivateIpAddress)

-- | The private DNS name.
networkInterfacePrivateIpAddress_privateDnsName :: Lens.Lens' NetworkInterfacePrivateIpAddress (Prelude.Maybe Prelude.Text)
networkInterfacePrivateIpAddress_privateDnsName = Lens.lens (\NetworkInterfacePrivateIpAddress' {privateDnsName} -> privateDnsName) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {privateDnsName = a} :: NetworkInterfacePrivateIpAddress)

-- | The private IPv4 address.
networkInterfacePrivateIpAddress_privateIpAddress :: Lens.Lens' NetworkInterfacePrivateIpAddress (Prelude.Maybe Prelude.Text)
networkInterfacePrivateIpAddress_privateIpAddress = Lens.lens (\NetworkInterfacePrivateIpAddress' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {privateIpAddress = a} :: NetworkInterfacePrivateIpAddress)

instance
  Data.FromXML
    NetworkInterfacePrivateIpAddress
  where
  parseXML x =
    NetworkInterfacePrivateIpAddress'
      Prelude.<$> (x Data..@? "association")
      Prelude.<*> (x Data..@? "primary")
      Prelude.<*> (x Data..@? "privateDnsName")
      Prelude.<*> (x Data..@? "privateIpAddress")

instance
  Prelude.Hashable
    NetworkInterfacePrivateIpAddress
  where
  hashWithSalt
    _salt
    NetworkInterfacePrivateIpAddress' {..} =
      _salt
        `Prelude.hashWithSalt` association
        `Prelude.hashWithSalt` primary
        `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` privateIpAddress

instance
  Prelude.NFData
    NetworkInterfacePrivateIpAddress
  where
  rnf NetworkInterfacePrivateIpAddress' {..} =
    Prelude.rnf association `Prelude.seq`
      Prelude.rnf primary `Prelude.seq`
        Prelude.rnf privateDnsName `Prelude.seq`
          Prelude.rnf privateIpAddress

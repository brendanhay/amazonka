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
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import qualified Network.AWS.Lens as Lens

-- | Describes the private IPv4 address of a network interface.
--
-- /See:/ 'newNetworkInterfacePrivateIpAddress' smart constructor.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress'
  { -- | Indicates whether this IPv4 address is the primary private IPv4 address
    -- of the network interface.
    primary :: Core.Maybe Core.Bool,
    -- | The association information for an Elastic IP address (IPv4) associated
    -- with the network interface.
    association :: Core.Maybe NetworkInterfaceAssociation,
    -- | The private DNS name.
    privateDnsName :: Core.Maybe Core.Text,
    -- | The private IPv4 address.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInterfacePrivateIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'networkInterfacePrivateIpAddress_primary' - Indicates whether this IPv4 address is the primary private IPv4 address
-- of the network interface.
--
-- 'association', 'networkInterfacePrivateIpAddress_association' - The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
--
-- 'privateDnsName', 'networkInterfacePrivateIpAddress_privateDnsName' - The private DNS name.
--
-- 'privateIpAddress', 'networkInterfacePrivateIpAddress_privateIpAddress' - The private IPv4 address.
newNetworkInterfacePrivateIpAddress ::
  NetworkInterfacePrivateIpAddress
newNetworkInterfacePrivateIpAddress =
  NetworkInterfacePrivateIpAddress'
    { primary =
        Core.Nothing,
      association = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | Indicates whether this IPv4 address is the primary private IPv4 address
-- of the network interface.
networkInterfacePrivateIpAddress_primary :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Bool)
networkInterfacePrivateIpAddress_primary = Lens.lens (\NetworkInterfacePrivateIpAddress' {primary} -> primary) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {primary = a} :: NetworkInterfacePrivateIpAddress)

-- | The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
networkInterfacePrivateIpAddress_association :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe NetworkInterfaceAssociation)
networkInterfacePrivateIpAddress_association = Lens.lens (\NetworkInterfacePrivateIpAddress' {association} -> association) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {association = a} :: NetworkInterfacePrivateIpAddress)

-- | The private DNS name.
networkInterfacePrivateIpAddress_privateDnsName :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Text)
networkInterfacePrivateIpAddress_privateDnsName = Lens.lens (\NetworkInterfacePrivateIpAddress' {privateDnsName} -> privateDnsName) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {privateDnsName = a} :: NetworkInterfacePrivateIpAddress)

-- | The private IPv4 address.
networkInterfacePrivateIpAddress_privateIpAddress :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Text)
networkInterfacePrivateIpAddress_privateIpAddress = Lens.lens (\NetworkInterfacePrivateIpAddress' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterfacePrivateIpAddress' {} a -> s {privateIpAddress = a} :: NetworkInterfacePrivateIpAddress)

instance
  Core.FromXML
    NetworkInterfacePrivateIpAddress
  where
  parseXML x =
    NetworkInterfacePrivateIpAddress'
      Core.<$> (x Core..@? "primary")
      Core.<*> (x Core..@? "association")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateIpAddress")

instance
  Core.Hashable
    NetworkInterfacePrivateIpAddress

instance Core.NFData NetworkInterfacePrivateIpAddress

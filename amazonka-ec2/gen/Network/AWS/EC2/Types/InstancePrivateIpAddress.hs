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
-- Module      : Network.AWS.EC2.Types.InstancePrivateIpAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstancePrivateIpAddress where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import qualified Network.AWS.Lens as Lens

-- | Describes a private IPv4 address.
--
-- /See:/ 'newInstancePrivateIpAddress' smart constructor.
data InstancePrivateIpAddress = InstancePrivateIpAddress'
  { -- | Indicates whether this IPv4 address is the primary private IP address of
    -- the network interface.
    primary :: Core.Maybe Core.Bool,
    -- | The association information for an Elastic IP address for the network
    -- interface.
    association :: Core.Maybe InstanceNetworkInterfaceAssociation,
    -- | The private IPv4 DNS name.
    privateDnsName :: Core.Maybe Core.Text,
    -- | The private IPv4 address of the network interface.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstancePrivateIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'instancePrivateIpAddress_primary' - Indicates whether this IPv4 address is the primary private IP address of
-- the network interface.
--
-- 'association', 'instancePrivateIpAddress_association' - The association information for an Elastic IP address for the network
-- interface.
--
-- 'privateDnsName', 'instancePrivateIpAddress_privateDnsName' - The private IPv4 DNS name.
--
-- 'privateIpAddress', 'instancePrivateIpAddress_privateIpAddress' - The private IPv4 address of the network interface.
newInstancePrivateIpAddress ::
  InstancePrivateIpAddress
newInstancePrivateIpAddress =
  InstancePrivateIpAddress'
    { primary = Core.Nothing,
      association = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | Indicates whether this IPv4 address is the primary private IP address of
-- the network interface.
instancePrivateIpAddress_primary :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Bool)
instancePrivateIpAddress_primary = Lens.lens (\InstancePrivateIpAddress' {primary} -> primary) (\s@InstancePrivateIpAddress' {} a -> s {primary = a} :: InstancePrivateIpAddress)

-- | The association information for an Elastic IP address for the network
-- interface.
instancePrivateIpAddress_association :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe InstanceNetworkInterfaceAssociation)
instancePrivateIpAddress_association = Lens.lens (\InstancePrivateIpAddress' {association} -> association) (\s@InstancePrivateIpAddress' {} a -> s {association = a} :: InstancePrivateIpAddress)

-- | The private IPv4 DNS name.
instancePrivateIpAddress_privateDnsName :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Text)
instancePrivateIpAddress_privateDnsName = Lens.lens (\InstancePrivateIpAddress' {privateDnsName} -> privateDnsName) (\s@InstancePrivateIpAddress' {} a -> s {privateDnsName = a} :: InstancePrivateIpAddress)

-- | The private IPv4 address of the network interface.
instancePrivateIpAddress_privateIpAddress :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Text)
instancePrivateIpAddress_privateIpAddress = Lens.lens (\InstancePrivateIpAddress' {privateIpAddress} -> privateIpAddress) (\s@InstancePrivateIpAddress' {} a -> s {privateIpAddress = a} :: InstancePrivateIpAddress)

instance Core.FromXML InstancePrivateIpAddress where
  parseXML x =
    InstancePrivateIpAddress'
      Core.<$> (x Core..@? "primary")
      Core.<*> (x Core..@? "association")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateIpAddress")

instance Core.Hashable InstancePrivateIpAddress

instance Core.NFData InstancePrivateIpAddress

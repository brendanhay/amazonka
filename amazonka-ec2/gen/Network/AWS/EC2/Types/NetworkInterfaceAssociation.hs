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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAssociation where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes association information for an Elastic IP address (IPv4 only),
-- or a Carrier IP address (for a network interface which resides in a
-- subnet in a Wavelength Zone).
--
-- /See:/ 'newNetworkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { -- | The ID of the Elastic IP address owner.
    ipOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The carrier IP address associated with the network interface.
    --
    -- This option is only available when the network interface is in a subnet
    -- which is associated with a Wavelength Zone.
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address associated with the network interface.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The address of the Elastic IP address bound to the network interface.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The allocation ID.
    allocationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipOwnerId', 'networkInterfaceAssociation_ipOwnerId' - The ID of the Elastic IP address owner.
--
-- 'carrierIp', 'networkInterfaceAssociation_carrierIp' - The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet
-- which is associated with a Wavelength Zone.
--
-- 'customerOwnedIp', 'networkInterfaceAssociation_customerOwnedIp' - The customer-owned IP address associated with the network interface.
--
-- 'publicDnsName', 'networkInterfaceAssociation_publicDnsName' - The public DNS name.
--
-- 'associationId', 'networkInterfaceAssociation_associationId' - The association ID.
--
-- 'publicIp', 'networkInterfaceAssociation_publicIp' - The address of the Elastic IP address bound to the network interface.
--
-- 'allocationId', 'networkInterfaceAssociation_allocationId' - The allocation ID.
newNetworkInterfaceAssociation ::
  NetworkInterfaceAssociation
newNetworkInterfaceAssociation =
  NetworkInterfaceAssociation'
    { ipOwnerId =
        Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      associationId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      allocationId = Prelude.Nothing
    }

-- | The ID of the Elastic IP address owner.
networkInterfaceAssociation_ipOwnerId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_ipOwnerId = Lens.lens (\NetworkInterfaceAssociation' {ipOwnerId} -> ipOwnerId) (\s@NetworkInterfaceAssociation' {} a -> s {ipOwnerId = a} :: NetworkInterfaceAssociation)

-- | The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet
-- which is associated with a Wavelength Zone.
networkInterfaceAssociation_carrierIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_carrierIp = Lens.lens (\NetworkInterfaceAssociation' {carrierIp} -> carrierIp) (\s@NetworkInterfaceAssociation' {} a -> s {carrierIp = a} :: NetworkInterfaceAssociation)

-- | The customer-owned IP address associated with the network interface.
networkInterfaceAssociation_customerOwnedIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_customerOwnedIp = Lens.lens (\NetworkInterfaceAssociation' {customerOwnedIp} -> customerOwnedIp) (\s@NetworkInterfaceAssociation' {} a -> s {customerOwnedIp = a} :: NetworkInterfaceAssociation)

-- | The public DNS name.
networkInterfaceAssociation_publicDnsName :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_publicDnsName = Lens.lens (\NetworkInterfaceAssociation' {publicDnsName} -> publicDnsName) (\s@NetworkInterfaceAssociation' {} a -> s {publicDnsName = a} :: NetworkInterfaceAssociation)

-- | The association ID.
networkInterfaceAssociation_associationId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_associationId = Lens.lens (\NetworkInterfaceAssociation' {associationId} -> associationId) (\s@NetworkInterfaceAssociation' {} a -> s {associationId = a} :: NetworkInterfaceAssociation)

-- | The address of the Elastic IP address bound to the network interface.
networkInterfaceAssociation_publicIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_publicIp = Lens.lens (\NetworkInterfaceAssociation' {publicIp} -> publicIp) (\s@NetworkInterfaceAssociation' {} a -> s {publicIp = a} :: NetworkInterfaceAssociation)

-- | The allocation ID.
networkInterfaceAssociation_allocationId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_allocationId = Lens.lens (\NetworkInterfaceAssociation' {allocationId} -> allocationId) (\s@NetworkInterfaceAssociation' {} a -> s {allocationId = a} :: NetworkInterfaceAssociation)

instance Prelude.FromXML NetworkInterfaceAssociation where
  parseXML x =
    NetworkInterfaceAssociation'
      Prelude.<$> (x Prelude..@? "ipOwnerId")
      Prelude.<*> (x Prelude..@? "carrierIp")
      Prelude.<*> (x Prelude..@? "customerOwnedIp")
      Prelude.<*> (x Prelude..@? "publicDnsName")
      Prelude.<*> (x Prelude..@? "associationId")
      Prelude.<*> (x Prelude..@? "publicIp")
      Prelude.<*> (x Prelude..@? "allocationId")

instance Prelude.Hashable NetworkInterfaceAssociation

instance Prelude.NFData NetworkInterfaceAssociation

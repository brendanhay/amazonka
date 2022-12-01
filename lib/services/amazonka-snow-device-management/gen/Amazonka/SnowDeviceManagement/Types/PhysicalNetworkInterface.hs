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
-- Module      : Amazonka.SnowDeviceManagement.Types.PhysicalNetworkInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.PhysicalNetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.IpAddressAssignment
import Amazonka.SnowDeviceManagement.Types.PhysicalConnectorType

-- | The details about the physical network interface for the device.
--
-- /See:/ 'newPhysicalNetworkInterface' smart constructor.
data PhysicalNetworkInterface = PhysicalNetworkInterface'
  { -- | A value that describes whether the IP address is dynamic or persistent.
    ipAddressAssignment :: Prelude.Maybe IpAddressAssignment,
    -- | The netmask used to divide the IP address into subnets.
    netmask :: Prelude.Maybe Prelude.Text,
    -- | The physical connector type.
    physicalConnectorType :: Prelude.Maybe PhysicalConnectorType,
    -- | The MAC address of the device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The default gateway of the device.
    defaultGateway :: Prelude.Maybe Prelude.Text,
    -- | The physical network interface ID.
    physicalNetworkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the device.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressAssignment', 'physicalNetworkInterface_ipAddressAssignment' - A value that describes whether the IP address is dynamic or persistent.
--
-- 'netmask', 'physicalNetworkInterface_netmask' - The netmask used to divide the IP address into subnets.
--
-- 'physicalConnectorType', 'physicalNetworkInterface_physicalConnectorType' - The physical connector type.
--
-- 'macAddress', 'physicalNetworkInterface_macAddress' - The MAC address of the device.
--
-- 'defaultGateway', 'physicalNetworkInterface_defaultGateway' - The default gateway of the device.
--
-- 'physicalNetworkInterfaceId', 'physicalNetworkInterface_physicalNetworkInterfaceId' - The physical network interface ID.
--
-- 'ipAddress', 'physicalNetworkInterface_ipAddress' - The IP address of the device.
newPhysicalNetworkInterface ::
  PhysicalNetworkInterface
newPhysicalNetworkInterface =
  PhysicalNetworkInterface'
    { ipAddressAssignment =
        Prelude.Nothing,
      netmask = Prelude.Nothing,
      physicalConnectorType = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      defaultGateway = Prelude.Nothing,
      physicalNetworkInterfaceId = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | A value that describes whether the IP address is dynamic or persistent.
physicalNetworkInterface_ipAddressAssignment :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe IpAddressAssignment)
physicalNetworkInterface_ipAddressAssignment = Lens.lens (\PhysicalNetworkInterface' {ipAddressAssignment} -> ipAddressAssignment) (\s@PhysicalNetworkInterface' {} a -> s {ipAddressAssignment = a} :: PhysicalNetworkInterface)

-- | The netmask used to divide the IP address into subnets.
physicalNetworkInterface_netmask :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe Prelude.Text)
physicalNetworkInterface_netmask = Lens.lens (\PhysicalNetworkInterface' {netmask} -> netmask) (\s@PhysicalNetworkInterface' {} a -> s {netmask = a} :: PhysicalNetworkInterface)

-- | The physical connector type.
physicalNetworkInterface_physicalConnectorType :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe PhysicalConnectorType)
physicalNetworkInterface_physicalConnectorType = Lens.lens (\PhysicalNetworkInterface' {physicalConnectorType} -> physicalConnectorType) (\s@PhysicalNetworkInterface' {} a -> s {physicalConnectorType = a} :: PhysicalNetworkInterface)

-- | The MAC address of the device.
physicalNetworkInterface_macAddress :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe Prelude.Text)
physicalNetworkInterface_macAddress = Lens.lens (\PhysicalNetworkInterface' {macAddress} -> macAddress) (\s@PhysicalNetworkInterface' {} a -> s {macAddress = a} :: PhysicalNetworkInterface)

-- | The default gateway of the device.
physicalNetworkInterface_defaultGateway :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe Prelude.Text)
physicalNetworkInterface_defaultGateway = Lens.lens (\PhysicalNetworkInterface' {defaultGateway} -> defaultGateway) (\s@PhysicalNetworkInterface' {} a -> s {defaultGateway = a} :: PhysicalNetworkInterface)

-- | The physical network interface ID.
physicalNetworkInterface_physicalNetworkInterfaceId :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe Prelude.Text)
physicalNetworkInterface_physicalNetworkInterfaceId = Lens.lens (\PhysicalNetworkInterface' {physicalNetworkInterfaceId} -> physicalNetworkInterfaceId) (\s@PhysicalNetworkInterface' {} a -> s {physicalNetworkInterfaceId = a} :: PhysicalNetworkInterface)

-- | The IP address of the device.
physicalNetworkInterface_ipAddress :: Lens.Lens' PhysicalNetworkInterface (Prelude.Maybe Prelude.Text)
physicalNetworkInterface_ipAddress = Lens.lens (\PhysicalNetworkInterface' {ipAddress} -> ipAddress) (\s@PhysicalNetworkInterface' {} a -> s {ipAddress = a} :: PhysicalNetworkInterface)

instance Core.FromJSON PhysicalNetworkInterface where
  parseJSON =
    Core.withObject
      "PhysicalNetworkInterface"
      ( \x ->
          PhysicalNetworkInterface'
            Prelude.<$> (x Core..:? "ipAddressAssignment")
            Prelude.<*> (x Core..:? "netmask")
            Prelude.<*> (x Core..:? "physicalConnectorType")
            Prelude.<*> (x Core..:? "macAddress")
            Prelude.<*> (x Core..:? "defaultGateway")
            Prelude.<*> (x Core..:? "physicalNetworkInterfaceId")
            Prelude.<*> (x Core..:? "ipAddress")
      )

instance Prelude.Hashable PhysicalNetworkInterface where
  hashWithSalt _salt PhysicalNetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` ipAddressAssignment
      `Prelude.hashWithSalt` netmask
      `Prelude.hashWithSalt` physicalConnectorType
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` defaultGateway
      `Prelude.hashWithSalt` physicalNetworkInterfaceId
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData PhysicalNetworkInterface where
  rnf PhysicalNetworkInterface' {..} =
    Prelude.rnf ipAddressAssignment
      `Prelude.seq` Prelude.rnf netmask
      `Prelude.seq` Prelude.rnf physicalConnectorType
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf defaultGateway
      `Prelude.seq` Prelude.rnf physicalNetworkInterfaceId
      `Prelude.seq` Prelude.rnf ipAddress

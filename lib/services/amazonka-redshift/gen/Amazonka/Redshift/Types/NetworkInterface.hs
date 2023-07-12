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
-- Module      : Amazonka.Redshift.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The network interface identifier.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The subnet identifier.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'networkInterface_availabilityZone' - The Availability Zone.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The network interface identifier.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
--
-- 'subnetId', 'networkInterface_subnetId' - The subnet identifier.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { availabilityZone =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The Availability Zone.
networkInterface_availabilityZone :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_availabilityZone = Lens.lens (\NetworkInterface' {availabilityZone} -> availabilityZone) (\s@NetworkInterface' {} a -> s {availabilityZone = a} :: NetworkInterface)

-- | The network interface identifier.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

-- | The subnet identifier.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

instance Data.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
      Prelude.<$> (x Data..@? "AvailabilityZone")
      Prelude.<*> (x Data..@? "NetworkInterfaceId")
      Prelude.<*> (x Data..@? "PrivateIpAddress")
      Prelude.<*> (x Data..@? "SubnetId")

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf subnetId

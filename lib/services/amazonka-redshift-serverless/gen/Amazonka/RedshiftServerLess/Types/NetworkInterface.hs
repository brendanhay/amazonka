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
-- Module      : Amazonka.RedshiftServerLess.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a network interface in an Amazon Redshift
-- Serverless managed VPC endpoint.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the subnet.
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
-- 'availabilityZone', 'networkInterface_availabilityZone' - The availability Zone.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The unique identifier of the network interface.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
--
-- 'subnetId', 'networkInterface_subnetId' - The unique identifier of the subnet.
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

-- | The availability Zone.
networkInterface_availabilityZone :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_availabilityZone = Lens.lens (\NetworkInterface' {availabilityZone} -> availabilityZone) (\s@NetworkInterface' {} a -> s {availabilityZone = a} :: NetworkInterface)

-- | The unique identifier of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

-- | The unique identifier of the subnet.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "availabilityZone")
            Prelude.<*> (x Data..:? "networkInterfaceId")
            Prelude.<*> (x Data..:? "privateIpAddress")
            Prelude.<*> (x Data..:? "subnetId")
      )

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

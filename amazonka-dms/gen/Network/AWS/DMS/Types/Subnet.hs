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
-- Module      : Network.AWS.DMS.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Subnet where

import Network.AWS.DMS.Types.AvailabilityZone
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | In response to a request by the @DescribeReplicationSubnetGroups@
-- operation, this object identifies a subnet by its given Availability
-- Zone, subnet identifier, and status.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The status of the subnet.
    subnetStatus :: Prelude.Maybe Prelude.Text,
    -- | The subnet identifier.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the subnet.
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetStatus', 'subnet_subnetStatus' - The status of the subnet.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The subnet identifier.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - The Availability Zone of the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetStatus = Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing
    }

-- | The status of the subnet.
subnet_subnetStatus :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetStatus = Lens.lens (\Subnet' {subnetStatus} -> subnetStatus) (\s@Subnet' {} a -> s {subnetStatus = a} :: Subnet)

-- | The subnet identifier.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | The Availability Zone of the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

instance Prelude.FromJSON Subnet where
  parseJSON =
    Prelude.withObject
      "Subnet"
      ( \x ->
          Subnet'
            Prelude.<$> (x Prelude..:? "SubnetStatus")
            Prelude.<*> (x Prelude..:? "SubnetIdentifier")
            Prelude.<*> (x Prelude..:? "SubnetAvailabilityZone")
      )

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet

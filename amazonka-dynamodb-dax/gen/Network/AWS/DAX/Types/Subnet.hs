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
-- Module      : Network.AWS.DAX.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Subnet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the subnet associated with a DAX cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC)
-- and used with DAX.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The system-assigned identifier for the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) for the subnet.
    subnetAvailabilityZone :: Prelude.Maybe Prelude.Text
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
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The system-assigned identifier for the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - The Availability Zone (AZ) for the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing
    }

-- | The system-assigned identifier for the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | The Availability Zone (AZ) for the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

instance Prelude.FromJSON Subnet where
  parseJSON =
    Prelude.withObject
      "Subnet"
      ( \x ->
          Subnet'
            Prelude.<$> (x Prelude..:? "SubnetIdentifier")
            Prelude.<*> (x Prelude..:? "SubnetAvailabilityZone")
      )

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet

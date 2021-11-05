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
-- Module      : Amazonka.DocumentDB.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.Subnet where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types.AvailabilityZone
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a subnet.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | Specifies the status of the subnet.
    subnetStatus :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier of the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Availability Zone for the subnet.
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetStatus', 'subnet_subnetStatus' - Specifies the status of the subnet.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - Specifies the identifier of the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - Specifies the Availability Zone for the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetStatus = Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing
    }

-- | Specifies the status of the subnet.
subnet_subnetStatus :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetStatus = Lens.lens (\Subnet' {subnetStatus} -> subnetStatus) (\s@Subnet' {} a -> s {subnetStatus = a} :: Subnet)

-- | Specifies the identifier of the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | Specifies the Availability Zone for the subnet.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Core..@? "SubnetStatus")
      Prelude.<*> (x Core..@? "SubnetIdentifier")
      Prelude.<*> (x Core..@? "SubnetAvailabilityZone")

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet

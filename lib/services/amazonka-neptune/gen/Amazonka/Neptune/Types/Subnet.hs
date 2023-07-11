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
-- Module      : Amazonka.Neptune.Types.Subnet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.Subnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.AvailabilityZone
import qualified Amazonka.Prelude as Prelude

-- | Specifies a subnet.
--
-- This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | Specifies the EC2 Availability Zone that the subnet is in.
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone,
    -- | Specifies the identifier of the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of the subnet.
    subnetStatus :: Prelude.Maybe Prelude.Text
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
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - Specifies the EC2 Availability Zone that the subnet is in.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - Specifies the identifier of the subnet.
--
-- 'subnetStatus', 'subnet_subnetStatus' - Specifies the status of the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetAvailabilityZone = Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetStatus = Prelude.Nothing
    }

-- | Specifies the EC2 Availability Zone that the subnet is in.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

-- | Specifies the identifier of the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | Specifies the status of the subnet.
subnet_subnetStatus :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetStatus = Lens.lens (\Subnet' {subnetStatus} -> subnetStatus) (\s@Subnet' {} a -> s {subnetStatus = a} :: Subnet)

instance Data.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Data..@? "SubnetAvailabilityZone")
      Prelude.<*> (x Data..@? "SubnetIdentifier")
      Prelude.<*> (x Data..@? "SubnetStatus")

instance Prelude.Hashable Subnet where
  hashWithSalt _salt Subnet' {..} =
    _salt
      `Prelude.hashWithSalt` subnetAvailabilityZone
      `Prelude.hashWithSalt` subnetIdentifier
      `Prelude.hashWithSalt` subnetStatus

instance Prelude.NFData Subnet where
  rnf Subnet' {..} =
    Prelude.rnf subnetAvailabilityZone
      `Prelude.seq` Prelude.rnf subnetIdentifier
      `Prelude.seq` Prelude.rnf subnetStatus

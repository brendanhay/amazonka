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
-- Module      : Network.AWS.MemoryDb.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.Subnet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MemoryDb.Types.AvailabilityZone
import qualified Network.AWS.Prelude as Prelude

-- | Represents the subnet associated with a cluster. This parameter refers
-- to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used
-- with MemoryDB.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The unique identifier for the subnet.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone where the subnet resides
    availabilityZone :: Prelude.Maybe AvailabilityZone
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
-- 'identifier', 'subnet_identifier' - The unique identifier for the subnet.
--
-- 'availabilityZone', 'subnet_availabilityZone' - The Availability Zone where the subnet resides
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { identifier = Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The unique identifier for the subnet.
subnet_identifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_identifier = Lens.lens (\Subnet' {identifier} -> identifier) (\s@Subnet' {} a -> s {identifier = a} :: Subnet)

-- | The Availability Zone where the subnet resides
subnet_availabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_availabilityZone = Lens.lens (\Subnet' {availabilityZone} -> availabilityZone) (\s@Subnet' {} a -> s {availabilityZone = a} :: Subnet)

instance Core.FromJSON Subnet where
  parseJSON =
    Core.withObject
      "Subnet"
      ( \x ->
          Subnet'
            Prelude.<$> (x Core..:? "Identifier")
            Prelude.<*> (x Core..:? "AvailabilityZone")
      )

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet

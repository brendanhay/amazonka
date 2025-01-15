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
-- Module      : Amazonka.MemoryDb.Types.Subnet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Subnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.AvailabilityZone
import qualified Amazonka.Prelude as Prelude

-- | Represents the subnet associated with a cluster. This parameter refers
-- to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used
-- with MemoryDB.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The Availability Zone where the subnet resides
    availabilityZone :: Prelude.Maybe AvailabilityZone,
    -- | The unique identifier for the subnet.
    identifier :: Prelude.Maybe Prelude.Text
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
-- 'availabilityZone', 'subnet_availabilityZone' - The Availability Zone where the subnet resides
--
-- 'identifier', 'subnet_identifier' - The unique identifier for the subnet.
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { availabilityZone = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | The Availability Zone where the subnet resides
subnet_availabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_availabilityZone = Lens.lens (\Subnet' {availabilityZone} -> availabilityZone) (\s@Subnet' {} a -> s {availabilityZone = a} :: Subnet)

-- | The unique identifier for the subnet.
subnet_identifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_identifier = Lens.lens (\Subnet' {identifier} -> identifier) (\s@Subnet' {} a -> s {identifier = a} :: Subnet)

instance Data.FromJSON Subnet where
  parseJSON =
    Data.withObject
      "Subnet"
      ( \x ->
          Subnet'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "Identifier")
      )

instance Prelude.Hashable Subnet where
  hashWithSalt _salt Subnet' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData Subnet where
  rnf Subnet' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf identifier

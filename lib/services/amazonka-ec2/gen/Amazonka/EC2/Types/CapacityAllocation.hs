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
-- Module      : Amazonka.EC2.Types.CapacityAllocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityAllocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AllocationType
import qualified Amazonka.Prelude as Prelude

-- | Information about instance capacity usage for a Capacity Reservation.
--
-- /See:/ 'newCapacityAllocation' smart constructor.
data CapacityAllocation = CapacityAllocation'
  { -- | The usage type. @used@ indicates that the instance capacity is in use by
    -- instances that are running in the Capacity Reservation.
    allocationType :: Prelude.Maybe AllocationType,
    -- | The amount of instance capacity associated with the usage. For example a
    -- value of @4@ indicates that instance capacity for 4 instances is
    -- currently in use.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationType', 'capacityAllocation_allocationType' - The usage type. @used@ indicates that the instance capacity is in use by
-- instances that are running in the Capacity Reservation.
--
-- 'count', 'capacityAllocation_count' - The amount of instance capacity associated with the usage. For example a
-- value of @4@ indicates that instance capacity for 4 instances is
-- currently in use.
newCapacityAllocation ::
  CapacityAllocation
newCapacityAllocation =
  CapacityAllocation'
    { allocationType =
        Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The usage type. @used@ indicates that the instance capacity is in use by
-- instances that are running in the Capacity Reservation.
capacityAllocation_allocationType :: Lens.Lens' CapacityAllocation (Prelude.Maybe AllocationType)
capacityAllocation_allocationType = Lens.lens (\CapacityAllocation' {allocationType} -> allocationType) (\s@CapacityAllocation' {} a -> s {allocationType = a} :: CapacityAllocation)

-- | The amount of instance capacity associated with the usage. For example a
-- value of @4@ indicates that instance capacity for 4 instances is
-- currently in use.
capacityAllocation_count :: Lens.Lens' CapacityAllocation (Prelude.Maybe Prelude.Int)
capacityAllocation_count = Lens.lens (\CapacityAllocation' {count} -> count) (\s@CapacityAllocation' {} a -> s {count = a} :: CapacityAllocation)

instance Data.FromXML CapacityAllocation where
  parseXML x =
    CapacityAllocation'
      Prelude.<$> (x Data..@? "allocationType")
      Prelude.<*> (x Data..@? "count")

instance Prelude.Hashable CapacityAllocation where
  hashWithSalt _salt CapacityAllocation' {..} =
    _salt
      `Prelude.hashWithSalt` allocationType
      `Prelude.hashWithSalt` count

instance Prelude.NFData CapacityAllocation where
  rnf CapacityAllocation' {..} =
    Prelude.rnf allocationType `Prelude.seq`
      Prelude.rnf count

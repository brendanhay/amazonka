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
-- Module      : Amazonka.Athena.Types.CapacityAssignmentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CapacityAssignmentConfiguration where

import Amazonka.Athena.Types.CapacityAssignment
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Assigns Athena workgroups (and hence their queries) to capacity
-- reservations. A capacity reservation can have only one capacity
-- assignment configuration, but the capacity assignment configuration can
-- be made up of multiple individual assignments. Each assignment specifies
-- how Athena queries can consume capacity from the capacity reservation
-- that their workgroup is mapped to.
--
-- /See:/ 'newCapacityAssignmentConfiguration' smart constructor.
data CapacityAssignmentConfiguration = CapacityAssignmentConfiguration'
  { -- | The list of assignments that make up the capacity assignment
    -- configuration.
    capacityAssignments :: Prelude.Maybe [CapacityAssignment],
    -- | The name of the reservation that the capacity assignment configuration
    -- is for.
    capacityReservationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityAssignmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityAssignments', 'capacityAssignmentConfiguration_capacityAssignments' - The list of assignments that make up the capacity assignment
-- configuration.
--
-- 'capacityReservationName', 'capacityAssignmentConfiguration_capacityReservationName' - The name of the reservation that the capacity assignment configuration
-- is for.
newCapacityAssignmentConfiguration ::
  CapacityAssignmentConfiguration
newCapacityAssignmentConfiguration =
  CapacityAssignmentConfiguration'
    { capacityAssignments =
        Prelude.Nothing,
      capacityReservationName = Prelude.Nothing
    }

-- | The list of assignments that make up the capacity assignment
-- configuration.
capacityAssignmentConfiguration_capacityAssignments :: Lens.Lens' CapacityAssignmentConfiguration (Prelude.Maybe [CapacityAssignment])
capacityAssignmentConfiguration_capacityAssignments = Lens.lens (\CapacityAssignmentConfiguration' {capacityAssignments} -> capacityAssignments) (\s@CapacityAssignmentConfiguration' {} a -> s {capacityAssignments = a} :: CapacityAssignmentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the reservation that the capacity assignment configuration
-- is for.
capacityAssignmentConfiguration_capacityReservationName :: Lens.Lens' CapacityAssignmentConfiguration (Prelude.Maybe Prelude.Text)
capacityAssignmentConfiguration_capacityReservationName = Lens.lens (\CapacityAssignmentConfiguration' {capacityReservationName} -> capacityReservationName) (\s@CapacityAssignmentConfiguration' {} a -> s {capacityReservationName = a} :: CapacityAssignmentConfiguration)

instance
  Data.FromJSON
    CapacityAssignmentConfiguration
  where
  parseJSON =
    Data.withObject
      "CapacityAssignmentConfiguration"
      ( \x ->
          CapacityAssignmentConfiguration'
            Prelude.<$> ( x
                            Data..:? "CapacityAssignments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CapacityReservationName")
      )

instance
  Prelude.Hashable
    CapacityAssignmentConfiguration
  where
  hashWithSalt
    _salt
    CapacityAssignmentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` capacityAssignments
        `Prelude.hashWithSalt` capacityReservationName

instance
  Prelude.NFData
    CapacityAssignmentConfiguration
  where
  rnf CapacityAssignmentConfiguration' {..} =
    Prelude.rnf capacityAssignments
      `Prelude.seq` Prelude.rnf capacityReservationName

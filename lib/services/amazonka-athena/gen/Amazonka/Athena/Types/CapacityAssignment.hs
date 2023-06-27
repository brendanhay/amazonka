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
-- Module      : Amazonka.Athena.Types.CapacityAssignment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CapacityAssignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A mapping between one or more workgroups and a capacity reservation.
--
-- /See:/ 'newCapacityAssignment' smart constructor.
data CapacityAssignment = CapacityAssignment'
  { -- | The list of workgroup names for the capacity assignment.
    workGroupNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workGroupNames', 'capacityAssignment_workGroupNames' - The list of workgroup names for the capacity assignment.
newCapacityAssignment ::
  CapacityAssignment
newCapacityAssignment =
  CapacityAssignment'
    { workGroupNames =
        Prelude.Nothing
    }

-- | The list of workgroup names for the capacity assignment.
capacityAssignment_workGroupNames :: Lens.Lens' CapacityAssignment (Prelude.Maybe [Prelude.Text])
capacityAssignment_workGroupNames = Lens.lens (\CapacityAssignment' {workGroupNames} -> workGroupNames) (\s@CapacityAssignment' {} a -> s {workGroupNames = a} :: CapacityAssignment) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CapacityAssignment where
  parseJSON =
    Data.withObject
      "CapacityAssignment"
      ( \x ->
          CapacityAssignment'
            Prelude.<$> ( x
                            Data..:? "WorkGroupNames"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CapacityAssignment where
  hashWithSalt _salt CapacityAssignment' {..} =
    _salt `Prelude.hashWithSalt` workGroupNames

instance Prelude.NFData CapacityAssignment where
  rnf CapacityAssignment' {..} =
    Prelude.rnf workGroupNames

instance Data.ToJSON CapacityAssignment where
  toJSON CapacityAssignment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WorkGroupNames" Data..=)
              Prelude.<$> workGroupNames
          ]
      )

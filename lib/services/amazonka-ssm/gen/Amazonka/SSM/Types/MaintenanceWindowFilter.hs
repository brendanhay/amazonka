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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filter used in the request. Supported filter keys depend on the API
-- operation that includes the filter. API operations that use
-- @MaintenanceWindowFilter>@ include the following:
--
-- -   DescribeMaintenanceWindowExecutions
--
-- -   DescribeMaintenanceWindowExecutionTaskInvocations
--
-- -   DescribeMaintenanceWindowExecutionTasks
--
-- -   DescribeMaintenanceWindows
--
-- -   DescribeMaintenanceWindowTargets
--
-- -   DescribeMaintenanceWindowTasks
--
-- /See:/ 'newMaintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'maintenanceWindowFilter_values' - The filter values.
--
-- 'key', 'maintenanceWindowFilter_key' - The name of the filter.
newMaintenanceWindowFilter ::
  MaintenanceWindowFilter
newMaintenanceWindowFilter =
  MaintenanceWindowFilter'
    { values = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The filter values.
maintenanceWindowFilter_values :: Lens.Lens' MaintenanceWindowFilter (Prelude.Maybe [Prelude.Text])
maintenanceWindowFilter_values = Lens.lens (\MaintenanceWindowFilter' {values} -> values) (\s@MaintenanceWindowFilter' {} a -> s {values = a} :: MaintenanceWindowFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter.
maintenanceWindowFilter_key :: Lens.Lens' MaintenanceWindowFilter (Prelude.Maybe Prelude.Text)
maintenanceWindowFilter_key = Lens.lens (\MaintenanceWindowFilter' {key} -> key) (\s@MaintenanceWindowFilter' {} a -> s {key = a} :: MaintenanceWindowFilter)

instance Prelude.Hashable MaintenanceWindowFilter where
  hashWithSalt _salt MaintenanceWindowFilter' {..} =
    _salt `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` key

instance Prelude.NFData MaintenanceWindowFilter where
  rnf MaintenanceWindowFilter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf key

instance Core.ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Key" Core..=) Prelude.<$> key
          ]
      )

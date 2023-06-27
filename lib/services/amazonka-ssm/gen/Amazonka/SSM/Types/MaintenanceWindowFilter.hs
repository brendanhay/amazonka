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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The name of the filter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'key', 'maintenanceWindowFilter_key' - The name of the filter.
--
-- 'values', 'maintenanceWindowFilter_values' - The filter values.
newMaintenanceWindowFilter ::
  MaintenanceWindowFilter
newMaintenanceWindowFilter =
  MaintenanceWindowFilter'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
maintenanceWindowFilter_key :: Lens.Lens' MaintenanceWindowFilter (Prelude.Maybe Prelude.Text)
maintenanceWindowFilter_key = Lens.lens (\MaintenanceWindowFilter' {key} -> key) (\s@MaintenanceWindowFilter' {} a -> s {key = a} :: MaintenanceWindowFilter)

-- | The filter values.
maintenanceWindowFilter_values :: Lens.Lens' MaintenanceWindowFilter (Prelude.Maybe [Prelude.Text])
maintenanceWindowFilter_values = Lens.lens (\MaintenanceWindowFilter' {values} -> values) (\s@MaintenanceWindowFilter' {} a -> s {values = a} :: MaintenanceWindowFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable MaintenanceWindowFilter where
  hashWithSalt _salt MaintenanceWindowFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData MaintenanceWindowFilter where
  rnf MaintenanceWindowFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Values" Data..=) Prelude.<$> values
          ]
      )

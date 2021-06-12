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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Filter used in the request. Supported filter keys are Name and Enabled.
--
-- /See:/ 'newMaintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { -- | The name of the filter.
    key :: Core.Maybe Core.Text,
    -- | The filter values.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the filter.
maintenanceWindowFilter_key :: Lens.Lens' MaintenanceWindowFilter (Core.Maybe Core.Text)
maintenanceWindowFilter_key = Lens.lens (\MaintenanceWindowFilter' {key} -> key) (\s@MaintenanceWindowFilter' {} a -> s {key = a} :: MaintenanceWindowFilter)

-- | The filter values.
maintenanceWindowFilter_values :: Lens.Lens' MaintenanceWindowFilter (Core.Maybe [Core.Text])
maintenanceWindowFilter_values = Lens.lens (\MaintenanceWindowFilter' {values} -> values) (\s@MaintenanceWindowFilter' {} a -> s {values = a} :: MaintenanceWindowFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable MaintenanceWindowFilter

instance Core.NFData MaintenanceWindowFilter

instance Core.ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values
          ]
      )

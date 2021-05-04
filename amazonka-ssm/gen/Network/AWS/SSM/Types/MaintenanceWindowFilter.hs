{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filter used in the request. Supported filter keys are Name and Enabled.
--
-- /See:/ 'newMaintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { -- | The name of the filter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
maintenanceWindowFilter_values = Lens.lens (\MaintenanceWindowFilter' {values} -> values) (\s@MaintenanceWindowFilter' {} a -> s {values = a} :: MaintenanceWindowFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable MaintenanceWindowFilter

instance Prelude.NFData MaintenanceWindowFilter

instance Prelude.ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values
          ]
      )

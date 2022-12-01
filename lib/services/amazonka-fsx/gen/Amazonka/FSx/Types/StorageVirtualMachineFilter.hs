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
-- Module      : Amazonka.FSx.Types.StorageVirtualMachineFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.StorageVirtualMachineFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.StorageVirtualMachineFilterName
import qualified Amazonka.Prelude as Prelude

-- | A filter used to restrict the results of describe calls for Amazon FSx
-- for NetApp ONTAP storage virtual machines (SVMs). You can use multiple
-- filters to return results that meet all applied filter requirements.
--
-- /See:/ 'newStorageVirtualMachineFilter' smart constructor.
data StorageVirtualMachineFilter = StorageVirtualMachineFilter'
  { -- | The name for this filter.
    name :: Prelude.Maybe StorageVirtualMachineFilterName,
    -- | The values of the filter. These are all the values for any of the
    -- applied filters.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageVirtualMachineFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'storageVirtualMachineFilter_name' - The name for this filter.
--
-- 'values', 'storageVirtualMachineFilter_values' - The values of the filter. These are all the values for any of the
-- applied filters.
newStorageVirtualMachineFilter ::
  StorageVirtualMachineFilter
newStorageVirtualMachineFilter =
  StorageVirtualMachineFilter'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name for this filter.
storageVirtualMachineFilter_name :: Lens.Lens' StorageVirtualMachineFilter (Prelude.Maybe StorageVirtualMachineFilterName)
storageVirtualMachineFilter_name = Lens.lens (\StorageVirtualMachineFilter' {name} -> name) (\s@StorageVirtualMachineFilter' {} a -> s {name = a} :: StorageVirtualMachineFilter)

-- | The values of the filter. These are all the values for any of the
-- applied filters.
storageVirtualMachineFilter_values :: Lens.Lens' StorageVirtualMachineFilter (Prelude.Maybe [Prelude.Text])
storageVirtualMachineFilter_values = Lens.lens (\StorageVirtualMachineFilter' {values} -> values) (\s@StorageVirtualMachineFilter' {} a -> s {values = a} :: StorageVirtualMachineFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable StorageVirtualMachineFilter where
  hashWithSalt _salt StorageVirtualMachineFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData StorageVirtualMachineFilter where
  rnf StorageVirtualMachineFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON StorageVirtualMachineFilter where
  toJSON StorageVirtualMachineFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Values" Core..=) Prelude.<$> values
          ]
      )

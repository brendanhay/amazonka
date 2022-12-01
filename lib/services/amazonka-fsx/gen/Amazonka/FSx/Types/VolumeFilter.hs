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
-- Module      : Amazonka.FSx.Types.VolumeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.VolumeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.VolumeFilterName
import qualified Amazonka.Prelude as Prelude

-- | A filter used to restrict the results of describe calls for Amazon FSx
-- for NetApp ONTAP or Amazon FSx for OpenZFS volumes. You can use multiple
-- filters to return results that meet all applied filter requirements.
--
-- /See:/ 'newVolumeFilter' smart constructor.
data VolumeFilter = VolumeFilter'
  { -- | The name for this filter.
    name :: Prelude.Maybe VolumeFilterName,
    -- | The values of the filter. These are all the values for any of the
    -- applied filters.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'volumeFilter_name' - The name for this filter.
--
-- 'values', 'volumeFilter_values' - The values of the filter. These are all the values for any of the
-- applied filters.
newVolumeFilter ::
  VolumeFilter
newVolumeFilter =
  VolumeFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name for this filter.
volumeFilter_name :: Lens.Lens' VolumeFilter (Prelude.Maybe VolumeFilterName)
volumeFilter_name = Lens.lens (\VolumeFilter' {name} -> name) (\s@VolumeFilter' {} a -> s {name = a} :: VolumeFilter)

-- | The values of the filter. These are all the values for any of the
-- applied filters.
volumeFilter_values :: Lens.Lens' VolumeFilter (Prelude.Maybe [Prelude.Text])
volumeFilter_values = Lens.lens (\VolumeFilter' {values} -> values) (\s@VolumeFilter' {} a -> s {values = a} :: VolumeFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable VolumeFilter where
  hashWithSalt _salt VolumeFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData VolumeFilter where
  rnf VolumeFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON VolumeFilter where
  toJSON VolumeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Values" Core..=) Prelude.<$> values
          ]
      )

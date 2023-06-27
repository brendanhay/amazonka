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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanFindingsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanFindingsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A name value pair that Image Builder applies to streamline results from
-- the vulnerability scan findings list action.
--
-- /See:/ 'newImageScanFindingsFilter' smart constructor.
data ImageScanFindingsFilter = ImageScanFindingsFilter'
  { -- | The name of the image scan finding filter. Filter names are
    -- case-sensitive.
    name :: Prelude.Maybe Prelude.Text,
    -- | The filter values. Filter values are case-sensitive.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFindingsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'imageScanFindingsFilter_name' - The name of the image scan finding filter. Filter names are
-- case-sensitive.
--
-- 'values', 'imageScanFindingsFilter_values' - The filter values. Filter values are case-sensitive.
newImageScanFindingsFilter ::
  ImageScanFindingsFilter
newImageScanFindingsFilter =
  ImageScanFindingsFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the image scan finding filter. Filter names are
-- case-sensitive.
imageScanFindingsFilter_name :: Lens.Lens' ImageScanFindingsFilter (Prelude.Maybe Prelude.Text)
imageScanFindingsFilter_name = Lens.lens (\ImageScanFindingsFilter' {name} -> name) (\s@ImageScanFindingsFilter' {} a -> s {name = a} :: ImageScanFindingsFilter)

-- | The filter values. Filter values are case-sensitive.
imageScanFindingsFilter_values :: Lens.Lens' ImageScanFindingsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
imageScanFindingsFilter_values = Lens.lens (\ImageScanFindingsFilter' {values} -> values) (\s@ImageScanFindingsFilter' {} a -> s {values = a} :: ImageScanFindingsFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ImageScanFindingsFilter where
  hashWithSalt _salt ImageScanFindingsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData ImageScanFindingsFilter where
  rnf ImageScanFindingsFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ImageScanFindingsFilter where
  toJSON ImageScanFindingsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )

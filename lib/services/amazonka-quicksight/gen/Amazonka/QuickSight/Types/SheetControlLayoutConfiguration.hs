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
-- Module      : Amazonka.QuickSight.Types.SheetControlLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetControlLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GridLayoutConfiguration

-- | The configuration that determines the elements and canvas size options
-- of sheet control.
--
-- /See:/ 'newSheetControlLayoutConfiguration' smart constructor.
data SheetControlLayoutConfiguration = SheetControlLayoutConfiguration'
  { -- | The configuration that determines the elements and canvas size options
    -- of sheet control.
    gridLayout :: Prelude.Maybe GridLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetControlLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gridLayout', 'sheetControlLayoutConfiguration_gridLayout' - The configuration that determines the elements and canvas size options
-- of sheet control.
newSheetControlLayoutConfiguration ::
  SheetControlLayoutConfiguration
newSheetControlLayoutConfiguration =
  SheetControlLayoutConfiguration'
    { gridLayout =
        Prelude.Nothing
    }

-- | The configuration that determines the elements and canvas size options
-- of sheet control.
sheetControlLayoutConfiguration_gridLayout :: Lens.Lens' SheetControlLayoutConfiguration (Prelude.Maybe GridLayoutConfiguration)
sheetControlLayoutConfiguration_gridLayout = Lens.lens (\SheetControlLayoutConfiguration' {gridLayout} -> gridLayout) (\s@SheetControlLayoutConfiguration' {} a -> s {gridLayout = a} :: SheetControlLayoutConfiguration)

instance
  Data.FromJSON
    SheetControlLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "SheetControlLayoutConfiguration"
      ( \x ->
          SheetControlLayoutConfiguration'
            Prelude.<$> (x Data..:? "GridLayout")
      )

instance
  Prelude.Hashable
    SheetControlLayoutConfiguration
  where
  hashWithSalt
    _salt
    SheetControlLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` gridLayout

instance
  Prelude.NFData
    SheetControlLayoutConfiguration
  where
  rnf SheetControlLayoutConfiguration' {..} =
    Prelude.rnf gridLayout

instance Data.ToJSON SheetControlLayoutConfiguration where
  toJSON SheetControlLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("GridLayout" Data..=) Prelude.<$> gridLayout]
      )

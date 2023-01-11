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
-- Module      : Amazonka.QuickSight.Types.GlobalTableBorderOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GlobalTableBorderOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableBorderOptions
import Amazonka.QuickSight.Types.TableSideBorderOptions

-- | Determines the border options for a table visual.
--
-- /See:/ 'newGlobalTableBorderOptions' smart constructor.
data GlobalTableBorderOptions = GlobalTableBorderOptions'
  { -- | Determines the options for side specific border.
    sideSpecificBorder :: Prelude.Maybe TableSideBorderOptions,
    -- | Determines the options for uniform border.
    uniformBorder :: Prelude.Maybe TableBorderOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalTableBorderOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sideSpecificBorder', 'globalTableBorderOptions_sideSpecificBorder' - Determines the options for side specific border.
--
-- 'uniformBorder', 'globalTableBorderOptions_uniformBorder' - Determines the options for uniform border.
newGlobalTableBorderOptions ::
  GlobalTableBorderOptions
newGlobalTableBorderOptions =
  GlobalTableBorderOptions'
    { sideSpecificBorder =
        Prelude.Nothing,
      uniformBorder = Prelude.Nothing
    }

-- | Determines the options for side specific border.
globalTableBorderOptions_sideSpecificBorder :: Lens.Lens' GlobalTableBorderOptions (Prelude.Maybe TableSideBorderOptions)
globalTableBorderOptions_sideSpecificBorder = Lens.lens (\GlobalTableBorderOptions' {sideSpecificBorder} -> sideSpecificBorder) (\s@GlobalTableBorderOptions' {} a -> s {sideSpecificBorder = a} :: GlobalTableBorderOptions)

-- | Determines the options for uniform border.
globalTableBorderOptions_uniformBorder :: Lens.Lens' GlobalTableBorderOptions (Prelude.Maybe TableBorderOptions)
globalTableBorderOptions_uniformBorder = Lens.lens (\GlobalTableBorderOptions' {uniformBorder} -> uniformBorder) (\s@GlobalTableBorderOptions' {} a -> s {uniformBorder = a} :: GlobalTableBorderOptions)

instance Data.FromJSON GlobalTableBorderOptions where
  parseJSON =
    Data.withObject
      "GlobalTableBorderOptions"
      ( \x ->
          GlobalTableBorderOptions'
            Prelude.<$> (x Data..:? "SideSpecificBorder")
            Prelude.<*> (x Data..:? "UniformBorder")
      )

instance Prelude.Hashable GlobalTableBorderOptions where
  hashWithSalt _salt GlobalTableBorderOptions' {..} =
    _salt `Prelude.hashWithSalt` sideSpecificBorder
      `Prelude.hashWithSalt` uniformBorder

instance Prelude.NFData GlobalTableBorderOptions where
  rnf GlobalTableBorderOptions' {..} =
    Prelude.rnf sideSpecificBorder
      `Prelude.seq` Prelude.rnf uniformBorder

instance Data.ToJSON GlobalTableBorderOptions where
  toJSON GlobalTableBorderOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SideSpecificBorder" Data..=)
              Prelude.<$> sideSpecificBorder,
            ("UniformBorder" Data..=) Prelude.<$> uniformBorder
          ]
      )

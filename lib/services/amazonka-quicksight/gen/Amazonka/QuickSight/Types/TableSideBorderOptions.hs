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
-- Module      : Amazonka.QuickSight.Types.TableSideBorderOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableSideBorderOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableBorderOptions

-- | The side border options for a table.
--
-- /See:/ 'newTableSideBorderOptions' smart constructor.
data TableSideBorderOptions = TableSideBorderOptions'
  { -- | The table border options of the bottom border.
    bottom :: Prelude.Maybe TableBorderOptions,
    -- | The table border options of the inner horizontal border.
    innerHorizontal :: Prelude.Maybe TableBorderOptions,
    -- | The table border options of the inner vertical border.
    innerVertical :: Prelude.Maybe TableBorderOptions,
    -- | The table border options of the left border.
    left :: Prelude.Maybe TableBorderOptions,
    -- | The table border options of the right border.
    right :: Prelude.Maybe TableBorderOptions,
    -- | The table border options of the top border.
    top :: Prelude.Maybe TableBorderOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableSideBorderOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bottom', 'tableSideBorderOptions_bottom' - The table border options of the bottom border.
--
-- 'innerHorizontal', 'tableSideBorderOptions_innerHorizontal' - The table border options of the inner horizontal border.
--
-- 'innerVertical', 'tableSideBorderOptions_innerVertical' - The table border options of the inner vertical border.
--
-- 'left', 'tableSideBorderOptions_left' - The table border options of the left border.
--
-- 'right', 'tableSideBorderOptions_right' - The table border options of the right border.
--
-- 'top', 'tableSideBorderOptions_top' - The table border options of the top border.
newTableSideBorderOptions ::
  TableSideBorderOptions
newTableSideBorderOptions =
  TableSideBorderOptions'
    { bottom = Prelude.Nothing,
      innerHorizontal = Prelude.Nothing,
      innerVertical = Prelude.Nothing,
      left = Prelude.Nothing,
      right = Prelude.Nothing,
      top = Prelude.Nothing
    }

-- | The table border options of the bottom border.
tableSideBorderOptions_bottom :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_bottom = Lens.lens (\TableSideBorderOptions' {bottom} -> bottom) (\s@TableSideBorderOptions' {} a -> s {bottom = a} :: TableSideBorderOptions)

-- | The table border options of the inner horizontal border.
tableSideBorderOptions_innerHorizontal :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_innerHorizontal = Lens.lens (\TableSideBorderOptions' {innerHorizontal} -> innerHorizontal) (\s@TableSideBorderOptions' {} a -> s {innerHorizontal = a} :: TableSideBorderOptions)

-- | The table border options of the inner vertical border.
tableSideBorderOptions_innerVertical :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_innerVertical = Lens.lens (\TableSideBorderOptions' {innerVertical} -> innerVertical) (\s@TableSideBorderOptions' {} a -> s {innerVertical = a} :: TableSideBorderOptions)

-- | The table border options of the left border.
tableSideBorderOptions_left :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_left = Lens.lens (\TableSideBorderOptions' {left} -> left) (\s@TableSideBorderOptions' {} a -> s {left = a} :: TableSideBorderOptions)

-- | The table border options of the right border.
tableSideBorderOptions_right :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_right = Lens.lens (\TableSideBorderOptions' {right} -> right) (\s@TableSideBorderOptions' {} a -> s {right = a} :: TableSideBorderOptions)

-- | The table border options of the top border.
tableSideBorderOptions_top :: Lens.Lens' TableSideBorderOptions (Prelude.Maybe TableBorderOptions)
tableSideBorderOptions_top = Lens.lens (\TableSideBorderOptions' {top} -> top) (\s@TableSideBorderOptions' {} a -> s {top = a} :: TableSideBorderOptions)

instance Data.FromJSON TableSideBorderOptions where
  parseJSON =
    Data.withObject
      "TableSideBorderOptions"
      ( \x ->
          TableSideBorderOptions'
            Prelude.<$> (x Data..:? "Bottom")
            Prelude.<*> (x Data..:? "InnerHorizontal")
            Prelude.<*> (x Data..:? "InnerVertical")
            Prelude.<*> (x Data..:? "Left")
            Prelude.<*> (x Data..:? "Right")
            Prelude.<*> (x Data..:? "Top")
      )

instance Prelude.Hashable TableSideBorderOptions where
  hashWithSalt _salt TableSideBorderOptions' {..} =
    _salt
      `Prelude.hashWithSalt` bottom
      `Prelude.hashWithSalt` innerHorizontal
      `Prelude.hashWithSalt` innerVertical
      `Prelude.hashWithSalt` left
      `Prelude.hashWithSalt` right
      `Prelude.hashWithSalt` top

instance Prelude.NFData TableSideBorderOptions where
  rnf TableSideBorderOptions' {..} =
    Prelude.rnf bottom
      `Prelude.seq` Prelude.rnf innerHorizontal
      `Prelude.seq` Prelude.rnf innerVertical
      `Prelude.seq` Prelude.rnf left
      `Prelude.seq` Prelude.rnf right
      `Prelude.seq` Prelude.rnf top

instance Data.ToJSON TableSideBorderOptions where
  toJSON TableSideBorderOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bottom" Data..=) Prelude.<$> bottom,
            ("InnerHorizontal" Data..=)
              Prelude.<$> innerHorizontal,
            ("InnerVertical" Data..=) Prelude.<$> innerVertical,
            ("Left" Data..=) Prelude.<$> left,
            ("Right" Data..=) Prelude.<$> right,
            ("Top" Data..=) Prelude.<$> top
          ]
      )

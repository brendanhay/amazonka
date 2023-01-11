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
-- Module      : Amazonka.QuickSight.Types.TableBorderOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableBorderOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableBorderStyle

-- | The border options for a table border.
--
-- /See:/ 'newTableBorderOptions' smart constructor.
data TableBorderOptions = TableBorderOptions'
  { -- | The color of a table border.
    color :: Prelude.Maybe Prelude.Text,
    -- | The style (none, solid) of a table border.
    style :: Prelude.Maybe TableBorderStyle,
    -- | The thickness of a table border.
    thickness :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableBorderOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'tableBorderOptions_color' - The color of a table border.
--
-- 'style', 'tableBorderOptions_style' - The style (none, solid) of a table border.
--
-- 'thickness', 'tableBorderOptions_thickness' - The thickness of a table border.
newTableBorderOptions ::
  TableBorderOptions
newTableBorderOptions =
  TableBorderOptions'
    { color = Prelude.Nothing,
      style = Prelude.Nothing,
      thickness = Prelude.Nothing
    }

-- | The color of a table border.
tableBorderOptions_color :: Lens.Lens' TableBorderOptions (Prelude.Maybe Prelude.Text)
tableBorderOptions_color = Lens.lens (\TableBorderOptions' {color} -> color) (\s@TableBorderOptions' {} a -> s {color = a} :: TableBorderOptions)

-- | The style (none, solid) of a table border.
tableBorderOptions_style :: Lens.Lens' TableBorderOptions (Prelude.Maybe TableBorderStyle)
tableBorderOptions_style = Lens.lens (\TableBorderOptions' {style} -> style) (\s@TableBorderOptions' {} a -> s {style = a} :: TableBorderOptions)

-- | The thickness of a table border.
tableBorderOptions_thickness :: Lens.Lens' TableBorderOptions (Prelude.Maybe Prelude.Natural)
tableBorderOptions_thickness = Lens.lens (\TableBorderOptions' {thickness} -> thickness) (\s@TableBorderOptions' {} a -> s {thickness = a} :: TableBorderOptions)

instance Data.FromJSON TableBorderOptions where
  parseJSON =
    Data.withObject
      "TableBorderOptions"
      ( \x ->
          TableBorderOptions'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "Style")
            Prelude.<*> (x Data..:? "Thickness")
      )

instance Prelude.Hashable TableBorderOptions where
  hashWithSalt _salt TableBorderOptions' {..} =
    _salt `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` style
      `Prelude.hashWithSalt` thickness

instance Prelude.NFData TableBorderOptions where
  rnf TableBorderOptions' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf style
      `Prelude.seq` Prelude.rnf thickness

instance Data.ToJSON TableBorderOptions where
  toJSON TableBorderOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("Style" Data..=) Prelude.<$> style,
            ("Thickness" Data..=) Prelude.<$> thickness
          ]
      )

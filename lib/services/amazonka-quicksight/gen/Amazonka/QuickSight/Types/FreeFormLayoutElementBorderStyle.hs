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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutElementBorderStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutElementBorderStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The background style configuration of a free-form layout element.
--
-- /See:/ 'newFreeFormLayoutElementBorderStyle' smart constructor.
data FreeFormLayoutElementBorderStyle = FreeFormLayoutElementBorderStyle'
  { -- | The border color of a free-form layout element.
    color :: Prelude.Maybe Prelude.Text,
    -- | The border visibility of a free-form layout element.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutElementBorderStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'freeFormLayoutElementBorderStyle_color' - The border color of a free-form layout element.
--
-- 'visibility', 'freeFormLayoutElementBorderStyle_visibility' - The border visibility of a free-form layout element.
newFreeFormLayoutElementBorderStyle ::
  FreeFormLayoutElementBorderStyle
newFreeFormLayoutElementBorderStyle =
  FreeFormLayoutElementBorderStyle'
    { color =
        Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The border color of a free-form layout element.
freeFormLayoutElementBorderStyle_color :: Lens.Lens' FreeFormLayoutElementBorderStyle (Prelude.Maybe Prelude.Text)
freeFormLayoutElementBorderStyle_color = Lens.lens (\FreeFormLayoutElementBorderStyle' {color} -> color) (\s@FreeFormLayoutElementBorderStyle' {} a -> s {color = a} :: FreeFormLayoutElementBorderStyle)

-- | The border visibility of a free-form layout element.
freeFormLayoutElementBorderStyle_visibility :: Lens.Lens' FreeFormLayoutElementBorderStyle (Prelude.Maybe Visibility)
freeFormLayoutElementBorderStyle_visibility = Lens.lens (\FreeFormLayoutElementBorderStyle' {visibility} -> visibility) (\s@FreeFormLayoutElementBorderStyle' {} a -> s {visibility = a} :: FreeFormLayoutElementBorderStyle)

instance
  Data.FromJSON
    FreeFormLayoutElementBorderStyle
  where
  parseJSON =
    Data.withObject
      "FreeFormLayoutElementBorderStyle"
      ( \x ->
          FreeFormLayoutElementBorderStyle'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance
  Prelude.Hashable
    FreeFormLayoutElementBorderStyle
  where
  hashWithSalt
    _salt
    FreeFormLayoutElementBorderStyle' {..} =
      _salt `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` visibility

instance
  Prelude.NFData
    FreeFormLayoutElementBorderStyle
  where
  rnf FreeFormLayoutElementBorderStyle' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON FreeFormLayoutElementBorderStyle where
  toJSON FreeFormLayoutElementBorderStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )

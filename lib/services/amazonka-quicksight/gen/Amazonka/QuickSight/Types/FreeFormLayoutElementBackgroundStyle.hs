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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutElementBackgroundStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutElementBackgroundStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The background style configuration of a free-form layout element.
--
-- /See:/ 'newFreeFormLayoutElementBackgroundStyle' smart constructor.
data FreeFormLayoutElementBackgroundStyle = FreeFormLayoutElementBackgroundStyle'
  { -- | The background color of a free-form layout element.
    color :: Prelude.Maybe Prelude.Text,
    -- | The background visibility of a free-form layout element.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutElementBackgroundStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'freeFormLayoutElementBackgroundStyle_color' - The background color of a free-form layout element.
--
-- 'visibility', 'freeFormLayoutElementBackgroundStyle_visibility' - The background visibility of a free-form layout element.
newFreeFormLayoutElementBackgroundStyle ::
  FreeFormLayoutElementBackgroundStyle
newFreeFormLayoutElementBackgroundStyle =
  FreeFormLayoutElementBackgroundStyle'
    { color =
        Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The background color of a free-form layout element.
freeFormLayoutElementBackgroundStyle_color :: Lens.Lens' FreeFormLayoutElementBackgroundStyle (Prelude.Maybe Prelude.Text)
freeFormLayoutElementBackgroundStyle_color = Lens.lens (\FreeFormLayoutElementBackgroundStyle' {color} -> color) (\s@FreeFormLayoutElementBackgroundStyle' {} a -> s {color = a} :: FreeFormLayoutElementBackgroundStyle)

-- | The background visibility of a free-form layout element.
freeFormLayoutElementBackgroundStyle_visibility :: Lens.Lens' FreeFormLayoutElementBackgroundStyle (Prelude.Maybe Visibility)
freeFormLayoutElementBackgroundStyle_visibility = Lens.lens (\FreeFormLayoutElementBackgroundStyle' {visibility} -> visibility) (\s@FreeFormLayoutElementBackgroundStyle' {} a -> s {visibility = a} :: FreeFormLayoutElementBackgroundStyle)

instance
  Data.FromJSON
    FreeFormLayoutElementBackgroundStyle
  where
  parseJSON =
    Data.withObject
      "FreeFormLayoutElementBackgroundStyle"
      ( \x ->
          FreeFormLayoutElementBackgroundStyle'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance
  Prelude.Hashable
    FreeFormLayoutElementBackgroundStyle
  where
  hashWithSalt
    _salt
    FreeFormLayoutElementBackgroundStyle' {..} =
      _salt
        `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` visibility

instance
  Prelude.NFData
    FreeFormLayoutElementBackgroundStyle
  where
  rnf FreeFormLayoutElementBackgroundStyle' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf visibility

instance
  Data.ToJSON
    FreeFormLayoutElementBackgroundStyle
  where
  toJSON FreeFormLayoutElementBackgroundStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )

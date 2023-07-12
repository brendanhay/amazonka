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
-- Module      : Amazonka.QuickSight.Types.Spacing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Spacing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of spacing (often a margin or padding).
--
-- /See:/ 'newSpacing' smart constructor.
data Spacing = Spacing'
  { -- | Define the bottom spacing.
    bottom :: Prelude.Maybe Prelude.Text,
    -- | Define the left spacing.
    left :: Prelude.Maybe Prelude.Text,
    -- | Define the right spacing.
    right :: Prelude.Maybe Prelude.Text,
    -- | Define the top spacing.
    top :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Spacing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bottom', 'spacing_bottom' - Define the bottom spacing.
--
-- 'left', 'spacing_left' - Define the left spacing.
--
-- 'right', 'spacing_right' - Define the right spacing.
--
-- 'top', 'spacing_top' - Define the top spacing.
newSpacing ::
  Spacing
newSpacing =
  Spacing'
    { bottom = Prelude.Nothing,
      left = Prelude.Nothing,
      right = Prelude.Nothing,
      top = Prelude.Nothing
    }

-- | Define the bottom spacing.
spacing_bottom :: Lens.Lens' Spacing (Prelude.Maybe Prelude.Text)
spacing_bottom = Lens.lens (\Spacing' {bottom} -> bottom) (\s@Spacing' {} a -> s {bottom = a} :: Spacing)

-- | Define the left spacing.
spacing_left :: Lens.Lens' Spacing (Prelude.Maybe Prelude.Text)
spacing_left = Lens.lens (\Spacing' {left} -> left) (\s@Spacing' {} a -> s {left = a} :: Spacing)

-- | Define the right spacing.
spacing_right :: Lens.Lens' Spacing (Prelude.Maybe Prelude.Text)
spacing_right = Lens.lens (\Spacing' {right} -> right) (\s@Spacing' {} a -> s {right = a} :: Spacing)

-- | Define the top spacing.
spacing_top :: Lens.Lens' Spacing (Prelude.Maybe Prelude.Text)
spacing_top = Lens.lens (\Spacing' {top} -> top) (\s@Spacing' {} a -> s {top = a} :: Spacing)

instance Data.FromJSON Spacing where
  parseJSON =
    Data.withObject
      "Spacing"
      ( \x ->
          Spacing'
            Prelude.<$> (x Data..:? "Bottom")
            Prelude.<*> (x Data..:? "Left")
            Prelude.<*> (x Data..:? "Right")
            Prelude.<*> (x Data..:? "Top")
      )

instance Prelude.Hashable Spacing where
  hashWithSalt _salt Spacing' {..} =
    _salt
      `Prelude.hashWithSalt` bottom
      `Prelude.hashWithSalt` left
      `Prelude.hashWithSalt` right
      `Prelude.hashWithSalt` top

instance Prelude.NFData Spacing where
  rnf Spacing' {..} =
    Prelude.rnf bottom
      `Prelude.seq` Prelude.rnf left
      `Prelude.seq` Prelude.rnf right
      `Prelude.seq` Prelude.rnf top

instance Data.ToJSON Spacing where
  toJSON Spacing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bottom" Data..=) Prelude.<$> bottom,
            ("Left" Data..=) Prelude.<$> left,
            ("Right" Data..=) Prelude.<$> right,
            ("Top" Data..=) Prelude.<$> top
          ]
      )

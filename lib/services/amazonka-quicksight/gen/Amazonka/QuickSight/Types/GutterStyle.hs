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
-- Module      : Amazonka.QuickSight.Types.GutterStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GutterStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The display options for gutter spacing between tiles on a sheet.
--
-- /See:/ 'newGutterStyle' smart constructor.
data GutterStyle = GutterStyle'
  { -- | This Boolean value controls whether to display a gutter space between
    -- sheet tiles.
    show :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GutterStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'show', 'gutterStyle_show' - This Boolean value controls whether to display a gutter space between
-- sheet tiles.
newGutterStyle ::
  GutterStyle
newGutterStyle = GutterStyle' {show = Prelude.Nothing}

-- | This Boolean value controls whether to display a gutter space between
-- sheet tiles.
gutterStyle_show :: Lens.Lens' GutterStyle (Prelude.Maybe Prelude.Bool)
gutterStyle_show = Lens.lens (\GutterStyle' {show} -> show) (\s@GutterStyle' {} a -> s {show = a} :: GutterStyle)

instance Data.FromJSON GutterStyle where
  parseJSON =
    Data.withObject
      "GutterStyle"
      (\x -> GutterStyle' Prelude.<$> (x Data..:? "Show"))

instance Prelude.Hashable GutterStyle where
  hashWithSalt _salt GutterStyle' {..} =
    _salt `Prelude.hashWithSalt` show

instance Prelude.NFData GutterStyle where
  rnf GutterStyle' {..} = Prelude.rnf show

instance Data.ToJSON GutterStyle where
  toJSON GutterStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Show" Data..=) Prelude.<$> show]
      )

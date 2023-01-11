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
-- Module      : Amazonka.AccessAnalyzer.Types.Position
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Position where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A position in a policy.
--
-- /See:/ 'newPosition' smart constructor.
data Position = Position'
  { -- | The line of the position, starting from 1.
    line :: Prelude.Int,
    -- | The column of the position, starting from 0.
    column :: Prelude.Int,
    -- | The offset within the policy that corresponds to the position, starting
    -- from 0.
    offset :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Position' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'line', 'position_line' - The line of the position, starting from 1.
--
-- 'column', 'position_column' - The column of the position, starting from 0.
--
-- 'offset', 'position_offset' - The offset within the policy that corresponds to the position, starting
-- from 0.
newPosition ::
  -- | 'line'
  Prelude.Int ->
  -- | 'column'
  Prelude.Int ->
  -- | 'offset'
  Prelude.Int ->
  Position
newPosition pLine_ pColumn_ pOffset_ =
  Position'
    { line = pLine_,
      column = pColumn_,
      offset = pOffset_
    }

-- | The line of the position, starting from 1.
position_line :: Lens.Lens' Position Prelude.Int
position_line = Lens.lens (\Position' {line} -> line) (\s@Position' {} a -> s {line = a} :: Position)

-- | The column of the position, starting from 0.
position_column :: Lens.Lens' Position Prelude.Int
position_column = Lens.lens (\Position' {column} -> column) (\s@Position' {} a -> s {column = a} :: Position)

-- | The offset within the policy that corresponds to the position, starting
-- from 0.
position_offset :: Lens.Lens' Position Prelude.Int
position_offset = Lens.lens (\Position' {offset} -> offset) (\s@Position' {} a -> s {offset = a} :: Position)

instance Data.FromJSON Position where
  parseJSON =
    Data.withObject
      "Position"
      ( \x ->
          Position'
            Prelude.<$> (x Data..: "line")
            Prelude.<*> (x Data..: "column")
            Prelude.<*> (x Data..: "offset")
      )

instance Prelude.Hashable Position where
  hashWithSalt _salt Position' {..} =
    _salt `Prelude.hashWithSalt` line
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` offset

instance Prelude.NFData Position where
  rnf Position' {..} =
    Prelude.rnf line
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf offset

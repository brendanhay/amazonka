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
-- Module      : Amazonka.IAM.Types.Position
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.Position where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the row and column of a location of a @Statement@ element in a
-- policy document.
--
-- This data type is used as a member of the @ Statement @ type.
--
-- /See:/ 'newPosition' smart constructor.
data Position = Position'
  { -- | The column in the line containing the specified position in the
    -- document.
    column :: Prelude.Maybe Prelude.Int,
    -- | The line containing the specified position in the document.
    line :: Prelude.Maybe Prelude.Int
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
-- 'column', 'position_column' - The column in the line containing the specified position in the
-- document.
--
-- 'line', 'position_line' - The line containing the specified position in the document.
newPosition ::
  Position
newPosition =
  Position'
    { column = Prelude.Nothing,
      line = Prelude.Nothing
    }

-- | The column in the line containing the specified position in the
-- document.
position_column :: Lens.Lens' Position (Prelude.Maybe Prelude.Int)
position_column = Lens.lens (\Position' {column} -> column) (\s@Position' {} a -> s {column = a} :: Position)

-- | The line containing the specified position in the document.
position_line :: Lens.Lens' Position (Prelude.Maybe Prelude.Int)
position_line = Lens.lens (\Position' {line} -> line) (\s@Position' {} a -> s {line = a} :: Position)

instance Data.FromXML Position where
  parseXML x =
    Position'
      Prelude.<$> (x Data..@? "Column") Prelude.<*> (x Data..@? "Line")

instance Prelude.Hashable Position where
  hashWithSalt _salt Position' {..} =
    _salt `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` line

instance Prelude.NFData Position where
  rnf Position' {..} =
    Prelude.rnf column `Prelude.seq` Prelude.rnf line

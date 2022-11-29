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
-- Module      : Amazonka.AppSync.Types.CodeErrorLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.CodeErrorLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the location of the error in a code sample.
--
-- /See:/ 'newCodeErrorLocation' smart constructor.
data CodeErrorLocation = CodeErrorLocation'
  { -- | The line number in the code. Defaults to @0@ if unknown.
    line :: Prelude.Maybe Prelude.Int,
    -- | The span\/length of the error. Defaults to @-1@ if unknown.
    span :: Prelude.Maybe Prelude.Int,
    -- | The column number in the code. Defaults to @0@ if unknown.
    column :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeErrorLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'line', 'codeErrorLocation_line' - The line number in the code. Defaults to @0@ if unknown.
--
-- 'span', 'codeErrorLocation_span' - The span\/length of the error. Defaults to @-1@ if unknown.
--
-- 'column', 'codeErrorLocation_column' - The column number in the code. Defaults to @0@ if unknown.
newCodeErrorLocation ::
  CodeErrorLocation
newCodeErrorLocation =
  CodeErrorLocation'
    { line = Prelude.Nothing,
      span = Prelude.Nothing,
      column = Prelude.Nothing
    }

-- | The line number in the code. Defaults to @0@ if unknown.
codeErrorLocation_line :: Lens.Lens' CodeErrorLocation (Prelude.Maybe Prelude.Int)
codeErrorLocation_line = Lens.lens (\CodeErrorLocation' {line} -> line) (\s@CodeErrorLocation' {} a -> s {line = a} :: CodeErrorLocation)

-- | The span\/length of the error. Defaults to @-1@ if unknown.
codeErrorLocation_span :: Lens.Lens' CodeErrorLocation (Prelude.Maybe Prelude.Int)
codeErrorLocation_span = Lens.lens (\CodeErrorLocation' {span} -> span) (\s@CodeErrorLocation' {} a -> s {span = a} :: CodeErrorLocation)

-- | The column number in the code. Defaults to @0@ if unknown.
codeErrorLocation_column :: Lens.Lens' CodeErrorLocation (Prelude.Maybe Prelude.Int)
codeErrorLocation_column = Lens.lens (\CodeErrorLocation' {column} -> column) (\s@CodeErrorLocation' {} a -> s {column = a} :: CodeErrorLocation)

instance Core.FromJSON CodeErrorLocation where
  parseJSON =
    Core.withObject
      "CodeErrorLocation"
      ( \x ->
          CodeErrorLocation'
            Prelude.<$> (x Core..:? "line")
            Prelude.<*> (x Core..:? "span")
            Prelude.<*> (x Core..:? "column")
      )

instance Prelude.Hashable CodeErrorLocation where
  hashWithSalt _salt CodeErrorLocation' {..} =
    _salt `Prelude.hashWithSalt` line
      `Prelude.hashWithSalt` span
      `Prelude.hashWithSalt` column

instance Prelude.NFData CodeErrorLocation where
  rnf CodeErrorLocation' {..} =
    Prelude.rnf line
      `Prelude.seq` Prelude.rnf span
      `Prelude.seq` Prelude.rnf column

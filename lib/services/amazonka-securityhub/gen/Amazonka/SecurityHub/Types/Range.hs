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
-- Module      : Amazonka.SecurityHub.Types.Range
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies where the sensitive data begins and ends.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The number of lines (for a line range) or characters (for an offset
    -- range) from the beginning of the file to the end of the sensitive data.
    end :: Prelude.Maybe Prelude.Integer,
    -- | The number of lines (for a line range) or characters (for an offset
    -- range) from the beginning of the file to the end of the sensitive data.
    start :: Prelude.Maybe Prelude.Integer,
    -- | In the line where the sensitive data starts, the column within the line
    -- where the sensitive data starts.
    startColumn :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'range_end' - The number of lines (for a line range) or characters (for an offset
-- range) from the beginning of the file to the end of the sensitive data.
--
-- 'start', 'range_start' - The number of lines (for a line range) or characters (for an offset
-- range) from the beginning of the file to the end of the sensitive data.
--
-- 'startColumn', 'range_startColumn' - In the line where the sensitive data starts, the column within the line
-- where the sensitive data starts.
newRange ::
  Range
newRange =
  Range'
    { end = Prelude.Nothing,
      start = Prelude.Nothing,
      startColumn = Prelude.Nothing
    }

-- | The number of lines (for a line range) or characters (for an offset
-- range) from the beginning of the file to the end of the sensitive data.
range_end :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_end = Lens.lens (\Range' {end} -> end) (\s@Range' {} a -> s {end = a} :: Range)

-- | The number of lines (for a line range) or characters (for an offset
-- range) from the beginning of the file to the end of the sensitive data.
range_start :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_start = Lens.lens (\Range' {start} -> start) (\s@Range' {} a -> s {start = a} :: Range)

-- | In the line where the sensitive data starts, the column within the line
-- where the sensitive data starts.
range_startColumn :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_startColumn = Lens.lens (\Range' {startColumn} -> startColumn) (\s@Range' {} a -> s {startColumn = a} :: Range)

instance Data.FromJSON Range where
  parseJSON =
    Data.withObject
      "Range"
      ( \x ->
          Range'
            Prelude.<$> (x Data..:? "End")
            Prelude.<*> (x Data..:? "Start")
            Prelude.<*> (x Data..:? "StartColumn")
      )

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` startColumn

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf end
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf startColumn

instance Data.ToJSON Range where
  toJSON Range' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("End" Data..=) Prelude.<$> end,
            ("Start" Data..=) Prelude.<$> start,
            ("StartColumn" Data..=) Prelude.<$> startColumn
          ]
      )

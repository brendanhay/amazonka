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
-- Module      : Amazonka.MacieV2.Types.Range
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of an occurrence of sensitive data in a
-- non-binary text file, such as an HTML, TXT, or XML file.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The number of lines from the beginning of the file to the beginning of
    -- the sensitive data.
    start :: Prelude.Maybe Prelude.Integer,
    -- | The number of lines from the beginning of the file to the end of the
    -- sensitive data.
    end :: Prelude.Maybe Prelude.Integer,
    -- | The number of characters, with spaces and starting from 1, from the
    -- beginning of the first line that contains the sensitive data (start) to
    -- the beginning of the sensitive data.
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
-- 'start', 'range_start' - The number of lines from the beginning of the file to the beginning of
-- the sensitive data.
--
-- 'end', 'range_end' - The number of lines from the beginning of the file to the end of the
-- sensitive data.
--
-- 'startColumn', 'range_startColumn' - The number of characters, with spaces and starting from 1, from the
-- beginning of the first line that contains the sensitive data (start) to
-- the beginning of the sensitive data.
newRange ::
  Range
newRange =
  Range'
    { start = Prelude.Nothing,
      end = Prelude.Nothing,
      startColumn = Prelude.Nothing
    }

-- | The number of lines from the beginning of the file to the beginning of
-- the sensitive data.
range_start :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_start = Lens.lens (\Range' {start} -> start) (\s@Range' {} a -> s {start = a} :: Range)

-- | The number of lines from the beginning of the file to the end of the
-- sensitive data.
range_end :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_end = Lens.lens (\Range' {end} -> end) (\s@Range' {} a -> s {end = a} :: Range)

-- | The number of characters, with spaces and starting from 1, from the
-- beginning of the first line that contains the sensitive data (start) to
-- the beginning of the sensitive data.
range_startColumn :: Lens.Lens' Range (Prelude.Maybe Prelude.Integer)
range_startColumn = Lens.lens (\Range' {startColumn} -> startColumn) (\s@Range' {} a -> s {startColumn = a} :: Range)

instance Core.FromJSON Range where
  parseJSON =
    Core.withObject
      "Range"
      ( \x ->
          Range'
            Prelude.<$> (x Core..:? "start")
            Prelude.<*> (x Core..:? "end")
            Prelude.<*> (x Core..:? "startColumn")
      )

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` startColumn

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf start
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf startColumn

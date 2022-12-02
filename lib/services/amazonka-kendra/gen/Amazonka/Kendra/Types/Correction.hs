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
-- Module      : Amazonka.Kendra.Types.Correction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Correction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A corrected misspelled word in a query.
--
-- /See:/ 'newCorrection' smart constructor.
data Correction = Correction'
  { -- | The zero-based location in the response string or text where the
    -- corrected word starts.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The zero-based location in the response string or text where the
    -- corrected word ends.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The string or text of a misspelled word in a query.
    term :: Prelude.Maybe Prelude.Text,
    -- | The string or text of a corrected misspelled word in a query.
    correctedTerm :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Correction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'correction_beginOffset' - The zero-based location in the response string or text where the
-- corrected word starts.
--
-- 'endOffset', 'correction_endOffset' - The zero-based location in the response string or text where the
-- corrected word ends.
--
-- 'term', 'correction_term' - The string or text of a misspelled word in a query.
--
-- 'correctedTerm', 'correction_correctedTerm' - The string or text of a corrected misspelled word in a query.
newCorrection ::
  Correction
newCorrection =
  Correction'
    { beginOffset = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      term = Prelude.Nothing,
      correctedTerm = Prelude.Nothing
    }

-- | The zero-based location in the response string or text where the
-- corrected word starts.
correction_beginOffset :: Lens.Lens' Correction (Prelude.Maybe Prelude.Int)
correction_beginOffset = Lens.lens (\Correction' {beginOffset} -> beginOffset) (\s@Correction' {} a -> s {beginOffset = a} :: Correction)

-- | The zero-based location in the response string or text where the
-- corrected word ends.
correction_endOffset :: Lens.Lens' Correction (Prelude.Maybe Prelude.Int)
correction_endOffset = Lens.lens (\Correction' {endOffset} -> endOffset) (\s@Correction' {} a -> s {endOffset = a} :: Correction)

-- | The string or text of a misspelled word in a query.
correction_term :: Lens.Lens' Correction (Prelude.Maybe Prelude.Text)
correction_term = Lens.lens (\Correction' {term} -> term) (\s@Correction' {} a -> s {term = a} :: Correction)

-- | The string or text of a corrected misspelled word in a query.
correction_correctedTerm :: Lens.Lens' Correction (Prelude.Maybe Prelude.Text)
correction_correctedTerm = Lens.lens (\Correction' {correctedTerm} -> correctedTerm) (\s@Correction' {} a -> s {correctedTerm = a} :: Correction)

instance Data.FromJSON Correction where
  parseJSON =
    Data.withObject
      "Correction"
      ( \x ->
          Correction'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Term")
            Prelude.<*> (x Data..:? "CorrectedTerm")
      )

instance Prelude.Hashable Correction where
  hashWithSalt _salt Correction' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` term
      `Prelude.hashWithSalt` correctedTerm

instance Prelude.NFData Correction where
  rnf Correction' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf term
      `Prelude.seq` Prelude.rnf correctedTerm

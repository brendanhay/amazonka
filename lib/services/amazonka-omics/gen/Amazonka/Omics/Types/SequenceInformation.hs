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
-- Module      : Amazonka.Omics.Types.SequenceInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SequenceInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a sequence.
--
-- /See:/ 'newSequenceInformation' smart constructor.
data SequenceInformation = SequenceInformation'
  { -- | The sequence\'s alignment setting.
    alignment :: Prelude.Maybe Prelude.Text,
    -- | Where the sequence originated.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | The sequence\'s total base count.
    totalBaseCount :: Prelude.Maybe Prelude.Integer,
    -- | The sequence\'s total read count.
    totalReadCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SequenceInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'sequenceInformation_alignment' - The sequence\'s alignment setting.
--
-- 'generatedFrom', 'sequenceInformation_generatedFrom' - Where the sequence originated.
--
-- 'totalBaseCount', 'sequenceInformation_totalBaseCount' - The sequence\'s total base count.
--
-- 'totalReadCount', 'sequenceInformation_totalReadCount' - The sequence\'s total read count.
newSequenceInformation ::
  SequenceInformation
newSequenceInformation =
  SequenceInformation'
    { alignment = Prelude.Nothing,
      generatedFrom = Prelude.Nothing,
      totalBaseCount = Prelude.Nothing,
      totalReadCount = Prelude.Nothing
    }

-- | The sequence\'s alignment setting.
sequenceInformation_alignment :: Lens.Lens' SequenceInformation (Prelude.Maybe Prelude.Text)
sequenceInformation_alignment = Lens.lens (\SequenceInformation' {alignment} -> alignment) (\s@SequenceInformation' {} a -> s {alignment = a} :: SequenceInformation)

-- | Where the sequence originated.
sequenceInformation_generatedFrom :: Lens.Lens' SequenceInformation (Prelude.Maybe Prelude.Text)
sequenceInformation_generatedFrom = Lens.lens (\SequenceInformation' {generatedFrom} -> generatedFrom) (\s@SequenceInformation' {} a -> s {generatedFrom = a} :: SequenceInformation)

-- | The sequence\'s total base count.
sequenceInformation_totalBaseCount :: Lens.Lens' SequenceInformation (Prelude.Maybe Prelude.Integer)
sequenceInformation_totalBaseCount = Lens.lens (\SequenceInformation' {totalBaseCount} -> totalBaseCount) (\s@SequenceInformation' {} a -> s {totalBaseCount = a} :: SequenceInformation)

-- | The sequence\'s total read count.
sequenceInformation_totalReadCount :: Lens.Lens' SequenceInformation (Prelude.Maybe Prelude.Integer)
sequenceInformation_totalReadCount = Lens.lens (\SequenceInformation' {totalReadCount} -> totalReadCount) (\s@SequenceInformation' {} a -> s {totalReadCount = a} :: SequenceInformation)

instance Data.FromJSON SequenceInformation where
  parseJSON =
    Data.withObject
      "SequenceInformation"
      ( \x ->
          SequenceInformation'
            Prelude.<$> (x Data..:? "alignment")
            Prelude.<*> (x Data..:? "generatedFrom")
            Prelude.<*> (x Data..:? "totalBaseCount")
            Prelude.<*> (x Data..:? "totalReadCount")
      )

instance Prelude.Hashable SequenceInformation where
  hashWithSalt _salt SequenceInformation' {..} =
    _salt `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` generatedFrom
      `Prelude.hashWithSalt` totalBaseCount
      `Prelude.hashWithSalt` totalReadCount

instance Prelude.NFData SequenceInformation where
  rnf SequenceInformation' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf totalBaseCount
      `Prelude.seq` Prelude.rnf totalReadCount

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
-- Module      : Amazonka.SecurityHub.Types.Occurrences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Occurrences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Cell
import Amazonka.SecurityHub.Types.Page
import Amazonka.SecurityHub.Types.Range
import Amazonka.SecurityHub.Types.Record

-- | The detected occurrences of sensitive data.
--
-- /See:/ 'newOccurrences' smart constructor.
data Occurrences = Occurrences'
  { -- | Occurrences of sensitive data in an Apache Avro object container or an
    -- Apache Parquet file.
    records :: Prelude.Maybe [Record],
    -- | Occurrences of sensitive data detected in a non-binary text file or a
    -- Microsoft Word file. Non-binary text files include files such as HTML,
    -- XML, JSON, and TXT files.
    lineRanges :: Prelude.Maybe [Range],
    -- | Occurrences of sensitive data detected in a binary text file.
    offsetRanges :: Prelude.Maybe [Range],
    -- | Occurrences of sensitive data detected in Microsoft Excel workbooks,
    -- comma-separated value (CSV) files, or tab-separated value (TSV) files.
    cells :: Prelude.Maybe [Cell],
    -- | Occurrences of sensitive data in an Adobe Portable Document Format (PDF)
    -- file.
    pages :: Prelude.Maybe [Page]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Occurrences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'records', 'occurrences_records' - Occurrences of sensitive data in an Apache Avro object container or an
-- Apache Parquet file.
--
-- 'lineRanges', 'occurrences_lineRanges' - Occurrences of sensitive data detected in a non-binary text file or a
-- Microsoft Word file. Non-binary text files include files such as HTML,
-- XML, JSON, and TXT files.
--
-- 'offsetRanges', 'occurrences_offsetRanges' - Occurrences of sensitive data detected in a binary text file.
--
-- 'cells', 'occurrences_cells' - Occurrences of sensitive data detected in Microsoft Excel workbooks,
-- comma-separated value (CSV) files, or tab-separated value (TSV) files.
--
-- 'pages', 'occurrences_pages' - Occurrences of sensitive data in an Adobe Portable Document Format (PDF)
-- file.
newOccurrences ::
  Occurrences
newOccurrences =
  Occurrences'
    { records = Prelude.Nothing,
      lineRanges = Prelude.Nothing,
      offsetRanges = Prelude.Nothing,
      cells = Prelude.Nothing,
      pages = Prelude.Nothing
    }

-- | Occurrences of sensitive data in an Apache Avro object container or an
-- Apache Parquet file.
occurrences_records :: Lens.Lens' Occurrences (Prelude.Maybe [Record])
occurrences_records = Lens.lens (\Occurrences' {records} -> records) (\s@Occurrences' {} a -> s {records = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | Occurrences of sensitive data detected in a non-binary text file or a
-- Microsoft Word file. Non-binary text files include files such as HTML,
-- XML, JSON, and TXT files.
occurrences_lineRanges :: Lens.Lens' Occurrences (Prelude.Maybe [Range])
occurrences_lineRanges = Lens.lens (\Occurrences' {lineRanges} -> lineRanges) (\s@Occurrences' {} a -> s {lineRanges = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | Occurrences of sensitive data detected in a binary text file.
occurrences_offsetRanges :: Lens.Lens' Occurrences (Prelude.Maybe [Range])
occurrences_offsetRanges = Lens.lens (\Occurrences' {offsetRanges} -> offsetRanges) (\s@Occurrences' {} a -> s {offsetRanges = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | Occurrences of sensitive data detected in Microsoft Excel workbooks,
-- comma-separated value (CSV) files, or tab-separated value (TSV) files.
occurrences_cells :: Lens.Lens' Occurrences (Prelude.Maybe [Cell])
occurrences_cells = Lens.lens (\Occurrences' {cells} -> cells) (\s@Occurrences' {} a -> s {cells = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | Occurrences of sensitive data in an Adobe Portable Document Format (PDF)
-- file.
occurrences_pages :: Lens.Lens' Occurrences (Prelude.Maybe [Page])
occurrences_pages = Lens.lens (\Occurrences' {pages} -> pages) (\s@Occurrences' {} a -> s {pages = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Occurrences where
  parseJSON =
    Core.withObject
      "Occurrences"
      ( \x ->
          Occurrences'
            Prelude.<$> (x Core..:? "Records" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LineRanges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OffsetRanges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Cells" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Pages" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Occurrences where
  hashWithSalt _salt Occurrences' {..} =
    _salt `Prelude.hashWithSalt` records
      `Prelude.hashWithSalt` lineRanges
      `Prelude.hashWithSalt` offsetRanges
      `Prelude.hashWithSalt` cells
      `Prelude.hashWithSalt` pages

instance Prelude.NFData Occurrences where
  rnf Occurrences' {..} =
    Prelude.rnf records
      `Prelude.seq` Prelude.rnf lineRanges
      `Prelude.seq` Prelude.rnf offsetRanges
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf pages

instance Core.ToJSON Occurrences where
  toJSON Occurrences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Records" Core..=) Prelude.<$> records,
            ("LineRanges" Core..=) Prelude.<$> lineRanges,
            ("OffsetRanges" Core..=) Prelude.<$> offsetRanges,
            ("Cells" Core..=) Prelude.<$> cells,
            ("Pages" Core..=) Prelude.<$> pages
          ]
      )

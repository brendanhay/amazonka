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
-- Module      : Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.CsvFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.CSVFileCompression
import qualified Amazonka.Prelude as Prelude

-- | Contains information about how a source CSV data file should be
-- analyzed.
--
-- /See:/ 'newCsvFormatDescriptor' smart constructor.
data CsvFormatDescriptor = CsvFormatDescriptor'
  { -- | The character used as a quote character.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the source CSV file contains a header.
    containsHeader :: Prelude.Maybe Prelude.Bool,
    -- | The character used to delimit the source CSV file.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | A list of the source CSV file\'s headers, if any.
    headerList :: Prelude.Maybe [Prelude.Text],
    -- | The level of compression of the source CSV file.
    fileCompression :: Prelude.Maybe CSVFileCompression,
    -- | The character set in which the source CSV file is written.
    charset :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsvFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quoteSymbol', 'csvFormatDescriptor_quoteSymbol' - The character used as a quote character.
--
-- 'containsHeader', 'csvFormatDescriptor_containsHeader' - Whether or not the source CSV file contains a header.
--
-- 'delimiter', 'csvFormatDescriptor_delimiter' - The character used to delimit the source CSV file.
--
-- 'headerList', 'csvFormatDescriptor_headerList' - A list of the source CSV file\'s headers, if any.
--
-- 'fileCompression', 'csvFormatDescriptor_fileCompression' - The level of compression of the source CSV file.
--
-- 'charset', 'csvFormatDescriptor_charset' - The character set in which the source CSV file is written.
newCsvFormatDescriptor ::
  CsvFormatDescriptor
newCsvFormatDescriptor =
  CsvFormatDescriptor'
    { quoteSymbol = Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      headerList = Prelude.Nothing,
      fileCompression = Prelude.Nothing,
      charset = Prelude.Nothing
    }

-- | The character used as a quote character.
csvFormatDescriptor_quoteSymbol :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe Prelude.Text)
csvFormatDescriptor_quoteSymbol = Lens.lens (\CsvFormatDescriptor' {quoteSymbol} -> quoteSymbol) (\s@CsvFormatDescriptor' {} a -> s {quoteSymbol = a} :: CsvFormatDescriptor)

-- | Whether or not the source CSV file contains a header.
csvFormatDescriptor_containsHeader :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe Prelude.Bool)
csvFormatDescriptor_containsHeader = Lens.lens (\CsvFormatDescriptor' {containsHeader} -> containsHeader) (\s@CsvFormatDescriptor' {} a -> s {containsHeader = a} :: CsvFormatDescriptor)

-- | The character used to delimit the source CSV file.
csvFormatDescriptor_delimiter :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe Prelude.Text)
csvFormatDescriptor_delimiter = Lens.lens (\CsvFormatDescriptor' {delimiter} -> delimiter) (\s@CsvFormatDescriptor' {} a -> s {delimiter = a} :: CsvFormatDescriptor)

-- | A list of the source CSV file\'s headers, if any.
csvFormatDescriptor_headerList :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe [Prelude.Text])
csvFormatDescriptor_headerList = Lens.lens (\CsvFormatDescriptor' {headerList} -> headerList) (\s@CsvFormatDescriptor' {} a -> s {headerList = a} :: CsvFormatDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | The level of compression of the source CSV file.
csvFormatDescriptor_fileCompression :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe CSVFileCompression)
csvFormatDescriptor_fileCompression = Lens.lens (\CsvFormatDescriptor' {fileCompression} -> fileCompression) (\s@CsvFormatDescriptor' {} a -> s {fileCompression = a} :: CsvFormatDescriptor)

-- | The character set in which the source CSV file is written.
csvFormatDescriptor_charset :: Lens.Lens' CsvFormatDescriptor (Prelude.Maybe Prelude.Text)
csvFormatDescriptor_charset = Lens.lens (\CsvFormatDescriptor' {charset} -> charset) (\s@CsvFormatDescriptor' {} a -> s {charset = a} :: CsvFormatDescriptor)

instance Core.FromJSON CsvFormatDescriptor where
  parseJSON =
    Core.withObject
      "CsvFormatDescriptor"
      ( \x ->
          CsvFormatDescriptor'
            Prelude.<$> (x Core..:? "QuoteSymbol")
            Prelude.<*> (x Core..:? "ContainsHeader")
            Prelude.<*> (x Core..:? "Delimiter")
            Prelude.<*> (x Core..:? "HeaderList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FileCompression")
            Prelude.<*> (x Core..:? "Charset")
      )

instance Prelude.Hashable CsvFormatDescriptor where
  hashWithSalt _salt CsvFormatDescriptor' {..} =
    _salt `Prelude.hashWithSalt` quoteSymbol
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` headerList
      `Prelude.hashWithSalt` fileCompression
      `Prelude.hashWithSalt` charset

instance Prelude.NFData CsvFormatDescriptor where
  rnf CsvFormatDescriptor' {..} =
    Prelude.rnf quoteSymbol
      `Prelude.seq` Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf headerList
      `Prelude.seq` Prelude.rnf fileCompression
      `Prelude.seq` Prelude.rnf charset

instance Core.ToJSON CsvFormatDescriptor where
  toJSON CsvFormatDescriptor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QuoteSymbol" Core..=) Prelude.<$> quoteSymbol,
            ("ContainsHeader" Core..=)
              Prelude.<$> containsHeader,
            ("Delimiter" Core..=) Prelude.<$> delimiter,
            ("HeaderList" Core..=) Prelude.<$> headerList,
            ("FileCompression" Core..=)
              Prelude.<$> fileCompression,
            ("Charset" Core..=) Prelude.<$> charset
          ]
      )

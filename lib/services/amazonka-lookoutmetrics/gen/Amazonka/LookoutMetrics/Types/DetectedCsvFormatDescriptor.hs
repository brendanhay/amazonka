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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedCsvFormatDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedCsvFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DetectedField
import qualified Amazonka.Prelude as Prelude

-- | Properties of an inferred CSV format.
--
-- /See:/ 'newDetectedCsvFormatDescriptor' smart constructor.
data DetectedCsvFormatDescriptor = DetectedCsvFormatDescriptor'
  { -- | The format\'s charset.
    charset :: Prelude.Maybe DetectedField,
    -- | Whether the format includes a header.
    containsHeader :: Prelude.Maybe DetectedField,
    -- | The format\'s delimiter.
    delimiter :: Prelude.Maybe DetectedField,
    -- | The format\'s file compression.
    fileCompression :: Prelude.Maybe DetectedField,
    -- | The format\'s header list.
    headerList :: Prelude.Maybe DetectedField,
    -- | The format\'s quote symbol.
    quoteSymbol :: Prelude.Maybe DetectedField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedCsvFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'charset', 'detectedCsvFormatDescriptor_charset' - The format\'s charset.
--
-- 'containsHeader', 'detectedCsvFormatDescriptor_containsHeader' - Whether the format includes a header.
--
-- 'delimiter', 'detectedCsvFormatDescriptor_delimiter' - The format\'s delimiter.
--
-- 'fileCompression', 'detectedCsvFormatDescriptor_fileCompression' - The format\'s file compression.
--
-- 'headerList', 'detectedCsvFormatDescriptor_headerList' - The format\'s header list.
--
-- 'quoteSymbol', 'detectedCsvFormatDescriptor_quoteSymbol' - The format\'s quote symbol.
newDetectedCsvFormatDescriptor ::
  DetectedCsvFormatDescriptor
newDetectedCsvFormatDescriptor =
  DetectedCsvFormatDescriptor'
    { charset =
        Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      fileCompression = Prelude.Nothing,
      headerList = Prelude.Nothing,
      quoteSymbol = Prelude.Nothing
    }

-- | The format\'s charset.
detectedCsvFormatDescriptor_charset :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_charset = Lens.lens (\DetectedCsvFormatDescriptor' {charset} -> charset) (\s@DetectedCsvFormatDescriptor' {} a -> s {charset = a} :: DetectedCsvFormatDescriptor)

-- | Whether the format includes a header.
detectedCsvFormatDescriptor_containsHeader :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_containsHeader = Lens.lens (\DetectedCsvFormatDescriptor' {containsHeader} -> containsHeader) (\s@DetectedCsvFormatDescriptor' {} a -> s {containsHeader = a} :: DetectedCsvFormatDescriptor)

-- | The format\'s delimiter.
detectedCsvFormatDescriptor_delimiter :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_delimiter = Lens.lens (\DetectedCsvFormatDescriptor' {delimiter} -> delimiter) (\s@DetectedCsvFormatDescriptor' {} a -> s {delimiter = a} :: DetectedCsvFormatDescriptor)

-- | The format\'s file compression.
detectedCsvFormatDescriptor_fileCompression :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_fileCompression = Lens.lens (\DetectedCsvFormatDescriptor' {fileCompression} -> fileCompression) (\s@DetectedCsvFormatDescriptor' {} a -> s {fileCompression = a} :: DetectedCsvFormatDescriptor)

-- | The format\'s header list.
detectedCsvFormatDescriptor_headerList :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_headerList = Lens.lens (\DetectedCsvFormatDescriptor' {headerList} -> headerList) (\s@DetectedCsvFormatDescriptor' {} a -> s {headerList = a} :: DetectedCsvFormatDescriptor)

-- | The format\'s quote symbol.
detectedCsvFormatDescriptor_quoteSymbol :: Lens.Lens' DetectedCsvFormatDescriptor (Prelude.Maybe DetectedField)
detectedCsvFormatDescriptor_quoteSymbol = Lens.lens (\DetectedCsvFormatDescriptor' {quoteSymbol} -> quoteSymbol) (\s@DetectedCsvFormatDescriptor' {} a -> s {quoteSymbol = a} :: DetectedCsvFormatDescriptor)

instance Data.FromJSON DetectedCsvFormatDescriptor where
  parseJSON =
    Data.withObject
      "DetectedCsvFormatDescriptor"
      ( \x ->
          DetectedCsvFormatDescriptor'
            Prelude.<$> (x Data..:? "Charset")
            Prelude.<*> (x Data..:? "ContainsHeader")
            Prelude.<*> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "FileCompression")
            Prelude.<*> (x Data..:? "HeaderList")
            Prelude.<*> (x Data..:? "QuoteSymbol")
      )

instance Prelude.Hashable DetectedCsvFormatDescriptor where
  hashWithSalt _salt DetectedCsvFormatDescriptor' {..} =
    _salt
      `Prelude.hashWithSalt` charset
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` fileCompression
      `Prelude.hashWithSalt` headerList
      `Prelude.hashWithSalt` quoteSymbol

instance Prelude.NFData DetectedCsvFormatDescriptor where
  rnf DetectedCsvFormatDescriptor' {..} =
    Prelude.rnf charset
      `Prelude.seq` Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf fileCompression
      `Prelude.seq` Prelude.rnf headerList
      `Prelude.seq` Prelude.rnf quoteSymbol

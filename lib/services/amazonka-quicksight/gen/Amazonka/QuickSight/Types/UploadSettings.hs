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
-- Module      : Amazonka.QuickSight.Types.UploadSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UploadSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FileFormat
import Amazonka.QuickSight.Types.TextQualifier

-- | Information about the format for a source file or files.
--
-- /See:/ 'newUploadSettings' smart constructor.
data UploadSettings = UploadSettings'
  { -- | Whether the file has a header row, or the files each have a header row.
    containsHeader :: Prelude.Maybe Prelude.Bool,
    -- | The delimiter between values in the file.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | File format.
    format :: Prelude.Maybe FileFormat,
    -- | A row number to start reading data from.
    startFromRow :: Prelude.Maybe Prelude.Natural,
    -- | Text qualifier.
    textQualifier :: Prelude.Maybe TextQualifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containsHeader', 'uploadSettings_containsHeader' - Whether the file has a header row, or the files each have a header row.
--
-- 'delimiter', 'uploadSettings_delimiter' - The delimiter between values in the file.
--
-- 'format', 'uploadSettings_format' - File format.
--
-- 'startFromRow', 'uploadSettings_startFromRow' - A row number to start reading data from.
--
-- 'textQualifier', 'uploadSettings_textQualifier' - Text qualifier.
newUploadSettings ::
  UploadSettings
newUploadSettings =
  UploadSettings'
    { containsHeader = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      format = Prelude.Nothing,
      startFromRow = Prelude.Nothing,
      textQualifier = Prelude.Nothing
    }

-- | Whether the file has a header row, or the files each have a header row.
uploadSettings_containsHeader :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Bool)
uploadSettings_containsHeader = Lens.lens (\UploadSettings' {containsHeader} -> containsHeader) (\s@UploadSettings' {} a -> s {containsHeader = a} :: UploadSettings)

-- | The delimiter between values in the file.
uploadSettings_delimiter :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Text)
uploadSettings_delimiter = Lens.lens (\UploadSettings' {delimiter} -> delimiter) (\s@UploadSettings' {} a -> s {delimiter = a} :: UploadSettings)

-- | File format.
uploadSettings_format :: Lens.Lens' UploadSettings (Prelude.Maybe FileFormat)
uploadSettings_format = Lens.lens (\UploadSettings' {format} -> format) (\s@UploadSettings' {} a -> s {format = a} :: UploadSettings)

-- | A row number to start reading data from.
uploadSettings_startFromRow :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Natural)
uploadSettings_startFromRow = Lens.lens (\UploadSettings' {startFromRow} -> startFromRow) (\s@UploadSettings' {} a -> s {startFromRow = a} :: UploadSettings)

-- | Text qualifier.
uploadSettings_textQualifier :: Lens.Lens' UploadSettings (Prelude.Maybe TextQualifier)
uploadSettings_textQualifier = Lens.lens (\UploadSettings' {textQualifier} -> textQualifier) (\s@UploadSettings' {} a -> s {textQualifier = a} :: UploadSettings)

instance Data.FromJSON UploadSettings where
  parseJSON =
    Data.withObject
      "UploadSettings"
      ( \x ->
          UploadSettings'
            Prelude.<$> (x Data..:? "ContainsHeader")
            Prelude.<*> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "StartFromRow")
            Prelude.<*> (x Data..:? "TextQualifier")
      )

instance Prelude.Hashable UploadSettings where
  hashWithSalt _salt UploadSettings' {..} =
    _salt
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` startFromRow
      `Prelude.hashWithSalt` textQualifier

instance Prelude.NFData UploadSettings where
  rnf UploadSettings' {..} =
    Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf startFromRow
      `Prelude.seq` Prelude.rnf textQualifier

instance Data.ToJSON UploadSettings where
  toJSON UploadSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainsHeader" Data..=)
              Prelude.<$> containsHeader,
            ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("Format" Data..=) Prelude.<$> format,
            ("StartFromRow" Data..=) Prelude.<$> startFromRow,
            ("TextQualifier" Data..=) Prelude.<$> textQualifier
          ]
      )

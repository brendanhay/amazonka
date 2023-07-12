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
-- Module      : Amazonka.Glacier.Types.CSVInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.CSVInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.FileHeaderInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the comma-separated value (CSV) file to
-- select from.
--
-- /See:/ 'newCSVInput' smart constructor.
data CSVInput = CSVInput'
  { -- | A single character used to indicate that a row should be ignored when
    -- the character is present at the start of that row.
    comments :: Prelude.Maybe Prelude.Text,
    -- | A value used to separate individual fields from each other within a
    -- record.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
    -- | Describes the first line of input. Valid values are @None@, @Ignore@,
    -- and @Use@.
    fileHeaderInfo :: Prelude.Maybe FileHeaderInfo,
    -- | A value used as an escape character where the field delimiter is part of
    -- the value.
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | A single character used for escaping the quotation-mark character inside
    -- an already escaped value.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text,
    -- | A value used to separate individual records from each other.
    recordDelimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CSVInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comments', 'cSVInput_comments' - A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row.
--
-- 'fieldDelimiter', 'cSVInput_fieldDelimiter' - A value used to separate individual fields from each other within a
-- record.
--
-- 'fileHeaderInfo', 'cSVInput_fileHeaderInfo' - Describes the first line of input. Valid values are @None@, @Ignore@,
-- and @Use@.
--
-- 'quoteCharacter', 'cSVInput_quoteCharacter' - A value used as an escape character where the field delimiter is part of
-- the value.
--
-- 'quoteEscapeCharacter', 'cSVInput_quoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside
-- an already escaped value.
--
-- 'recordDelimiter', 'cSVInput_recordDelimiter' - A value used to separate individual records from each other.
newCSVInput ::
  CSVInput
newCSVInput =
  CSVInput'
    { comments = Prelude.Nothing,
      fieldDelimiter = Prelude.Nothing,
      fileHeaderInfo = Prelude.Nothing,
      quoteCharacter = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing,
      recordDelimiter = Prelude.Nothing
    }

-- | A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row.
cSVInput_comments :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_comments = Lens.lens (\CSVInput' {comments} -> comments) (\s@CSVInput' {} a -> s {comments = a} :: CSVInput)

-- | A value used to separate individual fields from each other within a
-- record.
cSVInput_fieldDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_fieldDelimiter = Lens.lens (\CSVInput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVInput' {} a -> s {fieldDelimiter = a} :: CSVInput)

-- | Describes the first line of input. Valid values are @None@, @Ignore@,
-- and @Use@.
cSVInput_fileHeaderInfo :: Lens.Lens' CSVInput (Prelude.Maybe FileHeaderInfo)
cSVInput_fileHeaderInfo = Lens.lens (\CSVInput' {fileHeaderInfo} -> fileHeaderInfo) (\s@CSVInput' {} a -> s {fileHeaderInfo = a} :: CSVInput)

-- | A value used as an escape character where the field delimiter is part of
-- the value.
cSVInput_quoteCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteCharacter = Lens.lens (\CSVInput' {quoteCharacter} -> quoteCharacter) (\s@CSVInput' {} a -> s {quoteCharacter = a} :: CSVInput)

-- | A single character used for escaping the quotation-mark character inside
-- an already escaped value.
cSVInput_quoteEscapeCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteEscapeCharacter = Lens.lens (\CSVInput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVInput' {} a -> s {quoteEscapeCharacter = a} :: CSVInput)

-- | A value used to separate individual records from each other.
cSVInput_recordDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_recordDelimiter = Lens.lens (\CSVInput' {recordDelimiter} -> recordDelimiter) (\s@CSVInput' {} a -> s {recordDelimiter = a} :: CSVInput)

instance Data.FromJSON CSVInput where
  parseJSON =
    Data.withObject
      "CSVInput"
      ( \x ->
          CSVInput'
            Prelude.<$> (x Data..:? "Comments")
            Prelude.<*> (x Data..:? "FieldDelimiter")
            Prelude.<*> (x Data..:? "FileHeaderInfo")
            Prelude.<*> (x Data..:? "QuoteCharacter")
            Prelude.<*> (x Data..:? "QuoteEscapeCharacter")
            Prelude.<*> (x Data..:? "RecordDelimiter")
      )

instance Prelude.Hashable CSVInput where
  hashWithSalt _salt CSVInput' {..} =
    _salt
      `Prelude.hashWithSalt` comments
      `Prelude.hashWithSalt` fieldDelimiter
      `Prelude.hashWithSalt` fileHeaderInfo
      `Prelude.hashWithSalt` quoteCharacter
      `Prelude.hashWithSalt` quoteEscapeCharacter
      `Prelude.hashWithSalt` recordDelimiter

instance Prelude.NFData CSVInput where
  rnf CSVInput' {..} =
    Prelude.rnf comments
      `Prelude.seq` Prelude.rnf fieldDelimiter
      `Prelude.seq` Prelude.rnf fileHeaderInfo
      `Prelude.seq` Prelude.rnf quoteCharacter
      `Prelude.seq` Prelude.rnf quoteEscapeCharacter
      `Prelude.seq` Prelude.rnf recordDelimiter

instance Data.ToJSON CSVInput where
  toJSON CSVInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comments" Data..=) Prelude.<$> comments,
            ("FieldDelimiter" Data..=)
              Prelude.<$> fieldDelimiter,
            ("FileHeaderInfo" Data..=)
              Prelude.<$> fileHeaderInfo,
            ("QuoteCharacter" Data..=)
              Prelude.<$> quoteCharacter,
            ("QuoteEscapeCharacter" Data..=)
              Prelude.<$> quoteEscapeCharacter,
            ("RecordDelimiter" Data..=)
              Prelude.<$> recordDelimiter
          ]
      )

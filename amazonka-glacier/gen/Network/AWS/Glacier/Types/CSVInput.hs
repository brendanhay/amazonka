{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.Types.CSVInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CSVInput where

import Network.AWS.Glacier.Types.FileHeaderInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the comma-separated value (CSV) file to
-- select from.
--
-- /See:/ 'newCSVInput' smart constructor.
data CSVInput = CSVInput'
  { -- | A value used to separate individual records from each other.
    recordDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A value used as an escape character where the field delimiter is part of
    -- the value.
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | Describes the first line of input. Valid values are @None@, @Ignore@,
    -- and @Use@.
    fileHeaderInfo :: Prelude.Maybe FileHeaderInfo,
    -- | A value used to separate individual fields from each other within a
    -- record.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A single character used to indicate that a row should be ignored when
    -- the character is present at the start of that row.
    comments :: Prelude.Maybe Prelude.Text,
    -- | A single character used for escaping the quotation-mark character inside
    -- an already escaped value.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CSVInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDelimiter', 'cSVInput_recordDelimiter' - A value used to separate individual records from each other.
--
-- 'quoteCharacter', 'cSVInput_quoteCharacter' - A value used as an escape character where the field delimiter is part of
-- the value.
--
-- 'fileHeaderInfo', 'cSVInput_fileHeaderInfo' - Describes the first line of input. Valid values are @None@, @Ignore@,
-- and @Use@.
--
-- 'fieldDelimiter', 'cSVInput_fieldDelimiter' - A value used to separate individual fields from each other within a
-- record.
--
-- 'comments', 'cSVInput_comments' - A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row.
--
-- 'quoteEscapeCharacter', 'cSVInput_quoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside
-- an already escaped value.
newCSVInput ::
  CSVInput
newCSVInput =
  CSVInput'
    { recordDelimiter = Prelude.Nothing,
      quoteCharacter = Prelude.Nothing,
      fileHeaderInfo = Prelude.Nothing,
      fieldDelimiter = Prelude.Nothing,
      comments = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing
    }

-- | A value used to separate individual records from each other.
cSVInput_recordDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_recordDelimiter = Lens.lens (\CSVInput' {recordDelimiter} -> recordDelimiter) (\s@CSVInput' {} a -> s {recordDelimiter = a} :: CSVInput)

-- | A value used as an escape character where the field delimiter is part of
-- the value.
cSVInput_quoteCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteCharacter = Lens.lens (\CSVInput' {quoteCharacter} -> quoteCharacter) (\s@CSVInput' {} a -> s {quoteCharacter = a} :: CSVInput)

-- | Describes the first line of input. Valid values are @None@, @Ignore@,
-- and @Use@.
cSVInput_fileHeaderInfo :: Lens.Lens' CSVInput (Prelude.Maybe FileHeaderInfo)
cSVInput_fileHeaderInfo = Lens.lens (\CSVInput' {fileHeaderInfo} -> fileHeaderInfo) (\s@CSVInput' {} a -> s {fileHeaderInfo = a} :: CSVInput)

-- | A value used to separate individual fields from each other within a
-- record.
cSVInput_fieldDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_fieldDelimiter = Lens.lens (\CSVInput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVInput' {} a -> s {fieldDelimiter = a} :: CSVInput)

-- | A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row.
cSVInput_comments :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_comments = Lens.lens (\CSVInput' {comments} -> comments) (\s@CSVInput' {} a -> s {comments = a} :: CSVInput)

-- | A single character used for escaping the quotation-mark character inside
-- an already escaped value.
cSVInput_quoteEscapeCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteEscapeCharacter = Lens.lens (\CSVInput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVInput' {} a -> s {quoteEscapeCharacter = a} :: CSVInput)

instance Prelude.FromJSON CSVInput where
  parseJSON =
    Prelude.withObject
      "CSVInput"
      ( \x ->
          CSVInput'
            Prelude.<$> (x Prelude..:? "RecordDelimiter")
            Prelude.<*> (x Prelude..:? "QuoteCharacter")
            Prelude.<*> (x Prelude..:? "FileHeaderInfo")
            Prelude.<*> (x Prelude..:? "FieldDelimiter")
            Prelude.<*> (x Prelude..:? "Comments")
            Prelude.<*> (x Prelude..:? "QuoteEscapeCharacter")
      )

instance Prelude.Hashable CSVInput

instance Prelude.NFData CSVInput

instance Prelude.ToJSON CSVInput where
  toJSON CSVInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RecordDelimiter" Prelude..=)
              Prelude.<$> recordDelimiter,
            ("QuoteCharacter" Prelude..=)
              Prelude.<$> quoteCharacter,
            ("FileHeaderInfo" Prelude..=)
              Prelude.<$> fileHeaderInfo,
            ("FieldDelimiter" Prelude..=)
              Prelude.<$> fieldDelimiter,
            ("Comments" Prelude..=) Prelude.<$> comments,
            ("QuoteEscapeCharacter" Prelude..=)
              Prelude.<$> quoteEscapeCharacter
          ]
      )

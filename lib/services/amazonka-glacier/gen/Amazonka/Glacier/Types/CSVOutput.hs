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
-- Module      : Amazonka.Glacier.Types.CSVOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.CSVOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.QuoteFields
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the comma-separated value (CSV) file that the
-- job results are stored in.
--
-- /See:/ 'newCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { -- | A value used to separate individual fields from each other within a
    -- record.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A value used as an escape character where the field delimiter is part of
    -- the value.
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | A single character used for escaping the quotation-mark character inside
    -- an already escaped value.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether all output fields should be contained
    -- within quotation marks.
    quoteFields :: Prelude.Maybe QuoteFields,
    -- | A value used to separate individual records from each other.
    recordDelimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CSVOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldDelimiter', 'cSVOutput_fieldDelimiter' - A value used to separate individual fields from each other within a
-- record.
--
-- 'quoteCharacter', 'cSVOutput_quoteCharacter' - A value used as an escape character where the field delimiter is part of
-- the value.
--
-- 'quoteEscapeCharacter', 'cSVOutput_quoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside
-- an already escaped value.
--
-- 'quoteFields', 'cSVOutput_quoteFields' - A value that indicates whether all output fields should be contained
-- within quotation marks.
--
-- 'recordDelimiter', 'cSVOutput_recordDelimiter' - A value used to separate individual records from each other.
newCSVOutput ::
  CSVOutput
newCSVOutput =
  CSVOutput'
    { fieldDelimiter = Prelude.Nothing,
      quoteCharacter = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing,
      quoteFields = Prelude.Nothing,
      recordDelimiter = Prelude.Nothing
    }

-- | A value used to separate individual fields from each other within a
-- record.
cSVOutput_fieldDelimiter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_fieldDelimiter = Lens.lens (\CSVOutput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVOutput' {} a -> s {fieldDelimiter = a} :: CSVOutput)

-- | A value used as an escape character where the field delimiter is part of
-- the value.
cSVOutput_quoteCharacter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_quoteCharacter = Lens.lens (\CSVOutput' {quoteCharacter} -> quoteCharacter) (\s@CSVOutput' {} a -> s {quoteCharacter = a} :: CSVOutput)

-- | A single character used for escaping the quotation-mark character inside
-- an already escaped value.
cSVOutput_quoteEscapeCharacter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_quoteEscapeCharacter = Lens.lens (\CSVOutput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVOutput' {} a -> s {quoteEscapeCharacter = a} :: CSVOutput)

-- | A value that indicates whether all output fields should be contained
-- within quotation marks.
cSVOutput_quoteFields :: Lens.Lens' CSVOutput (Prelude.Maybe QuoteFields)
cSVOutput_quoteFields = Lens.lens (\CSVOutput' {quoteFields} -> quoteFields) (\s@CSVOutput' {} a -> s {quoteFields = a} :: CSVOutput)

-- | A value used to separate individual records from each other.
cSVOutput_recordDelimiter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_recordDelimiter = Lens.lens (\CSVOutput' {recordDelimiter} -> recordDelimiter) (\s@CSVOutput' {} a -> s {recordDelimiter = a} :: CSVOutput)

instance Data.FromJSON CSVOutput where
  parseJSON =
    Data.withObject
      "CSVOutput"
      ( \x ->
          CSVOutput'
            Prelude.<$> (x Data..:? "FieldDelimiter")
            Prelude.<*> (x Data..:? "QuoteCharacter")
            Prelude.<*> (x Data..:? "QuoteEscapeCharacter")
            Prelude.<*> (x Data..:? "QuoteFields")
            Prelude.<*> (x Data..:? "RecordDelimiter")
      )

instance Prelude.Hashable CSVOutput where
  hashWithSalt _salt CSVOutput' {..} =
    _salt `Prelude.hashWithSalt` fieldDelimiter
      `Prelude.hashWithSalt` quoteCharacter
      `Prelude.hashWithSalt` quoteEscapeCharacter
      `Prelude.hashWithSalt` quoteFields
      `Prelude.hashWithSalt` recordDelimiter

instance Prelude.NFData CSVOutput where
  rnf CSVOutput' {..} =
    Prelude.rnf fieldDelimiter
      `Prelude.seq` Prelude.rnf quoteCharacter
      `Prelude.seq` Prelude.rnf quoteEscapeCharacter
      `Prelude.seq` Prelude.rnf quoteFields
      `Prelude.seq` Prelude.rnf recordDelimiter

instance Data.ToJSON CSVOutput where
  toJSON CSVOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldDelimiter" Data..=)
              Prelude.<$> fieldDelimiter,
            ("QuoteCharacter" Data..=)
              Prelude.<$> quoteCharacter,
            ("QuoteEscapeCharacter" Data..=)
              Prelude.<$> quoteEscapeCharacter,
            ("QuoteFields" Data..=) Prelude.<$> quoteFields,
            ("RecordDelimiter" Data..=)
              Prelude.<$> recordDelimiter
          ]
      )

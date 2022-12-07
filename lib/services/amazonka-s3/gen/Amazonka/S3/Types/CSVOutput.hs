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
-- Module      : Amazonka.S3.Types.CSVOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CSVOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.QuoteFields

-- | Describes how uncompressed comma-separated values (CSV)-formatted
-- results are formatted.
--
-- /See:/ 'newCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { -- | A single character used for escaping when the field delimiter is part of
    -- the value. For example, if the value is @a, b@, Amazon S3 wraps this
    -- field value in quotation marks, as follows: @\" a , b \"@.
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use quotation marks around output fields.
    --
    -- -   @ALWAYS@: Always use quotation marks for output fields.
    --
    -- -   @ASNEEDED@: Use quotation marks for output fields when needed.
    quoteFields :: Prelude.Maybe QuoteFields,
    -- | The single character used for escaping the quote character inside an
    -- already escaped value.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text,
    -- | The value used to separate individual fields in a record. You can
    -- specify an arbitrary delimiter.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A single character used to separate individual records in the output.
    -- Instead of the default value, you can specify an arbitrary delimiter.
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
-- 'quoteCharacter', 'cSVOutput_quoteCharacter' - A single character used for escaping when the field delimiter is part of
-- the value. For example, if the value is @a, b@, Amazon S3 wraps this
-- field value in quotation marks, as follows: @\" a , b \"@.
--
-- 'quoteFields', 'cSVOutput_quoteFields' - Indicates whether to use quotation marks around output fields.
--
-- -   @ALWAYS@: Always use quotation marks for output fields.
--
-- -   @ASNEEDED@: Use quotation marks for output fields when needed.
--
-- 'quoteEscapeCharacter', 'cSVOutput_quoteEscapeCharacter' - The single character used for escaping the quote character inside an
-- already escaped value.
--
-- 'fieldDelimiter', 'cSVOutput_fieldDelimiter' - The value used to separate individual fields in a record. You can
-- specify an arbitrary delimiter.
--
-- 'recordDelimiter', 'cSVOutput_recordDelimiter' - A single character used to separate individual records in the output.
-- Instead of the default value, you can specify an arbitrary delimiter.
newCSVOutput ::
  CSVOutput
newCSVOutput =
  CSVOutput'
    { quoteCharacter = Prelude.Nothing,
      quoteFields = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing,
      fieldDelimiter = Prelude.Nothing,
      recordDelimiter = Prelude.Nothing
    }

-- | A single character used for escaping when the field delimiter is part of
-- the value. For example, if the value is @a, b@, Amazon S3 wraps this
-- field value in quotation marks, as follows: @\" a , b \"@.
cSVOutput_quoteCharacter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_quoteCharacter = Lens.lens (\CSVOutput' {quoteCharacter} -> quoteCharacter) (\s@CSVOutput' {} a -> s {quoteCharacter = a} :: CSVOutput)

-- | Indicates whether to use quotation marks around output fields.
--
-- -   @ALWAYS@: Always use quotation marks for output fields.
--
-- -   @ASNEEDED@: Use quotation marks for output fields when needed.
cSVOutput_quoteFields :: Lens.Lens' CSVOutput (Prelude.Maybe QuoteFields)
cSVOutput_quoteFields = Lens.lens (\CSVOutput' {quoteFields} -> quoteFields) (\s@CSVOutput' {} a -> s {quoteFields = a} :: CSVOutput)

-- | The single character used for escaping the quote character inside an
-- already escaped value.
cSVOutput_quoteEscapeCharacter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_quoteEscapeCharacter = Lens.lens (\CSVOutput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVOutput' {} a -> s {quoteEscapeCharacter = a} :: CSVOutput)

-- | The value used to separate individual fields in a record. You can
-- specify an arbitrary delimiter.
cSVOutput_fieldDelimiter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_fieldDelimiter = Lens.lens (\CSVOutput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVOutput' {} a -> s {fieldDelimiter = a} :: CSVOutput)

-- | A single character used to separate individual records in the output.
-- Instead of the default value, you can specify an arbitrary delimiter.
cSVOutput_recordDelimiter :: Lens.Lens' CSVOutput (Prelude.Maybe Prelude.Text)
cSVOutput_recordDelimiter = Lens.lens (\CSVOutput' {recordDelimiter} -> recordDelimiter) (\s@CSVOutput' {} a -> s {recordDelimiter = a} :: CSVOutput)

instance Prelude.Hashable CSVOutput where
  hashWithSalt _salt CSVOutput' {..} =
    _salt `Prelude.hashWithSalt` quoteCharacter
      `Prelude.hashWithSalt` quoteFields
      `Prelude.hashWithSalt` quoteEscapeCharacter
      `Prelude.hashWithSalt` fieldDelimiter
      `Prelude.hashWithSalt` recordDelimiter

instance Prelude.NFData CSVOutput where
  rnf CSVOutput' {..} =
    Prelude.rnf quoteCharacter
      `Prelude.seq` Prelude.rnf quoteFields
      `Prelude.seq` Prelude.rnf quoteEscapeCharacter
      `Prelude.seq` Prelude.rnf fieldDelimiter
      `Prelude.seq` Prelude.rnf recordDelimiter

instance Data.ToXML CSVOutput where
  toXML CSVOutput' {..} =
    Prelude.mconcat
      [ "QuoteCharacter" Data.@= quoteCharacter,
        "QuoteFields" Data.@= quoteFields,
        "QuoteEscapeCharacter" Data.@= quoteEscapeCharacter,
        "FieldDelimiter" Data.@= fieldDelimiter,
        "RecordDelimiter" Data.@= recordDelimiter
      ]

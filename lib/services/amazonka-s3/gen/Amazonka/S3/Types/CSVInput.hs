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
-- Module      : Amazonka.S3.Types.CSVInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CSVInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.FileHeaderInfo

-- | Describes how an uncompressed comma-separated values (CSV)-formatted
-- input object is formatted.
--
-- /See:/ 'newCSVInput' smart constructor.
data CSVInput = CSVInput'
  { -- | A single character used for escaping when the field delimiter is part of
    -- the value. For example, if the value is @a, b@, Amazon S3 wraps this
    -- field value in quotation marks, as follows: @\" a , b \"@.
    --
    -- Type: String
    --
    -- Default: @\"@
    --
    -- Ancestors: @CSV@
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | Specifies that CSV field values may contain quoted record delimiters and
    -- such records should be allowed. Default value is FALSE. Setting this
    -- value to TRUE may lower performance.
    allowQuotedRecordDelimiter :: Prelude.Maybe Prelude.Bool,
    -- | A single character used to indicate that a row should be ignored when
    -- the character is present at the start of that row. You can specify any
    -- character to indicate a comment line.
    comments :: Prelude.Maybe Prelude.Text,
    -- | A single character used for escaping the quotation mark character inside
    -- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
    -- is parsed as @\" a , b \"@.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text,
    -- | A single character used to separate individual fields in a record. You
    -- can specify an arbitrary delimiter.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A single character used to separate individual records in the input.
    -- Instead of the default value, you can specify an arbitrary delimiter.
    recordDelimiter :: Prelude.Maybe Prelude.Text,
    -- | Describes the first line of input. Valid values are:
    --
    -- -   @NONE@: First line is not a header.
    --
    -- -   @IGNORE@: First line is a header, but you can\'t use the header
    --     values to indicate the column in an expression. You can use column
    --     position (such as _1, _2, …) to indicate the column
    --     (@SELECT s._1 FROM OBJECT s@).
    --
    -- -   @Use@: First line is a header, and you can use the header value to
    --     identify a column in an expression (@SELECT \"name\" FROM OBJECT@).
    fileHeaderInfo :: Prelude.Maybe FileHeaderInfo
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
-- 'quoteCharacter', 'cSVInput_quoteCharacter' - A single character used for escaping when the field delimiter is part of
-- the value. For example, if the value is @a, b@, Amazon S3 wraps this
-- field value in quotation marks, as follows: @\" a , b \"@.
--
-- Type: String
--
-- Default: @\"@
--
-- Ancestors: @CSV@
--
-- 'allowQuotedRecordDelimiter', 'cSVInput_allowQuotedRecordDelimiter' - Specifies that CSV field values may contain quoted record delimiters and
-- such records should be allowed. Default value is FALSE. Setting this
-- value to TRUE may lower performance.
--
-- 'comments', 'cSVInput_comments' - A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row. You can specify any
-- character to indicate a comment line.
--
-- 'quoteEscapeCharacter', 'cSVInput_quoteEscapeCharacter' - A single character used for escaping the quotation mark character inside
-- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
-- is parsed as @\" a , b \"@.
--
-- 'fieldDelimiter', 'cSVInput_fieldDelimiter' - A single character used to separate individual fields in a record. You
-- can specify an arbitrary delimiter.
--
-- 'recordDelimiter', 'cSVInput_recordDelimiter' - A single character used to separate individual records in the input.
-- Instead of the default value, you can specify an arbitrary delimiter.
--
-- 'fileHeaderInfo', 'cSVInput_fileHeaderInfo' - Describes the first line of input. Valid values are:
--
-- -   @NONE@: First line is not a header.
--
-- -   @IGNORE@: First line is a header, but you can\'t use the header
--     values to indicate the column in an expression. You can use column
--     position (such as _1, _2, …) to indicate the column
--     (@SELECT s._1 FROM OBJECT s@).
--
-- -   @Use@: First line is a header, and you can use the header value to
--     identify a column in an expression (@SELECT \"name\" FROM OBJECT@).
newCSVInput ::
  CSVInput
newCSVInput =
  CSVInput'
    { quoteCharacter = Prelude.Nothing,
      allowQuotedRecordDelimiter = Prelude.Nothing,
      comments = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing,
      fieldDelimiter = Prelude.Nothing,
      recordDelimiter = Prelude.Nothing,
      fileHeaderInfo = Prelude.Nothing
    }

-- | A single character used for escaping when the field delimiter is part of
-- the value. For example, if the value is @a, b@, Amazon S3 wraps this
-- field value in quotation marks, as follows: @\" a , b \"@.
--
-- Type: String
--
-- Default: @\"@
--
-- Ancestors: @CSV@
cSVInput_quoteCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteCharacter = Lens.lens (\CSVInput' {quoteCharacter} -> quoteCharacter) (\s@CSVInput' {} a -> s {quoteCharacter = a} :: CSVInput)

-- | Specifies that CSV field values may contain quoted record delimiters and
-- such records should be allowed. Default value is FALSE. Setting this
-- value to TRUE may lower performance.
cSVInput_allowQuotedRecordDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Bool)
cSVInput_allowQuotedRecordDelimiter = Lens.lens (\CSVInput' {allowQuotedRecordDelimiter} -> allowQuotedRecordDelimiter) (\s@CSVInput' {} a -> s {allowQuotedRecordDelimiter = a} :: CSVInput)

-- | A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row. You can specify any
-- character to indicate a comment line.
cSVInput_comments :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_comments = Lens.lens (\CSVInput' {comments} -> comments) (\s@CSVInput' {} a -> s {comments = a} :: CSVInput)

-- | A single character used for escaping the quotation mark character inside
-- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
-- is parsed as @\" a , b \"@.
cSVInput_quoteEscapeCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteEscapeCharacter = Lens.lens (\CSVInput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVInput' {} a -> s {quoteEscapeCharacter = a} :: CSVInput)

-- | A single character used to separate individual fields in a record. You
-- can specify an arbitrary delimiter.
cSVInput_fieldDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_fieldDelimiter = Lens.lens (\CSVInput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVInput' {} a -> s {fieldDelimiter = a} :: CSVInput)

-- | A single character used to separate individual records in the input.
-- Instead of the default value, you can specify an arbitrary delimiter.
cSVInput_recordDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_recordDelimiter = Lens.lens (\CSVInput' {recordDelimiter} -> recordDelimiter) (\s@CSVInput' {} a -> s {recordDelimiter = a} :: CSVInput)

-- | Describes the first line of input. Valid values are:
--
-- -   @NONE@: First line is not a header.
--
-- -   @IGNORE@: First line is a header, but you can\'t use the header
--     values to indicate the column in an expression. You can use column
--     position (such as _1, _2, …) to indicate the column
--     (@SELECT s._1 FROM OBJECT s@).
--
-- -   @Use@: First line is a header, and you can use the header value to
--     identify a column in an expression (@SELECT \"name\" FROM OBJECT@).
cSVInput_fileHeaderInfo :: Lens.Lens' CSVInput (Prelude.Maybe FileHeaderInfo)
cSVInput_fileHeaderInfo = Lens.lens (\CSVInput' {fileHeaderInfo} -> fileHeaderInfo) (\s@CSVInput' {} a -> s {fileHeaderInfo = a} :: CSVInput)

instance Prelude.Hashable CSVInput where
  hashWithSalt _salt CSVInput' {..} =
    _salt `Prelude.hashWithSalt` quoteCharacter
      `Prelude.hashWithSalt` allowQuotedRecordDelimiter
      `Prelude.hashWithSalt` comments
      `Prelude.hashWithSalt` quoteEscapeCharacter
      `Prelude.hashWithSalt` fieldDelimiter
      `Prelude.hashWithSalt` recordDelimiter
      `Prelude.hashWithSalt` fileHeaderInfo

instance Prelude.NFData CSVInput where
  rnf CSVInput' {..} =
    Prelude.rnf quoteCharacter
      `Prelude.seq` Prelude.rnf allowQuotedRecordDelimiter
      `Prelude.seq` Prelude.rnf comments
      `Prelude.seq` Prelude.rnf quoteEscapeCharacter
      `Prelude.seq` Prelude.rnf fieldDelimiter
      `Prelude.seq` Prelude.rnf recordDelimiter
      `Prelude.seq` Prelude.rnf fileHeaderInfo

instance Data.ToXML CSVInput where
  toXML CSVInput' {..} =
    Prelude.mconcat
      [ "QuoteCharacter" Data.@= quoteCharacter,
        "AllowQuotedRecordDelimiter"
          Data.@= allowQuotedRecordDelimiter,
        "Comments" Data.@= comments,
        "QuoteEscapeCharacter" Data.@= quoteEscapeCharacter,
        "FieldDelimiter" Data.@= fieldDelimiter,
        "RecordDelimiter" Data.@= recordDelimiter,
        "FileHeaderInfo" Data.@= fileHeaderInfo
      ]

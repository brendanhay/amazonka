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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | Specifies that CSV field values may contain quoted record delimiters and
    -- such records should be allowed. Default value is FALSE. Setting this
    -- value to TRUE may lower performance.
    allowQuotedRecordDelimiter :: Prelude.Maybe Prelude.Bool,
    -- | A single character used to indicate that a row should be ignored when
    -- the character is present at the start of that row. You can specify any
    -- character to indicate a comment line.
    comments :: Prelude.Maybe Prelude.Text,
    -- | A single character used to separate individual fields in a record. You
    -- can specify an arbitrary delimiter.
    fieldDelimiter :: Prelude.Maybe Prelude.Text,
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
    fileHeaderInfo :: Prelude.Maybe FileHeaderInfo,
    -- | A single character used for escaping when the field delimiter is part of
    -- the value. For example, if the value is @a, b@, Amazon S3 wraps this
    -- field value in quotation marks, as follows: @\" a , b \"@.
    --
    -- Type: String
    --
    -- Default: @\"@
    --
    -- Ancestors: @CSV@
    quoteCharacter :: Prelude.Maybe Prelude.Text,
    -- | A single character used for escaping the quotation mark character inside
    -- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
    -- is parsed as @\" a , b \"@.
    quoteEscapeCharacter :: Prelude.Maybe Prelude.Text,
    -- | A single character used to separate individual records in the input.
    -- Instead of the default value, you can specify an arbitrary delimiter.
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
-- 'allowQuotedRecordDelimiter', 'cSVInput_allowQuotedRecordDelimiter' - Specifies that CSV field values may contain quoted record delimiters and
-- such records should be allowed. Default value is FALSE. Setting this
-- value to TRUE may lower performance.
--
-- 'comments', 'cSVInput_comments' - A single character used to indicate that a row should be ignored when
-- the character is present at the start of that row. You can specify any
-- character to indicate a comment line.
--
-- 'fieldDelimiter', 'cSVInput_fieldDelimiter' - A single character used to separate individual fields in a record. You
-- can specify an arbitrary delimiter.
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
-- 'quoteEscapeCharacter', 'cSVInput_quoteEscapeCharacter' - A single character used for escaping the quotation mark character inside
-- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
-- is parsed as @\" a , b \"@.
--
-- 'recordDelimiter', 'cSVInput_recordDelimiter' - A single character used to separate individual records in the input.
-- Instead of the default value, you can specify an arbitrary delimiter.
newCSVInput ::
  CSVInput
newCSVInput =
  CSVInput'
    { allowQuotedRecordDelimiter =
        Prelude.Nothing,
      comments = Prelude.Nothing,
      fieldDelimiter = Prelude.Nothing,
      fileHeaderInfo = Prelude.Nothing,
      quoteCharacter = Prelude.Nothing,
      quoteEscapeCharacter = Prelude.Nothing,
      recordDelimiter = Prelude.Nothing
    }

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

-- | A single character used to separate individual fields in a record. You
-- can specify an arbitrary delimiter.
cSVInput_fieldDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_fieldDelimiter = Lens.lens (\CSVInput' {fieldDelimiter} -> fieldDelimiter) (\s@CSVInput' {} a -> s {fieldDelimiter = a} :: CSVInput)

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

-- | A single character used for escaping the quotation mark character inside
-- an already escaped value. For example, the value @\"\"\" a , b \"\"\"@
-- is parsed as @\" a , b \"@.
cSVInput_quoteEscapeCharacter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_quoteEscapeCharacter = Lens.lens (\CSVInput' {quoteEscapeCharacter} -> quoteEscapeCharacter) (\s@CSVInput' {} a -> s {quoteEscapeCharacter = a} :: CSVInput)

-- | A single character used to separate individual records in the input.
-- Instead of the default value, you can specify an arbitrary delimiter.
cSVInput_recordDelimiter :: Lens.Lens' CSVInput (Prelude.Maybe Prelude.Text)
cSVInput_recordDelimiter = Lens.lens (\CSVInput' {recordDelimiter} -> recordDelimiter) (\s@CSVInput' {} a -> s {recordDelimiter = a} :: CSVInput)

instance Prelude.Hashable CSVInput where
  hashWithSalt _salt CSVInput' {..} =
    _salt
      `Prelude.hashWithSalt` allowQuotedRecordDelimiter
      `Prelude.hashWithSalt` comments
      `Prelude.hashWithSalt` fieldDelimiter
      `Prelude.hashWithSalt` fileHeaderInfo
      `Prelude.hashWithSalt` quoteCharacter
      `Prelude.hashWithSalt` quoteEscapeCharacter
      `Prelude.hashWithSalt` recordDelimiter

instance Prelude.NFData CSVInput where
  rnf CSVInput' {..} =
    Prelude.rnf allowQuotedRecordDelimiter `Prelude.seq`
      Prelude.rnf comments `Prelude.seq`
        Prelude.rnf fieldDelimiter `Prelude.seq`
          Prelude.rnf fileHeaderInfo `Prelude.seq`
            Prelude.rnf quoteCharacter `Prelude.seq`
              Prelude.rnf quoteEscapeCharacter `Prelude.seq`
                Prelude.rnf recordDelimiter

instance Data.ToXML CSVInput where
  toXML CSVInput' {..} =
    Prelude.mconcat
      [ "AllowQuotedRecordDelimiter"
          Data.@= allowQuotedRecordDelimiter,
        "Comments" Data.@= comments,
        "FieldDelimiter" Data.@= fieldDelimiter,
        "FileHeaderInfo" Data.@= fileHeaderInfo,
        "QuoteCharacter" Data.@= quoteCharacter,
        "QuoteEscapeCharacter" Data.@= quoteEscapeCharacter,
        "RecordDelimiter" Data.@= recordDelimiter
      ]

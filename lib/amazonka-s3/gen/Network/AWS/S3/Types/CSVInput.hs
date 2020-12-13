{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CSVInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CSVInput
  ( CSVInput (..),

    -- * Smart constructor
    mkCSVInput,

    -- * Lenses
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciAllowQuotedRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FileHeaderInfo

-- | Describes how an uncompressed comma-separated values (CSV)-formatted input object is formatted.
--
-- /See:/ 'mkCSVInput' smart constructor.
data CSVInput = CSVInput'
  { -- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
    --
    -- Type: String
    -- Default: @"@
    -- Ancestors: @CSV@
    quoteCharacter :: Lude.Maybe Lude.Text,
    -- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
    recordDelimiter :: Lude.Maybe Lude.Text,
    -- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
    allowQuotedRecordDelimiter :: Lude.Maybe Lude.Bool,
    -- | Describes the first line of input. Valid values are:
    --
    --
    --     * @NONE@ : First line is not a header.
    --
    --
    --     * @IGNORE@ : First line is a header, but you can't use the header values to indicate the column in an expression. You can use column position (such as _1, _2, …) to indicate the column (@SELECT s._1 FROM OBJECT s@ ).
    --
    --
    --     * @Use@ : First line is a header, and you can use the header value to identify a column in an expression (@SELECT "name" FROM OBJECT@ ).
    fileHeaderInfo :: Lude.Maybe FileHeaderInfo,
    -- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
    quoteEscapeCharacter :: Lude.Maybe Lude.Text,
    -- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
    comments :: Lude.Maybe Lude.Text,
    -- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
    fieldDelimiter :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVInput' with the minimum fields required to make a request.
--
-- * 'quoteCharacter' - A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- Type: String
-- Default: @"@
-- Ancestors: @CSV@
-- * 'recordDelimiter' - A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
-- * 'allowQuotedRecordDelimiter' - Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
-- * 'fileHeaderInfo' - Describes the first line of input. Valid values are:
--
--
--     * @NONE@ : First line is not a header.
--
--
--     * @IGNORE@ : First line is a header, but you can't use the header values to indicate the column in an expression. You can use column position (such as _1, _2, …) to indicate the column (@SELECT s._1 FROM OBJECT s@ ).
--
--
--     * @Use@ : First line is a header, and you can use the header value to identify a column in an expression (@SELECT "name" FROM OBJECT@ ).
--
--
-- * 'quoteEscapeCharacter' - A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
-- * 'comments' - A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
-- * 'fieldDelimiter' - A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
mkCSVInput ::
  CSVInput
mkCSVInput =
  CSVInput'
    { quoteCharacter = Lude.Nothing,
      recordDelimiter = Lude.Nothing,
      allowQuotedRecordDelimiter = Lude.Nothing,
      fileHeaderInfo = Lude.Nothing,
      quoteEscapeCharacter = Lude.Nothing,
      comments = Lude.Nothing,
      fieldDelimiter = Lude.Nothing
    }

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- Type: String
-- Default: @"@
-- Ancestors: @CSV@
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciQuoteCharacter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciQuoteCharacter = Lens.lens (quoteCharacter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {quoteCharacter = a} :: CSVInput)
{-# DEPRECATED ciQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRecordDelimiter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciRecordDelimiter = Lens.lens (recordDelimiter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {recordDelimiter = a} :: CSVInput)
{-# DEPRECATED ciRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

-- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
--
-- /Note:/ Consider using 'allowQuotedRecordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAllowQuotedRecordDelimiter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Bool)
ciAllowQuotedRecordDelimiter = Lens.lens (allowQuotedRecordDelimiter :: CSVInput -> Lude.Maybe Lude.Bool) (\s a -> s {allowQuotedRecordDelimiter = a} :: CSVInput)
{-# DEPRECATED ciAllowQuotedRecordDelimiter "Use generic-lens or generic-optics with 'allowQuotedRecordDelimiter' instead." #-}

-- | Describes the first line of input. Valid values are:
--
--
--     * @NONE@ : First line is not a header.
--
--
--     * @IGNORE@ : First line is a header, but you can't use the header values to indicate the column in an expression. You can use column position (such as _1, _2, …) to indicate the column (@SELECT s._1 FROM OBJECT s@ ).
--
--
--     * @Use@ : First line is a header, and you can use the header value to identify a column in an expression (@SELECT "name" FROM OBJECT@ ).
--
--
--
-- /Note:/ Consider using 'fileHeaderInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciFileHeaderInfo :: Lens.Lens' CSVInput (Lude.Maybe FileHeaderInfo)
ciFileHeaderInfo = Lens.lens (fileHeaderInfo :: CSVInput -> Lude.Maybe FileHeaderInfo) (\s a -> s {fileHeaderInfo = a} :: CSVInput)
{-# DEPRECATED ciFileHeaderInfo "Use generic-lens or generic-optics with 'fileHeaderInfo' instead." #-}

-- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciQuoteEscapeCharacter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciQuoteEscapeCharacter = Lens.lens (quoteEscapeCharacter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {quoteEscapeCharacter = a} :: CSVInput)
{-# DEPRECATED ciQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComments :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciComments = Lens.lens (comments :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {comments = a} :: CSVInput)
{-# DEPRECATED ciComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciFieldDelimiter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciFieldDelimiter = Lens.lens (fieldDelimiter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {fieldDelimiter = a} :: CSVInput)
{-# DEPRECATED ciFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

instance Lude.ToXML CSVInput where
  toXML CSVInput' {..} =
    Lude.mconcat
      [ "QuoteCharacter" Lude.@= quoteCharacter,
        "RecordDelimiter" Lude.@= recordDelimiter,
        "AllowQuotedRecordDelimiter" Lude.@= allowQuotedRecordDelimiter,
        "FileHeaderInfo" Lude.@= fileHeaderInfo,
        "QuoteEscapeCharacter" Lude.@= quoteEscapeCharacter,
        "Comments" Lude.@= comments,
        "FieldDelimiter" Lude.@= fieldDelimiter
      ]

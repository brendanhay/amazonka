{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CSVOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CSVOutput
  ( CSVOutput (..),

    -- * Smart constructor
    mkCSVOutput,

    -- * Lenses
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.QuoteFields

-- | Describes how uncompressed comma-separated values (CSV)-formatted results are formatted.
--
-- /See:/ 'mkCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { -- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
    quoteCharacter :: Lude.Maybe Lude.Text,
    -- | Indicates whether to use quotation marks around output fields.
    --
    --
    --     * @ALWAYS@ : Always use quotation marks for output fields.
    --
    --
    --     * @ASNEEDED@ : Use quotation marks for output fields when needed.
    quoteFields :: Lude.Maybe QuoteFields,
    -- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
    recordDelimiter :: Lude.Maybe Lude.Text,
    -- | The single character used for escaping the quote character inside an already escaped value.
    quoteEscapeCharacter :: Lude.Maybe Lude.Text,
    -- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
    fieldDelimiter :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVOutput' with the minimum fields required to make a request.
--
-- * 'quoteCharacter' - A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
-- * 'quoteFields' - Indicates whether to use quotation marks around output fields.
--
--
--     * @ALWAYS@ : Always use quotation marks for output fields.
--
--
--     * @ASNEEDED@ : Use quotation marks for output fields when needed.
--
--
-- * 'recordDelimiter' - A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
-- * 'quoteEscapeCharacter' - The single character used for escaping the quote character inside an already escaped value.
-- * 'fieldDelimiter' - The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
mkCSVOutput ::
  CSVOutput
mkCSVOutput =
  CSVOutput'
    { quoteCharacter = Lude.Nothing,
      quoteFields = Lude.Nothing,
      recordDelimiter = Lude.Nothing,
      quoteEscapeCharacter = Lude.Nothing,
      fieldDelimiter = Lude.Nothing
    }

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteCharacter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coQuoteCharacter = Lens.lens (quoteCharacter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {quoteCharacter = a} :: CSVOutput)
{-# DEPRECATED coQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | Indicates whether to use quotation marks around output fields.
--
--
--     * @ALWAYS@ : Always use quotation marks for output fields.
--
--
--     * @ASNEEDED@ : Use quotation marks for output fields when needed.
--
--
--
-- /Note:/ Consider using 'quoteFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteFields :: Lens.Lens' CSVOutput (Lude.Maybe QuoteFields)
coQuoteFields = Lens.lens (quoteFields :: CSVOutput -> Lude.Maybe QuoteFields) (\s a -> s {quoteFields = a} :: CSVOutput)
{-# DEPRECATED coQuoteFields "Use generic-lens or generic-optics with 'quoteFields' instead." #-}

-- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRecordDelimiter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coRecordDelimiter = Lens.lens (recordDelimiter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {recordDelimiter = a} :: CSVOutput)
{-# DEPRECATED coRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

-- | The single character used for escaping the quote character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteEscapeCharacter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coQuoteEscapeCharacter = Lens.lens (quoteEscapeCharacter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {quoteEscapeCharacter = a} :: CSVOutput)
{-# DEPRECATED coQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

-- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coFieldDelimiter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coFieldDelimiter = Lens.lens (fieldDelimiter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {fieldDelimiter = a} :: CSVOutput)
{-# DEPRECATED coFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

instance Lude.ToXML CSVOutput where
  toXML CSVOutput' {..} =
    Lude.mconcat
      [ "QuoteCharacter" Lude.@= quoteCharacter,
        "QuoteFields" Lude.@= quoteFields,
        "RecordDelimiter" Lude.@= recordDelimiter,
        "QuoteEscapeCharacter" Lude.@= quoteEscapeCharacter,
        "FieldDelimiter" Lude.@= fieldDelimiter
      ]

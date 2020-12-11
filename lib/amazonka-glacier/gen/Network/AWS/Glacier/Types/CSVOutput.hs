-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CSVOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CSVOutput
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

import Network.AWS.Glacier.Types.QuoteFields
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the comma-separated value (CSV) file that the job results are stored in.
--
-- /See:/ 'mkCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { quoteCharacter :: Lude.Maybe Lude.Text,
    quoteFields :: Lude.Maybe QuoteFields,
    recordDelimiter :: Lude.Maybe Lude.Text,
    quoteEscapeCharacter :: Lude.Maybe Lude.Text,
    fieldDelimiter :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVOutput' with the minimum fields required to make a request.
--
-- * 'fieldDelimiter' - A value used to separate individual fields from each other within a record.
-- * 'quoteCharacter' - A value used as an escape character where the field delimiter is part of the value.
-- * 'quoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside an already escaped value.
-- * 'quoteFields' - A value that indicates whether all output fields should be contained within quotation marks.
-- * 'recordDelimiter' - A value used to separate individual records from each other.
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

-- | A value used as an escape character where the field delimiter is part of the value.
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteCharacter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coQuoteCharacter = Lens.lens (quoteCharacter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {quoteCharacter = a} :: CSVOutput)
{-# DEPRECATED coQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | A value that indicates whether all output fields should be contained within quotation marks.
--
-- /Note:/ Consider using 'quoteFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteFields :: Lens.Lens' CSVOutput (Lude.Maybe QuoteFields)
coQuoteFields = Lens.lens (quoteFields :: CSVOutput -> Lude.Maybe QuoteFields) (\s a -> s {quoteFields = a} :: CSVOutput)
{-# DEPRECATED coQuoteFields "Use generic-lens or generic-optics with 'quoteFields' instead." #-}

-- | A value used to separate individual records from each other.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRecordDelimiter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coRecordDelimiter = Lens.lens (recordDelimiter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {recordDelimiter = a} :: CSVOutput)
{-# DEPRECATED coRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coQuoteEscapeCharacter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coQuoteEscapeCharacter = Lens.lens (quoteEscapeCharacter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {quoteEscapeCharacter = a} :: CSVOutput)
{-# DEPRECATED coQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

-- | A value used to separate individual fields from each other within a record.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coFieldDelimiter :: Lens.Lens' CSVOutput (Lude.Maybe Lude.Text)
coFieldDelimiter = Lens.lens (fieldDelimiter :: CSVOutput -> Lude.Maybe Lude.Text) (\s a -> s {fieldDelimiter = a} :: CSVOutput)
{-# DEPRECATED coFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

instance Lude.FromJSON CSVOutput where
  parseJSON =
    Lude.withObject
      "CSVOutput"
      ( \x ->
          CSVOutput'
            Lude.<$> (x Lude..:? "QuoteCharacter")
            Lude.<*> (x Lude..:? "QuoteFields")
            Lude.<*> (x Lude..:? "RecordDelimiter")
            Lude.<*> (x Lude..:? "QuoteEscapeCharacter")
            Lude.<*> (x Lude..:? "FieldDelimiter")
      )

instance Lude.ToJSON CSVOutput where
  toJSON CSVOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QuoteCharacter" Lude..=) Lude.<$> quoteCharacter,
            ("QuoteFields" Lude..=) Lude.<$> quoteFields,
            ("RecordDelimiter" Lude..=) Lude.<$> recordDelimiter,
            ("QuoteEscapeCharacter" Lude..=) Lude.<$> quoteEscapeCharacter,
            ("FieldDelimiter" Lude..=) Lude.<$> fieldDelimiter
          ]
      )

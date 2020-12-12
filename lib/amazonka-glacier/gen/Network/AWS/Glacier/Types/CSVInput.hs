{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CSVInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CSVInput
  ( CSVInput (..),

    -- * Smart constructor
    mkCSVInput,

    -- * Lenses
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,
  )
where

import Network.AWS.Glacier.Types.FileHeaderInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the comma-separated value (CSV) file to select from.
--
-- /See:/ 'mkCSVInput' smart constructor.
data CSVInput = CSVInput'
  { quoteCharacter :: Lude.Maybe Lude.Text,
    recordDelimiter :: Lude.Maybe Lude.Text,
    fileHeaderInfo :: Lude.Maybe FileHeaderInfo,
    quoteEscapeCharacter :: Lude.Maybe Lude.Text,
    comments :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CSVInput' with the minimum fields required to make a request.
--
-- * 'comments' - A single character used to indicate that a row should be ignored when the character is present at the start of that row.
-- * 'fieldDelimiter' - A value used to separate individual fields from each other within a record.
-- * 'fileHeaderInfo' - Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
-- * 'quoteCharacter' - A value used as an escape character where the field delimiter is part of the value.
-- * 'quoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside an already escaped value.
-- * 'recordDelimiter' - A value used to separate individual records from each other.
mkCSVInput ::
  CSVInput
mkCSVInput =
  CSVInput'
    { quoteCharacter = Lude.Nothing,
      recordDelimiter = Lude.Nothing,
      fileHeaderInfo = Lude.Nothing,
      quoteEscapeCharacter = Lude.Nothing,
      comments = Lude.Nothing,
      fieldDelimiter = Lude.Nothing
    }

-- | A value used as an escape character where the field delimiter is part of the value.
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciQuoteCharacter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciQuoteCharacter = Lens.lens (quoteCharacter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {quoteCharacter = a} :: CSVInput)
{-# DEPRECATED ciQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | A value used to separate individual records from each other.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRecordDelimiter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciRecordDelimiter = Lens.lens (recordDelimiter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {recordDelimiter = a} :: CSVInput)
{-# DEPRECATED ciRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

-- | Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
--
-- /Note:/ Consider using 'fileHeaderInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciFileHeaderInfo :: Lens.Lens' CSVInput (Lude.Maybe FileHeaderInfo)
ciFileHeaderInfo = Lens.lens (fileHeaderInfo :: CSVInput -> Lude.Maybe FileHeaderInfo) (\s a -> s {fileHeaderInfo = a} :: CSVInput)
{-# DEPRECATED ciFileHeaderInfo "Use generic-lens or generic-optics with 'fileHeaderInfo' instead." #-}

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciQuoteEscapeCharacter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciQuoteEscapeCharacter = Lens.lens (quoteEscapeCharacter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {quoteEscapeCharacter = a} :: CSVInput)
{-# DEPRECATED ciQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComments :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciComments = Lens.lens (comments :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {comments = a} :: CSVInput)
{-# DEPRECATED ciComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | A value used to separate individual fields from each other within a record.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciFieldDelimiter :: Lens.Lens' CSVInput (Lude.Maybe Lude.Text)
ciFieldDelimiter = Lens.lens (fieldDelimiter :: CSVInput -> Lude.Maybe Lude.Text) (\s a -> s {fieldDelimiter = a} :: CSVInput)
{-# DEPRECATED ciFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

instance Lude.FromJSON CSVInput where
  parseJSON =
    Lude.withObject
      "CSVInput"
      ( \x ->
          CSVInput'
            Lude.<$> (x Lude..:? "QuoteCharacter")
            Lude.<*> (x Lude..:? "RecordDelimiter")
            Lude.<*> (x Lude..:? "FileHeaderInfo")
            Lude.<*> (x Lude..:? "QuoteEscapeCharacter")
            Lude.<*> (x Lude..:? "Comments")
            Lude.<*> (x Lude..:? "FieldDelimiter")
      )

instance Lude.ToJSON CSVInput where
  toJSON CSVInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QuoteCharacter" Lude..=) Lude.<$> quoteCharacter,
            ("RecordDelimiter" Lude..=) Lude.<$> recordDelimiter,
            ("FileHeaderInfo" Lude..=) Lude.<$> fileHeaderInfo,
            ("QuoteEscapeCharacter" Lude..=) Lude.<$> quoteEscapeCharacter,
            ("Comments" Lude..=) Lude.<$> comments,
            ("FieldDelimiter" Lude..=) Lude.<$> fieldDelimiter
          ]
      )

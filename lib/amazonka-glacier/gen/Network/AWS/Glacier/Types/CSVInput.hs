{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CSVInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.CSVInput
  ( CSVInput (..)
  -- * Smart constructor
  , mkCSVInput
  -- * Lenses
  , csviComments
  , csviFieldDelimiter
  , csviFileHeaderInfo
  , csviQuoteCharacter
  , csviQuoteEscapeCharacter
  , csviRecordDelimiter
  ) where

import qualified Network.AWS.Glacier.Types.FileHeaderInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the comma-separated value (CSV) file to select from.
--
-- /See:/ 'mkCSVInput' smart constructor.
data CSVInput = CSVInput'
  { comments :: Core.Maybe Core.Text
    -- ^ A single character used to indicate that a row should be ignored when the character is present at the start of that row.
  , fieldDelimiter :: Core.Maybe Core.Text
    -- ^ A value used to separate individual fields from each other within a record.
  , fileHeaderInfo :: Core.Maybe Types.FileHeaderInfo
    -- ^ Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
  , quoteCharacter :: Core.Maybe Core.Text
    -- ^ A value used as an escape character where the field delimiter is part of the value.
  , quoteEscapeCharacter :: Core.Maybe Core.Text
    -- ^ A single character used for escaping the quotation-mark character inside an already escaped value.
  , recordDelimiter :: Core.Maybe Core.Text
    -- ^ A value used to separate individual records from each other.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVInput' value with any optional fields omitted.
mkCSVInput
    :: CSVInput
mkCSVInput
  = CSVInput'{comments = Core.Nothing, fieldDelimiter = Core.Nothing,
              fileHeaderInfo = Core.Nothing, quoteCharacter = Core.Nothing,
              quoteEscapeCharacter = Core.Nothing,
              recordDelimiter = Core.Nothing}

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviComments :: Lens.Lens' CSVInput (Core.Maybe Core.Text)
csviComments = Lens.field @"comments"
{-# INLINEABLE csviComments #-}
{-# DEPRECATED comments "Use generic-lens or generic-optics with 'comments' instead"  #-}

-- | A value used to separate individual fields from each other within a record.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviFieldDelimiter :: Lens.Lens' CSVInput (Core.Maybe Core.Text)
csviFieldDelimiter = Lens.field @"fieldDelimiter"
{-# INLINEABLE csviFieldDelimiter #-}
{-# DEPRECATED fieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead"  #-}

-- | Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
--
-- /Note:/ Consider using 'fileHeaderInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviFileHeaderInfo :: Lens.Lens' CSVInput (Core.Maybe Types.FileHeaderInfo)
csviFileHeaderInfo = Lens.field @"fileHeaderInfo"
{-# INLINEABLE csviFileHeaderInfo #-}
{-# DEPRECATED fileHeaderInfo "Use generic-lens or generic-optics with 'fileHeaderInfo' instead"  #-}

-- | A value used as an escape character where the field delimiter is part of the value.
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteCharacter :: Lens.Lens' CSVInput (Core.Maybe Core.Text)
csviQuoteCharacter = Lens.field @"quoteCharacter"
{-# INLINEABLE csviQuoteCharacter #-}
{-# DEPRECATED quoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead"  #-}

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteEscapeCharacter :: Lens.Lens' CSVInput (Core.Maybe Core.Text)
csviQuoteEscapeCharacter = Lens.field @"quoteEscapeCharacter"
{-# INLINEABLE csviQuoteEscapeCharacter #-}
{-# DEPRECATED quoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead"  #-}

-- | A value used to separate individual records from each other.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviRecordDelimiter :: Lens.Lens' CSVInput (Core.Maybe Core.Text)
csviRecordDelimiter = Lens.field @"recordDelimiter"
{-# INLINEABLE csviRecordDelimiter #-}
{-# DEPRECATED recordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead"  #-}

instance Core.FromJSON CSVInput where
        toJSON CSVInput{..}
          = Core.object
              (Core.catMaybes
                 [("Comments" Core..=) Core.<$> comments,
                  ("FieldDelimiter" Core..=) Core.<$> fieldDelimiter,
                  ("FileHeaderInfo" Core..=) Core.<$> fileHeaderInfo,
                  ("QuoteCharacter" Core..=) Core.<$> quoteCharacter,
                  ("QuoteEscapeCharacter" Core..=) Core.<$> quoteEscapeCharacter,
                  ("RecordDelimiter" Core..=) Core.<$> recordDelimiter])

instance Core.FromJSON CSVInput where
        parseJSON
          = Core.withObject "CSVInput" Core.$
              \ x ->
                CSVInput' Core.<$>
                  (x Core..:? "Comments") Core.<*> x Core..:? "FieldDelimiter"
                    Core.<*> x Core..:? "FileHeaderInfo"
                    Core.<*> x Core..:? "QuoteCharacter"
                    Core.<*> x Core..:? "QuoteEscapeCharacter"
                    Core.<*> x Core..:? "RecordDelimiter"

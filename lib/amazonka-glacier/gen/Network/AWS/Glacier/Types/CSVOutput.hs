{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CSVOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.CSVOutput
  ( CSVOutput (..)
  -- * Smart constructor
  , mkCSVOutput
  -- * Lenses
  , csvoFieldDelimiter
  , csvoQuoteCharacter
  , csvoQuoteEscapeCharacter
  , csvoQuoteFields
  , csvoRecordDelimiter
  ) where

import qualified Network.AWS.Glacier.Types.QuoteFields as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the comma-separated value (CSV) file that the job results are stored in.
--
-- /See:/ 'mkCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { fieldDelimiter :: Core.Maybe Core.Text
    -- ^ A value used to separate individual fields from each other within a record.
  , quoteCharacter :: Core.Maybe Core.Text
    -- ^ A value used as an escape character where the field delimiter is part of the value.
  , quoteEscapeCharacter :: Core.Maybe Core.Text
    -- ^ A single character used for escaping the quotation-mark character inside an already escaped value.
  , quoteFields :: Core.Maybe Types.QuoteFields
    -- ^ A value that indicates whether all output fields should be contained within quotation marks.
  , recordDelimiter :: Core.Maybe Core.Text
    -- ^ A value used to separate individual records from each other.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVOutput' value with any optional fields omitted.
mkCSVOutput
    :: CSVOutput
mkCSVOutput
  = CSVOutput'{fieldDelimiter = Core.Nothing,
               quoteCharacter = Core.Nothing, quoteEscapeCharacter = Core.Nothing,
               quoteFields = Core.Nothing, recordDelimiter = Core.Nothing}

-- | A value used to separate individual fields from each other within a record.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoFieldDelimiter :: Lens.Lens' CSVOutput (Core.Maybe Core.Text)
csvoFieldDelimiter = Lens.field @"fieldDelimiter"
{-# INLINEABLE csvoFieldDelimiter #-}
{-# DEPRECATED fieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead"  #-}

-- | A value used as an escape character where the field delimiter is part of the value.
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoQuoteCharacter :: Lens.Lens' CSVOutput (Core.Maybe Core.Text)
csvoQuoteCharacter = Lens.field @"quoteCharacter"
{-# INLINEABLE csvoQuoteCharacter #-}
{-# DEPRECATED quoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead"  #-}

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoQuoteEscapeCharacter :: Lens.Lens' CSVOutput (Core.Maybe Core.Text)
csvoQuoteEscapeCharacter = Lens.field @"quoteEscapeCharacter"
{-# INLINEABLE csvoQuoteEscapeCharacter #-}
{-# DEPRECATED quoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead"  #-}

-- | A value that indicates whether all output fields should be contained within quotation marks.
--
-- /Note:/ Consider using 'quoteFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoQuoteFields :: Lens.Lens' CSVOutput (Core.Maybe Types.QuoteFields)
csvoQuoteFields = Lens.field @"quoteFields"
{-# INLINEABLE csvoQuoteFields #-}
{-# DEPRECATED quoteFields "Use generic-lens or generic-optics with 'quoteFields' instead"  #-}

-- | A value used to separate individual records from each other.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoRecordDelimiter :: Lens.Lens' CSVOutput (Core.Maybe Core.Text)
csvoRecordDelimiter = Lens.field @"recordDelimiter"
{-# INLINEABLE csvoRecordDelimiter #-}
{-# DEPRECATED recordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead"  #-}

instance Core.FromJSON CSVOutput where
        toJSON CSVOutput{..}
          = Core.object
              (Core.catMaybes
                 [("FieldDelimiter" Core..=) Core.<$> fieldDelimiter,
                  ("QuoteCharacter" Core..=) Core.<$> quoteCharacter,
                  ("QuoteEscapeCharacter" Core..=) Core.<$> quoteEscapeCharacter,
                  ("QuoteFields" Core..=) Core.<$> quoteFields,
                  ("RecordDelimiter" Core..=) Core.<$> recordDelimiter])

instance Core.FromJSON CSVOutput where
        parseJSON
          = Core.withObject "CSVOutput" Core.$
              \ x ->
                CSVOutput' Core.<$>
                  (x Core..:? "FieldDelimiter") Core.<*> x Core..:? "QuoteCharacter"
                    Core.<*> x Core..:? "QuoteEscapeCharacter"
                    Core.<*> x Core..:? "QuoteFields"
                    Core.<*> x Core..:? "RecordDelimiter"

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
    csvoFieldDelimiter,
    csvoQuoteCharacter,
    csvoQuoteEscapeCharacter,
    csvoQuoteFields,
    csvoRecordDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.FieldDelimiter as Types
import qualified Network.AWS.S3.Types.QuoteCharacter as Types
import qualified Network.AWS.S3.Types.QuoteEscapeCharacter as Types
import qualified Network.AWS.S3.Types.QuoteFields as Types
import qualified Network.AWS.S3.Types.RecordDelimiter as Types

-- | Describes how uncompressed comma-separated values (CSV)-formatted results are formatted.
--
-- /See:/ 'mkCSVOutput' smart constructor.
data CSVOutput = CSVOutput'
  { -- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
    fieldDelimiter :: Core.Maybe Types.FieldDelimiter,
    -- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
    quoteCharacter :: Core.Maybe Types.QuoteCharacter,
    -- | The single character used for escaping the quote character inside an already escaped value.
    quoteEscapeCharacter :: Core.Maybe Types.QuoteEscapeCharacter,
    -- | Indicates whether to use quotation marks around output fields.
    --
    --
    --     * @ALWAYS@ : Always use quotation marks for output fields.
    --
    --
    --     * @ASNEEDED@ : Use quotation marks for output fields when needed.
    quoteFields :: Core.Maybe Types.QuoteFields,
    -- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
    recordDelimiter :: Core.Maybe Types.RecordDelimiter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVOutput' value with any optional fields omitted.
mkCSVOutput ::
  CSVOutput
mkCSVOutput =
  CSVOutput'
    { fieldDelimiter = Core.Nothing,
      quoteCharacter = Core.Nothing,
      quoteEscapeCharacter = Core.Nothing,
      quoteFields = Core.Nothing,
      recordDelimiter = Core.Nothing
    }

-- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoFieldDelimiter :: Lens.Lens' CSVOutput (Core.Maybe Types.FieldDelimiter)
csvoFieldDelimiter = Lens.field @"fieldDelimiter"
{-# DEPRECATED csvoFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoQuoteCharacter :: Lens.Lens' CSVOutput (Core.Maybe Types.QuoteCharacter)
csvoQuoteCharacter = Lens.field @"quoteCharacter"
{-# DEPRECATED csvoQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | The single character used for escaping the quote character inside an already escaped value.
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoQuoteEscapeCharacter :: Lens.Lens' CSVOutput (Core.Maybe Types.QuoteEscapeCharacter)
csvoQuoteEscapeCharacter = Lens.field @"quoteEscapeCharacter"
{-# DEPRECATED csvoQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

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
csvoQuoteFields :: Lens.Lens' CSVOutput (Core.Maybe Types.QuoteFields)
csvoQuoteFields = Lens.field @"quoteFields"
{-# DEPRECATED csvoQuoteFields "Use generic-lens or generic-optics with 'quoteFields' instead." #-}

-- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvoRecordDelimiter :: Lens.Lens' CSVOutput (Core.Maybe Types.RecordDelimiter)
csvoRecordDelimiter = Lens.field @"recordDelimiter"
{-# DEPRECATED csvoRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

instance Core.ToXML CSVOutput where
  toXML CSVOutput {..} =
    Core.toXMLNode "FieldDelimiter" Core.<$> fieldDelimiter
      Core.<> Core.toXMLNode "QuoteCharacter" Core.<$> quoteCharacter
      Core.<> Core.toXMLNode "QuoteEscapeCharacter" Core.<$> quoteEscapeCharacter
      Core.<> Core.toXMLNode "QuoteFields" Core.<$> quoteFields
      Core.<> Core.toXMLNode "RecordDelimiter" Core.<$> recordDelimiter

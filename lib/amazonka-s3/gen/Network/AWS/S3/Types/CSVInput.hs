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
    csviAllowQuotedRecordDelimiter,
    csviComments,
    csviFieldDelimiter,
    csviFileHeaderInfo,
    csviQuoteCharacter,
    csviQuoteEscapeCharacter,
    csviRecordDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Comments as Types
import qualified Network.AWS.S3.Types.FieldDelimiter as Types
import qualified Network.AWS.S3.Types.FileHeaderInfo as Types
import qualified Network.AWS.S3.Types.QuoteCharacter as Types
import qualified Network.AWS.S3.Types.QuoteEscapeCharacter as Types
import qualified Network.AWS.S3.Types.RecordDelimiter as Types

-- | Describes how an uncompressed comma-separated values (CSV)-formatted input object is formatted.
--
-- /See:/ 'mkCSVInput' smart constructor.
data CSVInput = CSVInput'
  { -- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
    allowQuotedRecordDelimiter :: Core.Maybe Core.Bool,
    -- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
    comments :: Core.Maybe Types.Comments,
    -- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
    fieldDelimiter :: Core.Maybe Types.FieldDelimiter,
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
    fileHeaderInfo :: Core.Maybe Types.FileHeaderInfo,
    -- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
    --
    -- Type: String
    -- Default: @"@
    -- Ancestors: @CSV@
    quoteCharacter :: Core.Maybe Types.QuoteCharacter,
    -- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
    quoteEscapeCharacter :: Core.Maybe Types.QuoteEscapeCharacter,
    -- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
    recordDelimiter :: Core.Maybe Types.RecordDelimiter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVInput' value with any optional fields omitted.
mkCSVInput ::
  CSVInput
mkCSVInput =
  CSVInput'
    { allowQuotedRecordDelimiter = Core.Nothing,
      comments = Core.Nothing,
      fieldDelimiter = Core.Nothing,
      fileHeaderInfo = Core.Nothing,
      quoteCharacter = Core.Nothing,
      quoteEscapeCharacter = Core.Nothing,
      recordDelimiter = Core.Nothing
    }

-- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
--
-- /Note:/ Consider using 'allowQuotedRecordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviAllowQuotedRecordDelimiter :: Lens.Lens' CSVInput (Core.Maybe Core.Bool)
csviAllowQuotedRecordDelimiter = Lens.field @"allowQuotedRecordDelimiter"
{-# DEPRECATED csviAllowQuotedRecordDelimiter "Use generic-lens or generic-optics with 'allowQuotedRecordDelimiter' instead." #-}

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviComments :: Lens.Lens' CSVInput (Core.Maybe Types.Comments)
csviComments = Lens.field @"comments"
{-# DEPRECATED csviComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviFieldDelimiter :: Lens.Lens' CSVInput (Core.Maybe Types.FieldDelimiter)
csviFieldDelimiter = Lens.field @"fieldDelimiter"
{-# DEPRECATED csviFieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead." #-}

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
csviFileHeaderInfo :: Lens.Lens' CSVInput (Core.Maybe Types.FileHeaderInfo)
csviFileHeaderInfo = Lens.field @"fileHeaderInfo"
{-# DEPRECATED csviFileHeaderInfo "Use generic-lens or generic-optics with 'fileHeaderInfo' instead." #-}

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- Type: String
-- Default: @"@
-- Ancestors: @CSV@
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteCharacter :: Lens.Lens' CSVInput (Core.Maybe Types.QuoteCharacter)
csviQuoteCharacter = Lens.field @"quoteCharacter"
{-# DEPRECATED csviQuoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead." #-}

-- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteEscapeCharacter :: Lens.Lens' CSVInput (Core.Maybe Types.QuoteEscapeCharacter)
csviQuoteEscapeCharacter = Lens.field @"quoteEscapeCharacter"
{-# DEPRECATED csviQuoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead." #-}

-- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviRecordDelimiter :: Lens.Lens' CSVInput (Core.Maybe Types.RecordDelimiter)
csviRecordDelimiter = Lens.field @"recordDelimiter"
{-# DEPRECATED csviRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

instance Core.ToXML CSVInput where
  toXML CSVInput {..} =
    Core.toXMLNode "AllowQuotedRecordDelimiter"
      Core.<$> allowQuotedRecordDelimiter
      Core.<> Core.toXMLNode "Comments" Core.<$> comments
      Core.<> Core.toXMLNode "FieldDelimiter" Core.<$> fieldDelimiter
      Core.<> Core.toXMLNode "FileHeaderInfo" Core.<$> fileHeaderInfo
      Core.<> Core.toXMLNode "QuoteCharacter" Core.<$> quoteCharacter
      Core.<> Core.toXMLNode "QuoteEscapeCharacter" Core.<$> quoteEscapeCharacter
      Core.<> Core.toXMLNode "RecordDelimiter" Core.<$> recordDelimiter

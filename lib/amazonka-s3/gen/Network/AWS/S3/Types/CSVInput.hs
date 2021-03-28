{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CSVInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.CSVInput
  ( CSVInput (..)
  -- * Smart constructor
  , mkCSVInput
  -- * Lenses
  , csviAllowQuotedRecordDelimiter
  , csviComments
  , csviFieldDelimiter
  , csviFileHeaderInfo
  , csviQuoteCharacter
  , csviQuoteEscapeCharacter
  , csviRecordDelimiter
  ) where

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
  { allowQuotedRecordDelimiter :: Core.Maybe Core.Bool
    -- ^ Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
  , comments :: Core.Maybe Types.Comments
    -- ^ A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
  , fieldDelimiter :: Core.Maybe Types.FieldDelimiter
    -- ^ A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
  , fileHeaderInfo :: Core.Maybe Types.FileHeaderInfo
    -- ^ Describes the first line of input. Valid values are:
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
  , quoteCharacter :: Core.Maybe Types.QuoteCharacter
    -- ^ A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- Type: String
-- Default: @"@ 
-- Ancestors: @CSV@ 
  , quoteEscapeCharacter :: Core.Maybe Types.QuoteEscapeCharacter
    -- ^ A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
  , recordDelimiter :: Core.Maybe Types.RecordDelimiter
    -- ^ A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVInput' value with any optional fields omitted.
mkCSVInput
    :: CSVInput
mkCSVInput
  = CSVInput'{allowQuotedRecordDelimiter = Core.Nothing,
              comments = Core.Nothing, fieldDelimiter = Core.Nothing,
              fileHeaderInfo = Core.Nothing, quoteCharacter = Core.Nothing,
              quoteEscapeCharacter = Core.Nothing,
              recordDelimiter = Core.Nothing}

-- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
--
-- /Note:/ Consider using 'allowQuotedRecordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviAllowQuotedRecordDelimiter :: Lens.Lens' CSVInput (Core.Maybe Core.Bool)
csviAllowQuotedRecordDelimiter = Lens.field @"allowQuotedRecordDelimiter"
{-# INLINEABLE csviAllowQuotedRecordDelimiter #-}
{-# DEPRECATED allowQuotedRecordDelimiter "Use generic-lens or generic-optics with 'allowQuotedRecordDelimiter' instead"  #-}

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviComments :: Lens.Lens' CSVInput (Core.Maybe Types.Comments)
csviComments = Lens.field @"comments"
{-# INLINEABLE csviComments #-}
{-# DEPRECATED comments "Use generic-lens or generic-optics with 'comments' instead"  #-}

-- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'fieldDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviFieldDelimiter :: Lens.Lens' CSVInput (Core.Maybe Types.FieldDelimiter)
csviFieldDelimiter = Lens.field @"fieldDelimiter"
{-# INLINEABLE csviFieldDelimiter #-}
{-# DEPRECATED fieldDelimiter "Use generic-lens or generic-optics with 'fieldDelimiter' instead"  #-}

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
{-# INLINEABLE csviFileHeaderInfo #-}
{-# DEPRECATED fileHeaderInfo "Use generic-lens or generic-optics with 'fileHeaderInfo' instead"  #-}

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- Type: String
-- Default: @"@ 
-- Ancestors: @CSV@ 
--
-- /Note:/ Consider using 'quoteCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteCharacter :: Lens.Lens' CSVInput (Core.Maybe Types.QuoteCharacter)
csviQuoteCharacter = Lens.field @"quoteCharacter"
{-# INLINEABLE csviQuoteCharacter #-}
{-# DEPRECATED quoteCharacter "Use generic-lens or generic-optics with 'quoteCharacter' instead"  #-}

-- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
--
-- /Note:/ Consider using 'quoteEscapeCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviQuoteEscapeCharacter :: Lens.Lens' CSVInput (Core.Maybe Types.QuoteEscapeCharacter)
csviQuoteEscapeCharacter = Lens.field @"quoteEscapeCharacter"
{-# INLINEABLE csviQuoteEscapeCharacter #-}
{-# DEPRECATED quoteEscapeCharacter "Use generic-lens or generic-optics with 'quoteEscapeCharacter' instead"  #-}

-- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csviRecordDelimiter :: Lens.Lens' CSVInput (Core.Maybe Types.RecordDelimiter)
csviRecordDelimiter = Lens.field @"recordDelimiter"
{-# INLINEABLE csviRecordDelimiter #-}
{-# DEPRECATED recordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead"  #-}

instance Core.ToXML CSVInput where
        toXML CSVInput{..}
          = Core.maybe Core.mempty
              (Core.toXMLElement "AllowQuotedRecordDelimiter")
              allowQuotedRecordDelimiter
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comments") comments
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "FieldDelimiter")
                fieldDelimiter
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "FileHeaderInfo")
                fileHeaderInfo
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "QuoteCharacter")
                quoteCharacter
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "QuoteEscapeCharacter")
                quoteEscapeCharacter
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RecordDelimiter")
                recordDelimiter

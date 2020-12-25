{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageSystemAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageSystemAttributeValue
  ( MessageSystemAttributeValue (..),

    -- * Smart constructor
    mkMessageSystemAttributeValue,

    -- * Lenses
    msavDataType,
    msavBinaryListValues,
    msavBinaryValue,
    msavStringListValues,
    msavStringValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.String as Types

-- | The user-specified message system attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null.
--
-- /See:/ 'mkMessageSystemAttributeValue' smart constructor.
data MessageSystemAttributeValue = MessageSystemAttributeValue'
  { -- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
    --
    -- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
    dataType :: Types.String,
    -- | Not implemented. Reserved for future use.
    binaryListValues :: Core.Maybe [Core.Base64],
    -- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.
    binaryValue :: Core.Maybe Core.Base64,
    -- | Not implemented. Reserved for future use.
    stringListValues :: Core.Maybe [Types.String],
    -- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
    stringValue :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageSystemAttributeValue' value with any optional fields omitted.
mkMessageSystemAttributeValue ::
  -- | 'dataType'
  Types.String ->
  MessageSystemAttributeValue
mkMessageSystemAttributeValue dataType =
  MessageSystemAttributeValue'
    { dataType,
      binaryListValues = Core.Nothing,
      binaryValue = Core.Nothing,
      stringListValues = Core.Nothing,
      stringValue = Core.Nothing
    }

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
--
-- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavDataType :: Lens.Lens' MessageSystemAttributeValue Types.String
msavDataType = Lens.field @"dataType"
{-# DEPRECATED msavDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'binaryListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavBinaryListValues :: Lens.Lens' MessageSystemAttributeValue (Core.Maybe [Core.Base64])
msavBinaryListValues = Lens.field @"binaryListValues"
{-# DEPRECATED msavBinaryListValues "Use generic-lens or generic-optics with 'binaryListValues' instead." #-}

-- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavBinaryValue :: Lens.Lens' MessageSystemAttributeValue (Core.Maybe Core.Base64)
msavBinaryValue = Lens.field @"binaryValue"
{-# DEPRECATED msavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'stringListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavStringListValues :: Lens.Lens' MessageSystemAttributeValue (Core.Maybe [Types.String])
msavStringListValues = Lens.field @"stringListValues"
{-# DEPRECATED msavStringListValues "Use generic-lens or generic-optics with 'stringListValues' instead." #-}

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavStringValue :: Lens.Lens' MessageSystemAttributeValue (Core.Maybe Types.String)
msavStringValue = Lens.field @"stringValue"
{-# DEPRECATED msavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

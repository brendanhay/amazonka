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
    msavBinaryValue,
    msavStringListValues,
    msavStringValue,
    msavBinaryListValues,
    msavDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-specified message system attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null.
--
-- /See:/ 'mkMessageSystemAttributeValue' smart constructor.
data MessageSystemAttributeValue = MessageSystemAttributeValue'
  { -- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.
    binaryValue :: Lude.Maybe Lude.Base64,
    -- | Not implemented. Reserved for future use.
    stringListValues :: Lude.Maybe [Lude.Text],
    -- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
    stringValue :: Lude.Maybe Lude.Text,
    -- | Not implemented. Reserved for future use.
    binaryListValues :: Lude.Maybe [Lude.Base64],
    -- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
    --
    -- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
    dataType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageSystemAttributeValue' with the minimum fields required to make a request.
--
-- * 'binaryValue' - Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.
-- * 'stringListValues' - Not implemented. Reserved for future use.
-- * 'stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
-- * 'binaryListValues' - Not implemented. Reserved for future use.
-- * 'dataType' - Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
--
-- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
mkMessageSystemAttributeValue ::
  -- | 'dataType'
  Lude.Text ->
  MessageSystemAttributeValue
mkMessageSystemAttributeValue pDataType_ =
  MessageSystemAttributeValue'
    { binaryValue = Lude.Nothing,
      stringListValues = Lude.Nothing,
      stringValue = Lude.Nothing,
      binaryListValues = Lude.Nothing,
      dataType = pDataType_
    }

-- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavBinaryValue :: Lens.Lens' MessageSystemAttributeValue (Lude.Maybe Lude.Base64)
msavBinaryValue = Lens.lens (binaryValue :: MessageSystemAttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {binaryValue = a} :: MessageSystemAttributeValue)
{-# DEPRECATED msavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'stringListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavStringListValues :: Lens.Lens' MessageSystemAttributeValue (Lude.Maybe [Lude.Text])
msavStringListValues = Lens.lens (stringListValues :: MessageSystemAttributeValue -> Lude.Maybe [Lude.Text]) (\s a -> s {stringListValues = a} :: MessageSystemAttributeValue)
{-# DEPRECATED msavStringListValues "Use generic-lens or generic-optics with 'stringListValues' instead." #-}

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavStringValue :: Lens.Lens' MessageSystemAttributeValue (Lude.Maybe Lude.Text)
msavStringValue = Lens.lens (stringValue :: MessageSystemAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: MessageSystemAttributeValue)
{-# DEPRECATED msavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'binaryListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavBinaryListValues :: Lens.Lens' MessageSystemAttributeValue (Lude.Maybe [Lude.Base64])
msavBinaryListValues = Lens.lens (binaryListValues :: MessageSystemAttributeValue -> Lude.Maybe [Lude.Base64]) (\s a -> s {binaryListValues = a} :: MessageSystemAttributeValue)
{-# DEPRECATED msavBinaryListValues "Use generic-lens or generic-optics with 'binaryListValues' instead." #-}

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
--
-- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msavDataType :: Lens.Lens' MessageSystemAttributeValue Lude.Text
msavDataType = Lens.lens (dataType :: MessageSystemAttributeValue -> Lude.Text) (\s a -> s {dataType = a} :: MessageSystemAttributeValue)
{-# DEPRECATED msavDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.ToQuery MessageSystemAttributeValue where
  toQuery MessageSystemAttributeValue' {..} =
    Lude.mconcat
      [ "BinaryValue" Lude.=: binaryValue,
        "StringListValue"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "StringListValue" Lude.<$> stringListValues),
        "StringValue" Lude.=: stringValue,
        "BinaryListValue"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "BinaryListValue" Lude.<$> binaryListValues),
        "DataType" Lude.=: dataType
      ]

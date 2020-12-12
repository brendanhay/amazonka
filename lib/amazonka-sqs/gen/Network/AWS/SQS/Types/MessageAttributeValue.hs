{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttributeValue
  ( MessageAttributeValue (..),

    -- * Smart constructor
    mkMessageAttributeValue,

    -- * Lenses
    mavBinaryValue,
    mavStringListValues,
    mavStringValue,
    mavBinaryListValues,
    mavDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-specified message attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null. All parts of the message attribute, including @Name@ , @Type@ , and @Value@ , are part of the message size restriction (256 KB or 262,144 bytes).
--
-- /See:/ 'mkMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { binaryValue ::
      Lude.Maybe Lude.Base64,
    stringListValues :: Lude.Maybe [Lude.Text],
    stringValue :: Lude.Maybe Lude.Text,
    binaryListValues :: Lude.Maybe [Lude.Base64],
    dataType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- * 'binaryListValues' - Not implemented. Reserved for future use.
-- * 'binaryValue' - Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'dataType' - Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
--
-- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'stringListValues' - Not implemented. Reserved for future use.
-- * 'stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
mkMessageAttributeValue ::
  -- | 'dataType'
  Lude.Text ->
  MessageAttributeValue
mkMessageAttributeValue pDataType_ =
  MessageAttributeValue'
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
mavBinaryValue :: Lens.Lens' MessageAttributeValue (Lude.Maybe Lude.Base64)
mavBinaryValue = Lens.lens (binaryValue :: MessageAttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {binaryValue = a} :: MessageAttributeValue)
{-# DEPRECATED mavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'stringListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavStringListValues :: Lens.Lens' MessageAttributeValue (Lude.Maybe [Lude.Text])
mavStringListValues = Lens.lens (stringListValues :: MessageAttributeValue -> Lude.Maybe [Lude.Text]) (\s a -> s {stringListValues = a} :: MessageAttributeValue)
{-# DEPRECATED mavStringListValues "Use generic-lens or generic-optics with 'stringListValues' instead." #-}

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavStringValue :: Lens.Lens' MessageAttributeValue (Lude.Maybe Lude.Text)
mavStringValue = Lens.lens (stringValue :: MessageAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: MessageAttributeValue)
{-# DEPRECATED mavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | Not implemented. Reserved for future use.
--
-- /Note:/ Consider using 'binaryListValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavBinaryListValues :: Lens.Lens' MessageAttributeValue (Lude.Maybe [Lude.Base64])
mavBinaryListValues = Lens.lens (binaryListValues :: MessageAttributeValue -> Lude.Maybe [Lude.Base64]) (\s a -> s {binaryListValues = a} :: MessageAttributeValue)
{-# DEPRECATED mavBinaryListValues "Use generic-lens or generic-optics with 'binaryListValues' instead." #-}

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ .
--
-- You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavDataType :: Lens.Lens' MessageAttributeValue Lude.Text
mavDataType = Lens.lens (dataType :: MessageAttributeValue -> Lude.Text) (\s a -> s {dataType = a} :: MessageAttributeValue)
{-# DEPRECATED mavDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.FromXML MessageAttributeValue where
  parseXML x =
    MessageAttributeValue'
      Lude.<$> (x Lude..@? "BinaryValue")
      Lude.<*> ( x Lude..@? "StringListValue" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "StringListValue")
               )
      Lude.<*> (x Lude..@? "StringValue")
      Lude.<*> ( x Lude..@? "BinaryListValue" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "BinaryListValue")
               )
      Lude.<*> (x Lude..@ "DataType")

instance Lude.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
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

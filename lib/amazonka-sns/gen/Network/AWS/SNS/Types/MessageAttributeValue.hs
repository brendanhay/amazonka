{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.MessageAttributeValue
  ( MessageAttributeValue (..),

    -- * Smart constructor
    mkMessageAttributeValue,

    -- * Lenses
    mavBinaryValue,
    mavStringValue,
    mavDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> .
--
-- Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Amazon SNS message attributes> and <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone> in the /Amazon SNS Developer Guide./
--
-- /See:/ 'mkMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { -- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.
    binaryValue :: Lude.Maybe Lude.Base64,
    -- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
    stringValue :: Lude.Maybe Lude.Text,
    -- | Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
    dataType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- * 'binaryValue' - Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.
-- * 'stringValue' - Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
-- * 'dataType' - Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
mkMessageAttributeValue ::
  -- | 'dataType'
  Lude.Text ->
  MessageAttributeValue
mkMessageAttributeValue pDataType_ =
  MessageAttributeValue'
    { binaryValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      dataType = pDataType_
    }

-- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavBinaryValue :: Lens.Lens' MessageAttributeValue (Lude.Maybe Lude.Base64)
mavBinaryValue = Lens.lens (binaryValue :: MessageAttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {binaryValue = a} :: MessageAttributeValue)
{-# DEPRECATED mavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavStringValue :: Lens.Lens' MessageAttributeValue (Lude.Maybe Lude.Text)
mavStringValue = Lens.lens (stringValue :: MessageAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: MessageAttributeValue)
{-# DEPRECATED mavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavDataType :: Lens.Lens' MessageAttributeValue Lude.Text
mavDataType = Lens.lens (dataType :: MessageAttributeValue -> Lude.Text) (\s a -> s {dataType = a} :: MessageAttributeValue)
{-# DEPRECATED mavDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
    Lude.mconcat
      [ "BinaryValue" Lude.=: binaryValue,
        "StringValue" Lude.=: stringValue,
        "DataType" Lude.=: dataType
      ]

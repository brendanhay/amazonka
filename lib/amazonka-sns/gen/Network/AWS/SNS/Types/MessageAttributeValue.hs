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
    mavDataType,
    mavBinaryValue,
    mavStringValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SNS.Types.String as Types

-- | The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> .
--
-- Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Amazon SNS message attributes> and <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone> in the /Amazon SNS Developer Guide./
--
-- /See:/ 'mkMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { -- | Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
    dataType :: Types.String,
    -- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.
    binaryValue :: Core.Maybe Core.Base64,
    -- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
    stringValue :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageAttributeValue' value with any optional fields omitted.
mkMessageAttributeValue ::
  -- | 'dataType'
  Types.String ->
  MessageAttributeValue
mkMessageAttributeValue dataType =
  MessageAttributeValue'
    { dataType,
      binaryValue = Core.Nothing,
      stringValue = Core.Nothing
    }

-- | Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavDataType :: Lens.Lens' MessageAttributeValue Types.String
mavDataType = Lens.field @"dataType"
{-# DEPRECATED mavDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavBinaryValue :: Lens.Lens' MessageAttributeValue (Core.Maybe Core.Base64)
mavBinaryValue = Lens.field @"binaryValue"
{-# DEPRECATED mavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mavStringValue :: Lens.Lens' MessageAttributeValue (Core.Maybe Types.String)
mavStringValue = Lens.field @"stringValue"
{-# DEPRECATED mavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

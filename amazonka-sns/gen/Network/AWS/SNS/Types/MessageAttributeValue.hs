{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.MessageAttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The user-specified message attribute value. For string data types, the
-- value attribute has the same restrictions on the content as the message
-- body. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish>.
--
-- Name, type, and value must not be empty or null. In addition, the
-- message body should not be empty or null. All parts of the message
-- attribute, including name, type, and value, are included in the message
-- size restriction, which is currently 256 KB (262,144 bytes). For more
-- information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Amazon SNS message attributes>
-- and
-- <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone>
-- in the /Amazon SNS Developer Guide./
--
-- /See:/ 'newMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { -- | Strings are Unicode with UTF8 binary encoding. For a list of code
    -- values, see
    -- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
    stringValue :: Core.Maybe Core.Text,
    -- | Binary type attributes can store any binary data, for example,
    -- compressed data, encrypted data, or images.
    binaryValue :: Core.Maybe Core.Base64,
    -- | Amazon SNS supports the following logical data types: String,
    -- String.Array, Number, and Binary. For more information, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
    dataType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MessageAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValue', 'messageAttributeValue_stringValue' - Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
--
-- 'binaryValue', 'messageAttributeValue_binaryValue' - Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'dataType', 'messageAttributeValue_dataType' - Amazon SNS supports the following logical data types: String,
-- String.Array, Number, and Binary. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
newMessageAttributeValue ::
  -- | 'dataType'
  Core.Text ->
  MessageAttributeValue
newMessageAttributeValue pDataType_ =
  MessageAttributeValue'
    { stringValue = Core.Nothing,
      binaryValue = Core.Nothing,
      dataType = pDataType_
    }

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageAttributeValue_stringValue :: Lens.Lens' MessageAttributeValue (Core.Maybe Core.Text)
messageAttributeValue_stringValue = Lens.lens (\MessageAttributeValue' {stringValue} -> stringValue) (\s@MessageAttributeValue' {} a -> s {stringValue = a} :: MessageAttributeValue)

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageAttributeValue_binaryValue :: Lens.Lens' MessageAttributeValue (Core.Maybe Core.ByteString)
messageAttributeValue_binaryValue = Lens.lens (\MessageAttributeValue' {binaryValue} -> binaryValue) (\s@MessageAttributeValue' {} a -> s {binaryValue = a} :: MessageAttributeValue) Core.. Lens.mapping Core._Base64

-- | Amazon SNS supports the following logical data types: String,
-- String.Array, Number, and Binary. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
messageAttributeValue_dataType :: Lens.Lens' MessageAttributeValue Core.Text
messageAttributeValue_dataType = Lens.lens (\MessageAttributeValue' {dataType} -> dataType) (\s@MessageAttributeValue' {} a -> s {dataType = a} :: MessageAttributeValue)

instance Core.Hashable MessageAttributeValue

instance Core.NFData MessageAttributeValue

instance Core.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
    Core.mconcat
      [ "StringValue" Core.=: stringValue,
        "BinaryValue" Core.=: binaryValue,
        "DataType" Core.=: dataType
      ]

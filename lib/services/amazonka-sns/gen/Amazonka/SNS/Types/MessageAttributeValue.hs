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
-- Module      : Amazonka.SNS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.MessageAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | Binary type attributes can store any binary data, for example,
    -- compressed data, encrypted data, or images.
    binaryValue :: Prelude.Maybe Data.Base64,
    -- | Strings are Unicode with UTF8 binary encoding. For a list of code
    -- values, see
    -- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | Amazon SNS supports the following logical data types: String,
    -- String.Array, Number, and Binary. For more information, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
    dataType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binaryValue', 'messageAttributeValue_binaryValue' - Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'stringValue', 'messageAttributeValue_stringValue' - Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
--
-- 'dataType', 'messageAttributeValue_dataType' - Amazon SNS supports the following logical data types: String,
-- String.Array, Number, and Binary. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
newMessageAttributeValue ::
  -- | 'dataType'
  Prelude.Text ->
  MessageAttributeValue
newMessageAttributeValue pDataType_ =
  MessageAttributeValue'
    { binaryValue =
        Prelude.Nothing,
      stringValue = Prelude.Nothing,
      dataType = pDataType_
    }

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageAttributeValue_binaryValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.ByteString)
messageAttributeValue_binaryValue = Lens.lens (\MessageAttributeValue' {binaryValue} -> binaryValue) (\s@MessageAttributeValue' {} a -> s {binaryValue = a} :: MessageAttributeValue) Prelude.. Lens.mapping Data._Base64

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageAttributeValue_stringValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.Text)
messageAttributeValue_stringValue = Lens.lens (\MessageAttributeValue' {stringValue} -> stringValue) (\s@MessageAttributeValue' {} a -> s {stringValue = a} :: MessageAttributeValue)

-- | Amazon SNS supports the following logical data types: String,
-- String.Array, Number, and Binary. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
messageAttributeValue_dataType :: Lens.Lens' MessageAttributeValue Prelude.Text
messageAttributeValue_dataType = Lens.lens (\MessageAttributeValue' {dataType} -> dataType) (\s@MessageAttributeValue' {} a -> s {dataType = a} :: MessageAttributeValue)

instance Prelude.Hashable MessageAttributeValue where
  hashWithSalt _salt MessageAttributeValue' {..} =
    _salt
      `Prelude.hashWithSalt` binaryValue
      `Prelude.hashWithSalt` stringValue
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData MessageAttributeValue where
  rnf MessageAttributeValue' {..} =
    Prelude.rnf binaryValue
      `Prelude.seq` Prelude.rnf stringValue
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
    Prelude.mconcat
      [ "BinaryValue" Data.=: binaryValue,
        "StringValue" Data.=: stringValue,
        "DataType" Data.=: dataType
      ]

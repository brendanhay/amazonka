{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.Types.MessageSystemAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageSystemAttributeValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user-specified message system attribute value. For string data
-- types, the @Value@ attribute has the same restrictions on the content as
-- the message body. For more information, see @ SendMessage.@
--
-- @Name@, @type@, @value@ and the message body must not be empty or null.
--
-- /See:/ 'newMessageSystemAttributeValue' smart constructor.
data MessageSystemAttributeValue = MessageSystemAttributeValue'
  { -- | Not implemented. Reserved for future use.
    stringListValues :: Prelude.Maybe [Prelude.Text],
    -- | Strings are Unicode with UTF-8 binary encoding. For a list of code
    -- values, see
    -- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | Not implemented. Reserved for future use.
    binaryListValues :: Prelude.Maybe [Prelude.Base64],
    -- | Binary type attributes can store any binary data, such as compressed
    -- data, encrypted data, or images.
    binaryValue :: Prelude.Maybe Prelude.Base64,
    -- | Amazon SQS supports the following logical data types: @String@,
    -- @Number@, and @Binary@. For the @Number@ data type, you must use
    -- @StringValue@.
    --
    -- You can also append custom labels. For more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    dataType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MessageSystemAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringListValues', 'messageSystemAttributeValue_stringListValues' - Not implemented. Reserved for future use.
--
-- 'stringValue', 'messageSystemAttributeValue_stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
--
-- 'binaryListValues', 'messageSystemAttributeValue_binaryListValues' - Not implemented. Reserved for future use.
--
-- 'binaryValue', 'messageSystemAttributeValue_binaryValue' - Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'dataType', 'messageSystemAttributeValue_dataType' - Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
newMessageSystemAttributeValue ::
  -- | 'dataType'
  Prelude.Text ->
  MessageSystemAttributeValue
newMessageSystemAttributeValue pDataType_ =
  MessageSystemAttributeValue'
    { stringListValues =
        Prelude.Nothing,
      stringValue = Prelude.Nothing,
      binaryListValues = Prelude.Nothing,
      binaryValue = Prelude.Nothing,
      dataType = pDataType_
    }

-- | Not implemented. Reserved for future use.
messageSystemAttributeValue_stringListValues :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe [Prelude.Text])
messageSystemAttributeValue_stringListValues = Lens.lens (\MessageSystemAttributeValue' {stringListValues} -> stringListValues) (\s@MessageSystemAttributeValue' {} a -> s {stringListValues = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Prelude._Coerce

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageSystemAttributeValue_stringValue :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe Prelude.Text)
messageSystemAttributeValue_stringValue = Lens.lens (\MessageSystemAttributeValue' {stringValue} -> stringValue) (\s@MessageSystemAttributeValue' {} a -> s {stringValue = a} :: MessageSystemAttributeValue)

-- | Not implemented. Reserved for future use.
messageSystemAttributeValue_binaryListValues :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe [Prelude.ByteString])
messageSystemAttributeValue_binaryListValues = Lens.lens (\MessageSystemAttributeValue' {binaryListValues} -> binaryListValues) (\s@MessageSystemAttributeValue' {} a -> s {binaryListValues = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Prelude._Coerce

-- | Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageSystemAttributeValue_binaryValue :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe Prelude.ByteString)
messageSystemAttributeValue_binaryValue = Lens.lens (\MessageSystemAttributeValue' {binaryValue} -> binaryValue) (\s@MessageSystemAttributeValue' {} a -> s {binaryValue = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Prelude._Base64

-- | Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
messageSystemAttributeValue_dataType :: Lens.Lens' MessageSystemAttributeValue Prelude.Text
messageSystemAttributeValue_dataType = Lens.lens (\MessageSystemAttributeValue' {dataType} -> dataType) (\s@MessageSystemAttributeValue' {} a -> s {dataType = a} :: MessageSystemAttributeValue)

instance Prelude.Hashable MessageSystemAttributeValue

instance Prelude.NFData MessageSystemAttributeValue

instance Prelude.ToQuery MessageSystemAttributeValue where
  toQuery MessageSystemAttributeValue' {..} =
    Prelude.mconcat
      [ "StringListValue"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "StringListValue"
                Prelude.<$> stringListValues
            ),
        "StringValue" Prelude.=: stringValue,
        "BinaryListValue"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "BinaryListValue"
                Prelude.<$> binaryListValues
            ),
        "BinaryValue" Prelude.=: binaryValue,
        "DataType" Prelude.=: dataType
      ]

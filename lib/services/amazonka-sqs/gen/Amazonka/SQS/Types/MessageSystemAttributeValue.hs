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
-- Module      : Amazonka.SQS.Types.MessageSystemAttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.MessageSystemAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user-specified message system attribute value. For string data
-- types, the @Value@ attribute has the same restrictions on the content as
-- the message body. For more information, see @ SendMessage.@
--
-- @Name@, @type@, @value@ and the message body must not be empty or null.
--
-- /See:/ 'newMessageSystemAttributeValue' smart constructor.
data MessageSystemAttributeValue = MessageSystemAttributeValue'
  { -- | Not implemented. Reserved for future use.
    binaryListValues :: Prelude.Maybe [Data.Base64],
    -- | Binary type attributes can store any binary data, such as compressed
    -- data, encrypted data, or images.
    binaryValue :: Prelude.Maybe Data.Base64,
    -- | Not implemented. Reserved for future use.
    stringListValues :: Prelude.Maybe [Prelude.Text],
    -- | Strings are Unicode with UTF-8 binary encoding. For a list of code
    -- values, see
    -- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | Amazon SQS supports the following logical data types: @String@,
    -- @Number@, and @Binary@. For the @Number@ data type, you must use
    -- @StringValue@.
    --
    -- You can also append custom labels. For more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
    -- in the /Amazon SQS Developer Guide/.
    dataType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageSystemAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'stringListValues', 'messageSystemAttributeValue_stringListValues' - Not implemented. Reserved for future use.
--
-- 'stringValue', 'messageSystemAttributeValue_stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
--
-- 'dataType', 'messageSystemAttributeValue_dataType' - Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon SQS Developer Guide/.
newMessageSystemAttributeValue ::
  -- | 'dataType'
  Prelude.Text ->
  MessageSystemAttributeValue
newMessageSystemAttributeValue pDataType_ =
  MessageSystemAttributeValue'
    { binaryListValues =
        Prelude.Nothing,
      binaryValue = Prelude.Nothing,
      stringListValues = Prelude.Nothing,
      stringValue = Prelude.Nothing,
      dataType = pDataType_
    }

-- | Not implemented. Reserved for future use.
messageSystemAttributeValue_binaryListValues :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe [Prelude.ByteString])
messageSystemAttributeValue_binaryListValues = Lens.lens (\MessageSystemAttributeValue' {binaryListValues} -> binaryListValues) (\s@MessageSystemAttributeValue' {} a -> s {binaryListValues = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageSystemAttributeValue_binaryValue :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe Prelude.ByteString)
messageSystemAttributeValue_binaryValue = Lens.lens (\MessageSystemAttributeValue' {binaryValue} -> binaryValue) (\s@MessageSystemAttributeValue' {} a -> s {binaryValue = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Data._Base64

-- | Not implemented. Reserved for future use.
messageSystemAttributeValue_stringListValues :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe [Prelude.Text])
messageSystemAttributeValue_stringListValues = Lens.lens (\MessageSystemAttributeValue' {stringListValues} -> stringListValues) (\s@MessageSystemAttributeValue' {} a -> s {stringListValues = a} :: MessageSystemAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageSystemAttributeValue_stringValue :: Lens.Lens' MessageSystemAttributeValue (Prelude.Maybe Prelude.Text)
messageSystemAttributeValue_stringValue = Lens.lens (\MessageSystemAttributeValue' {stringValue} -> stringValue) (\s@MessageSystemAttributeValue' {} a -> s {stringValue = a} :: MessageSystemAttributeValue)

-- | Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon SQS Developer Guide/.
messageSystemAttributeValue_dataType :: Lens.Lens' MessageSystemAttributeValue Prelude.Text
messageSystemAttributeValue_dataType = Lens.lens (\MessageSystemAttributeValue' {dataType} -> dataType) (\s@MessageSystemAttributeValue' {} a -> s {dataType = a} :: MessageSystemAttributeValue)

instance Prelude.Hashable MessageSystemAttributeValue where
  hashWithSalt _salt MessageSystemAttributeValue' {..} =
    _salt `Prelude.hashWithSalt` binaryListValues
      `Prelude.hashWithSalt` binaryValue
      `Prelude.hashWithSalt` stringListValues
      `Prelude.hashWithSalt` stringValue
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData MessageSystemAttributeValue where
  rnf MessageSystemAttributeValue' {..} =
    Prelude.rnf binaryListValues
      `Prelude.seq` Prelude.rnf binaryValue
      `Prelude.seq` Prelude.rnf stringListValues
      `Prelude.seq` Prelude.rnf stringValue
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToQuery MessageSystemAttributeValue where
  toQuery MessageSystemAttributeValue' {..} =
    Prelude.mconcat
      [ "BinaryListValue"
          Data.=: Data.toQuery
            ( Data.toQueryList "BinaryListValue"
                Prelude.<$> binaryListValues
            ),
        "BinaryValue" Data.=: binaryValue,
        "StringListValue"
          Data.=: Data.toQuery
            ( Data.toQueryList "StringListValue"
                Prelude.<$> stringListValues
            ),
        "StringValue" Data.=: stringValue,
        "DataType" Data.=: dataType
      ]

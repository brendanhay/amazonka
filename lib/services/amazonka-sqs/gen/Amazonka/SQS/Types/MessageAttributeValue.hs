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
-- Module      : Amazonka.SQS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.MessageAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user-specified message attribute value. For string data types, the
-- @Value@ attribute has the same restrictions on the content as the
-- message body. For more information, see @ @@SendMessage@@.@
--
-- @Name@, @type@, @value@ and the message body must not be empty or null.
-- All parts of the message attribute, including @Name@, @Type@, and
-- @Value@, are part of the message size restriction (256 KiB or 262,144
-- bytes).
--
-- /See:/ 'newMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
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
-- Create a value of 'MessageAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binaryListValues', 'messageAttributeValue_binaryListValues' - Not implemented. Reserved for future use.
--
-- 'binaryValue', 'messageAttributeValue_binaryValue' - Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'stringListValues', 'messageAttributeValue_stringListValues' - Not implemented. Reserved for future use.
--
-- 'stringValue', 'messageAttributeValue_stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
--
-- 'dataType', 'messageAttributeValue_dataType' - Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon SQS Developer Guide/.
newMessageAttributeValue ::
  -- | 'dataType'
  Prelude.Text ->
  MessageAttributeValue
newMessageAttributeValue pDataType_ =
  MessageAttributeValue'
    { binaryListValues =
        Prelude.Nothing,
      binaryValue = Prelude.Nothing,
      stringListValues = Prelude.Nothing,
      stringValue = Prelude.Nothing,
      dataType = pDataType_
    }

-- | Not implemented. Reserved for future use.
messageAttributeValue_binaryListValues :: Lens.Lens' MessageAttributeValue (Prelude.Maybe [Prelude.ByteString])
messageAttributeValue_binaryListValues = Lens.lens (\MessageAttributeValue' {binaryListValues} -> binaryListValues) (\s@MessageAttributeValue' {} a -> s {binaryListValues = a} :: MessageAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageAttributeValue_binaryValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.ByteString)
messageAttributeValue_binaryValue = Lens.lens (\MessageAttributeValue' {binaryValue} -> binaryValue) (\s@MessageAttributeValue' {} a -> s {binaryValue = a} :: MessageAttributeValue) Prelude.. Lens.mapping Data._Base64

-- | Not implemented. Reserved for future use.
messageAttributeValue_stringListValues :: Lens.Lens' MessageAttributeValue (Prelude.Maybe [Prelude.Text])
messageAttributeValue_stringListValues = Lens.lens (\MessageAttributeValue' {stringListValues} -> stringListValues) (\s@MessageAttributeValue' {} a -> s {stringListValues = a} :: MessageAttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageAttributeValue_stringValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.Text)
messageAttributeValue_stringValue = Lens.lens (\MessageAttributeValue' {stringValue} -> stringValue) (\s@MessageAttributeValue' {} a -> s {stringValue = a} :: MessageAttributeValue)

-- | Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon SQS Developer Guide/.
messageAttributeValue_dataType :: Lens.Lens' MessageAttributeValue Prelude.Text
messageAttributeValue_dataType = Lens.lens (\MessageAttributeValue' {dataType} -> dataType) (\s@MessageAttributeValue' {} a -> s {dataType = a} :: MessageAttributeValue)

instance Data.FromXML MessageAttributeValue where
  parseXML x =
    MessageAttributeValue'
      Prelude.<$> ( x
                      Data..@? "BinaryListValue"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "BinaryListValue")
                  )
      Prelude.<*> (x Data..@? "BinaryValue")
      Prelude.<*> ( x
                      Data..@? "StringListValue"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "StringListValue")
                  )
      Prelude.<*> (x Data..@? "StringValue")
      Prelude.<*> (x Data..@ "DataType")

instance Prelude.Hashable MessageAttributeValue where
  hashWithSalt _salt MessageAttributeValue' {..} =
    _salt
      `Prelude.hashWithSalt` binaryListValues
      `Prelude.hashWithSalt` binaryValue
      `Prelude.hashWithSalt` stringListValues
      `Prelude.hashWithSalt` stringValue
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData MessageAttributeValue where
  rnf MessageAttributeValue' {..} =
    Prelude.rnf binaryListValues
      `Prelude.seq` Prelude.rnf binaryValue
      `Prelude.seq` Prelude.rnf stringListValues
      `Prelude.seq` Prelude.rnf stringValue
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
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

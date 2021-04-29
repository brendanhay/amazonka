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
-- Module      : Network.AWS.SQS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttributeValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user-specified message attribute value. For string data types, the
-- @Value@ attribute has the same restrictions on the content as the
-- message body. For more information, see @ SendMessage.@
--
-- @Name@, @type@, @value@ and the message body must not be empty or null.
-- All parts of the message attribute, including @Name@, @Type@, and
-- @Value@, are part of the message size restriction (256 KB or 262,144
-- bytes).
--
-- /See:/ 'newMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
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
-- Create a value of 'MessageAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringListValues', 'messageAttributeValue_stringListValues' - Not implemented. Reserved for future use.
--
-- 'stringValue', 'messageAttributeValue_stringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
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
-- 'dataType', 'messageAttributeValue_dataType' - Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
newMessageAttributeValue ::
  -- | 'dataType'
  Prelude.Text ->
  MessageAttributeValue
newMessageAttributeValue pDataType_ =
  MessageAttributeValue'
    { stringListValues =
        Prelude.Nothing,
      stringValue = Prelude.Nothing,
      binaryListValues = Prelude.Nothing,
      binaryValue = Prelude.Nothing,
      dataType = pDataType_
    }

-- | Not implemented. Reserved for future use.
messageAttributeValue_stringListValues :: Lens.Lens' MessageAttributeValue (Prelude.Maybe [Prelude.Text])
messageAttributeValue_stringListValues = Lens.lens (\MessageAttributeValue' {stringListValues} -> stringListValues) (\s@MessageAttributeValue' {} a -> s {stringListValues = a} :: MessageAttributeValue) Prelude.. Lens.mapping Prelude._Coerce

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters>.
messageAttributeValue_stringValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.Text)
messageAttributeValue_stringValue = Lens.lens (\MessageAttributeValue' {stringValue} -> stringValue) (\s@MessageAttributeValue' {} a -> s {stringValue = a} :: MessageAttributeValue)

-- | Not implemented. Reserved for future use.
messageAttributeValue_binaryListValues :: Lens.Lens' MessageAttributeValue (Prelude.Maybe [Prelude.ByteString])
messageAttributeValue_binaryListValues = Lens.lens (\MessageAttributeValue' {binaryListValues} -> binaryListValues) (\s@MessageAttributeValue' {} a -> s {binaryListValues = a} :: MessageAttributeValue) Prelude.. Lens.mapping Prelude._Coerce

-- | Binary type attributes can store any binary data, such as compressed
-- data, encrypted data, or images.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
messageAttributeValue_binaryValue :: Lens.Lens' MessageAttributeValue (Prelude.Maybe Prelude.ByteString)
messageAttributeValue_binaryValue = Lens.lens (\MessageAttributeValue' {binaryValue} -> binaryValue) (\s@MessageAttributeValue' {} a -> s {binaryValue = a} :: MessageAttributeValue) Prelude.. Lens.mapping Prelude._Base64

-- | Amazon SQS supports the following logical data types: @String@,
-- @Number@, and @Binary@. For the @Number@ data type, you must use
-- @StringValue@.
--
-- You can also append custom labels. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
messageAttributeValue_dataType :: Lens.Lens' MessageAttributeValue Prelude.Text
messageAttributeValue_dataType = Lens.lens (\MessageAttributeValue' {dataType} -> dataType) (\s@MessageAttributeValue' {} a -> s {dataType = a} :: MessageAttributeValue)

instance Prelude.FromXML MessageAttributeValue where
  parseXML x =
    MessageAttributeValue'
      Prelude.<$> ( x Prelude..@? "StringListValue"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "StringListValue")
                  )
      Prelude.<*> (x Prelude..@? "StringValue")
      Prelude.<*> ( x Prelude..@? "BinaryListValue"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "BinaryListValue")
                  )
      Prelude.<*> (x Prelude..@? "BinaryValue")
      Prelude.<*> (x Prelude..@ "DataType")

instance Prelude.Hashable MessageAttributeValue

instance Prelude.NFData MessageAttributeValue

instance Prelude.ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
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

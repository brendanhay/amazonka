{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user-specified message attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null. All parts of the message attribute, including @Name@ , @Type@ , and @Value@ , are part of the message size restriction (256 KB or 262,144 bytes).
--
--
-- /See:/ 'messageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { _mavBinaryValue ::
      !(Maybe Base64),
    _mavStringListValues :: !(Maybe [Text]),
    _mavStringValue :: !(Maybe Text),
    _mavBinaryListValues :: !(Maybe [Base64]),
    _mavDataType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mavBinaryValue' - Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mavStringListValues' - Not implemented. Reserved for future use.
--
-- * 'mavStringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- * 'mavBinaryListValues' - Not implemented. Reserved for future use.
--
-- * 'mavDataType' - Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
messageAttributeValue ::
  -- | 'mavDataType'
  Text ->
  MessageAttributeValue
messageAttributeValue pDataType_ =
  MessageAttributeValue'
    { _mavBinaryValue = Nothing,
      _mavStringListValues = Nothing,
      _mavStringValue = Nothing,
      _mavBinaryListValues = Nothing,
      _mavDataType = pDataType_
    }

-- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\s a -> s {_mavBinaryValue = a}) . mapping _Base64

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues = lens _mavStringListValues (\s a -> s {_mavStringListValues = a}) . _Default . _Coerce

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s {_mavStringValue = a})

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [ByteString]
mavBinaryListValues = lens _mavBinaryListValues (\s a -> s {_mavBinaryListValues = a}) . _Default . _Coerce

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s {_mavDataType = a})

instance FromXML MessageAttributeValue where
  parseXML x =
    MessageAttributeValue'
      <$> (x .@? "BinaryValue")
      <*> ( x .@? "StringListValue" .!@ mempty
              >>= may (parseXMLList "StringListValue")
          )
      <*> (x .@? "StringValue")
      <*> ( x .@? "BinaryListValue" .!@ mempty
              >>= may (parseXMLList "BinaryListValue")
          )
      <*> (x .@ "DataType")

instance Hashable MessageAttributeValue

instance NFData MessageAttributeValue

instance ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
    mconcat
      [ "BinaryValue" =: _mavBinaryValue,
        "StringListValue"
          =: toQuery (toQueryList "StringListValue" <$> _mavStringListValues),
        "StringValue" =: _mavStringValue,
        "BinaryListValue"
          =: toQuery (toQueryList "BinaryListValue" <$> _mavBinaryListValues),
        "DataType" =: _mavDataType
      ]

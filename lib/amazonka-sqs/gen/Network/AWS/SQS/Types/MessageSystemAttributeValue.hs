{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageSystemAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageSystemAttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user-specified message system attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null.
--
--
-- /See:/ 'messageSystemAttributeValue' smart constructor.
data MessageSystemAttributeValue = MessageSystemAttributeValue'
  { _msavBinaryValue ::
      !(Maybe Base64),
    _msavStringListValues ::
      !(Maybe [Text]),
    _msavStringValue :: !(Maybe Text),
    _msavBinaryListValues ::
      !(Maybe [Base64]),
    _msavDataType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageSystemAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msavBinaryValue' - Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'msavStringListValues' - Not implemented. Reserved for future use.
--
-- * 'msavStringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- * 'msavBinaryListValues' - Not implemented. Reserved for future use.
--
-- * 'msavDataType' - Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
messageSystemAttributeValue ::
  -- | 'msavDataType'
  Text ->
  MessageSystemAttributeValue
messageSystemAttributeValue pDataType_ =
  MessageSystemAttributeValue'
    { _msavBinaryValue = Nothing,
      _msavStringListValues = Nothing,
      _msavStringValue = Nothing,
      _msavBinaryListValues = Nothing,
      _msavDataType = pDataType_
    }

-- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
msavBinaryValue :: Lens' MessageSystemAttributeValue (Maybe ByteString)
msavBinaryValue = lens _msavBinaryValue (\s a -> s {_msavBinaryValue = a}) . mapping _Base64

-- | Not implemented. Reserved for future use.
msavStringListValues :: Lens' MessageSystemAttributeValue [Text]
msavStringListValues = lens _msavStringListValues (\s a -> s {_msavStringListValues = a}) . _Default . _Coerce

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
msavStringValue :: Lens' MessageSystemAttributeValue (Maybe Text)
msavStringValue = lens _msavStringValue (\s a -> s {_msavStringValue = a})

-- | Not implemented. Reserved for future use.
msavBinaryListValues :: Lens' MessageSystemAttributeValue [ByteString]
msavBinaryListValues = lens _msavBinaryListValues (\s a -> s {_msavBinaryListValues = a}) . _Default . _Coerce

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
msavDataType :: Lens' MessageSystemAttributeValue Text
msavDataType = lens _msavDataType (\s a -> s {_msavDataType = a})

instance Hashable MessageSystemAttributeValue

instance NFData MessageSystemAttributeValue

instance ToQuery MessageSystemAttributeValue where
  toQuery MessageSystemAttributeValue' {..} =
    mconcat
      [ "BinaryValue" =: _msavBinaryValue,
        "StringListValue"
          =: toQuery (toQueryList "StringListValue" <$> _msavStringListValues),
        "StringValue" =: _msavStringValue,
        "BinaryListValue"
          =: toQuery (toQueryList "BinaryListValue" <$> _msavBinaryListValues),
        "DataType" =: _msavDataType
      ]

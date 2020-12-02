{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.MessageAttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> .
--
--
-- Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Amazon SNS message attributes> and <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone> in the /Amazon SNS Developer Guide./
--
--
-- /See:/ 'messageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { _mavBinaryValue ::
      !(Maybe Base64),
    _mavStringValue :: !(Maybe Text),
    _mavDataType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mavBinaryValue' - Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mavStringValue' - Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- * 'mavDataType' - Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
messageAttributeValue ::
  -- | 'mavDataType'
  Text ->
  MessageAttributeValue
messageAttributeValue pDataType_ =
  MessageAttributeValue'
    { _mavBinaryValue = Nothing,
      _mavStringValue = Nothing,
      _mavDataType = pDataType_
    }

-- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\s a -> s {_mavBinaryValue = a}) . mapping _Base64

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s {_mavStringValue = a})

-- | Amazon SNS supports the following logical data types: String, String.Array, Number, and Binary. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s {_mavDataType = a})

instance Hashable MessageAttributeValue

instance NFData MessageAttributeValue

instance ToQuery MessageAttributeValue where
  toQuery MessageAttributeValue' {..} =
    mconcat
      [ "BinaryValue" =: _mavBinaryValue,
        "StringValue" =: _mavStringValue,
        "DataType" =: _mavDataType
      ]

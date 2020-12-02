{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.
--
--
--
-- /See:/ 'typedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { _tavBinaryValue ::
      !(Maybe Base64),
    _tavDatetimeValue :: !(Maybe POSIX),
    _tavNumberValue :: !(Maybe Text),
    _tavStringValue :: !(Maybe Text),
    _tavBooleanValue :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavBinaryValue' - A binary data value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'tavDatetimeValue' - A date and time value.
--
-- * 'tavNumberValue' - A number data value.
--
-- * 'tavStringValue' - A string data value.
--
-- * 'tavBooleanValue' - A Boolean data value.
typedAttributeValue ::
  TypedAttributeValue
typedAttributeValue =
  TypedAttributeValue'
    { _tavBinaryValue = Nothing,
      _tavDatetimeValue = Nothing,
      _tavNumberValue = Nothing,
      _tavStringValue = Nothing,
      _tavBooleanValue = Nothing
    }

-- | A binary data value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
tavBinaryValue :: Lens' TypedAttributeValue (Maybe ByteString)
tavBinaryValue = lens _tavBinaryValue (\s a -> s {_tavBinaryValue = a}) . mapping _Base64

-- | A date and time value.
tavDatetimeValue :: Lens' TypedAttributeValue (Maybe UTCTime)
tavDatetimeValue = lens _tavDatetimeValue (\s a -> s {_tavDatetimeValue = a}) . mapping _Time

-- | A number data value.
tavNumberValue :: Lens' TypedAttributeValue (Maybe Text)
tavNumberValue = lens _tavNumberValue (\s a -> s {_tavNumberValue = a})

-- | A string data value.
tavStringValue :: Lens' TypedAttributeValue (Maybe Text)
tavStringValue = lens _tavStringValue (\s a -> s {_tavStringValue = a})

-- | A Boolean data value.
tavBooleanValue :: Lens' TypedAttributeValue (Maybe Bool)
tavBooleanValue = lens _tavBooleanValue (\s a -> s {_tavBooleanValue = a})

instance FromJSON TypedAttributeValue where
  parseJSON =
    withObject
      "TypedAttributeValue"
      ( \x ->
          TypedAttributeValue'
            <$> (x .:? "BinaryValue")
            <*> (x .:? "DatetimeValue")
            <*> (x .:? "NumberValue")
            <*> (x .:? "StringValue")
            <*> (x .:? "BooleanValue")
      )

instance Hashable TypedAttributeValue

instance NFData TypedAttributeValue

instance ToJSON TypedAttributeValue where
  toJSON TypedAttributeValue' {..} =
    object
      ( catMaybes
          [ ("BinaryValue" .=) <$> _tavBinaryValue,
            ("DatetimeValue" .=) <$> _tavDatetimeValue,
            ("NumberValue" .=) <$> _tavNumberValue,
            ("StringValue" .=) <$> _tavStringValue,
            ("BooleanValue" .=) <$> _tavBooleanValue
          ]
      )

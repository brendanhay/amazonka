{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OpenXJSONSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OpenXJSONSerDe where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
--
--
-- /See:/ 'openXJSONSerDe' smart constructor.
data OpenXJSONSerDe = OpenXJSONSerDe'
  { _oxjsdColumnToJSONKeyMappings ::
      !(Maybe (Map Text (Text))),
    _oxjsdCaseInsensitive :: !(Maybe Bool),
    _oxjsdConvertDotsInJSONKeysToUnderscores :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpenXJSONSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oxjsdColumnToJSONKeyMappings' - Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
--
-- * 'oxjsdCaseInsensitive' - When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
--
-- * 'oxjsdConvertDotsInJSONKeysToUnderscores' - When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option. The default is @false@ .
openXJSONSerDe ::
  OpenXJSONSerDe
openXJSONSerDe =
  OpenXJSONSerDe'
    { _oxjsdColumnToJSONKeyMappings = Nothing,
      _oxjsdCaseInsensitive = Nothing,
      _oxjsdConvertDotsInJSONKeysToUnderscores = Nothing
    }

-- | Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
oxjsdColumnToJSONKeyMappings :: Lens' OpenXJSONSerDe (HashMap Text (Text))
oxjsdColumnToJSONKeyMappings = lens _oxjsdColumnToJSONKeyMappings (\s a -> s {_oxjsdColumnToJSONKeyMappings = a}) . _Default . _Map

-- | When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
oxjsdCaseInsensitive :: Lens' OpenXJSONSerDe (Maybe Bool)
oxjsdCaseInsensitive = lens _oxjsdCaseInsensitive (\s a -> s {_oxjsdCaseInsensitive = a})

-- | When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option. The default is @false@ .
oxjsdConvertDotsInJSONKeysToUnderscores :: Lens' OpenXJSONSerDe (Maybe Bool)
oxjsdConvertDotsInJSONKeysToUnderscores = lens _oxjsdConvertDotsInJSONKeysToUnderscores (\s a -> s {_oxjsdConvertDotsInJSONKeysToUnderscores = a})

instance FromJSON OpenXJSONSerDe where
  parseJSON =
    withObject
      "OpenXJSONSerDe"
      ( \x ->
          OpenXJSONSerDe'
            <$> (x .:? "ColumnToJsonKeyMappings" .!= mempty)
            <*> (x .:? "CaseInsensitive")
            <*> (x .:? "ConvertDotsInJsonKeysToUnderscores")
      )

instance Hashable OpenXJSONSerDe

instance NFData OpenXJSONSerDe

instance ToJSON OpenXJSONSerDe where
  toJSON OpenXJSONSerDe' {..} =
    object
      ( catMaybes
          [ ("ColumnToJsonKeyMappings" .=) <$> _oxjsdColumnToJSONKeyMappings,
            ("CaseInsensitive" .=) <$> _oxjsdCaseInsensitive,
            ("ConvertDotsInJsonKeysToUnderscores" .=)
              <$> _oxjsdConvertDotsInJSONKeysToUnderscores
          ]
      )

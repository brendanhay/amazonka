{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SerDeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SerDeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a serialization/deserialization program (SerDe) that serves as an extractor and loader.
--
--
--
-- /See:/ 'serDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { _sdiSerializationLibrary ::
      !(Maybe Text),
    _sdiName :: !(Maybe Text),
    _sdiParameters :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SerDeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiSerializationLibrary' - Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
--
-- * 'sdiName' - Name of the SerDe.
--
-- * 'sdiParameters' - These key-value pairs define initialization parameters for the SerDe.
serDeInfo ::
  SerDeInfo
serDeInfo =
  SerDeInfo'
    { _sdiSerializationLibrary = Nothing,
      _sdiName = Nothing,
      _sdiParameters = Nothing
    }

-- | Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
sdiSerializationLibrary :: Lens' SerDeInfo (Maybe Text)
sdiSerializationLibrary = lens _sdiSerializationLibrary (\s a -> s {_sdiSerializationLibrary = a})

-- | Name of the SerDe.
sdiName :: Lens' SerDeInfo (Maybe Text)
sdiName = lens _sdiName (\s a -> s {_sdiName = a})

-- | These key-value pairs define initialization parameters for the SerDe.
sdiParameters :: Lens' SerDeInfo (HashMap Text (Text))
sdiParameters = lens _sdiParameters (\s a -> s {_sdiParameters = a}) . _Default . _Map

instance FromJSON SerDeInfo where
  parseJSON =
    withObject
      "SerDeInfo"
      ( \x ->
          SerDeInfo'
            <$> (x .:? "SerializationLibrary")
            <*> (x .:? "Name")
            <*> (x .:? "Parameters" .!= mempty)
      )

instance Hashable SerDeInfo

instance NFData SerDeInfo

instance ToJSON SerDeInfo where
  toJSON SerDeInfo' {..} =
    object
      ( catMaybes
          [ ("SerializationLibrary" .=) <$> _sdiSerializationLibrary,
            ("Name" .=) <$> _sdiName,
            ("Parameters" .=) <$> _sdiParameters
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.JSONMappingParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
--
--
-- /See:/ 'jsonMappingParameters' smart constructor.
newtype JSONMappingParameters = JSONMappingParameters'
  { _jmpRecordRowPath ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JSONMappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jmpRecordRowPath' - Path to the top-level parent that contains the records.
jsonMappingParameters ::
  -- | 'jmpRecordRowPath'
  Text ->
  JSONMappingParameters
jsonMappingParameters pRecordRowPath_ =
  JSONMappingParameters' {_jmpRecordRowPath = pRecordRowPath_}

-- | Path to the top-level parent that contains the records.
jmpRecordRowPath :: Lens' JSONMappingParameters Text
jmpRecordRowPath = lens _jmpRecordRowPath (\s a -> s {_jmpRecordRowPath = a})

instance FromJSON JSONMappingParameters where
  parseJSON =
    withObject
      "JSONMappingParameters"
      (\x -> JSONMappingParameters' <$> (x .: "RecordRowPath"))

instance Hashable JSONMappingParameters

instance NFData JSONMappingParameters

instance ToJSON JSONMappingParameters where
  toJSON JSONMappingParameters' {..} =
    object (catMaybes [Just ("RecordRowPath" .= _jmpRecordRowPath)])

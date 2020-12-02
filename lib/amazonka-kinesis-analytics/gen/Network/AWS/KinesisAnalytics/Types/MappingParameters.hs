{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.MappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.MappingParameters where

import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
--
--
-- /See:/ 'mappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { _mpCSVMappingParameters ::
      !(Maybe CSVMappingParameters),
    _mpJSONMappingParameters ::
      !(Maybe JSONMappingParameters)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpCSVMappingParameters' - Provides additional mapping information when the record format uses delimiters (for example, CSV).
--
-- * 'mpJSONMappingParameters' - Provides additional mapping information when JSON is the record format on the streaming source.
mappingParameters ::
  MappingParameters
mappingParameters =
  MappingParameters'
    { _mpCSVMappingParameters = Nothing,
      _mpJSONMappingParameters = Nothing
    }

-- | Provides additional mapping information when the record format uses delimiters (for example, CSV).
mpCSVMappingParameters :: Lens' MappingParameters (Maybe CSVMappingParameters)
mpCSVMappingParameters = lens _mpCSVMappingParameters (\s a -> s {_mpCSVMappingParameters = a})

-- | Provides additional mapping information when JSON is the record format on the streaming source.
mpJSONMappingParameters :: Lens' MappingParameters (Maybe JSONMappingParameters)
mpJSONMappingParameters = lens _mpJSONMappingParameters (\s a -> s {_mpJSONMappingParameters = a})

instance FromJSON MappingParameters where
  parseJSON =
    withObject
      "MappingParameters"
      ( \x ->
          MappingParameters'
            <$> (x .:? "CSVMappingParameters") <*> (x .:? "JSONMappingParameters")
      )

instance Hashable MappingParameters

instance NFData MappingParameters

instance ToJSON MappingParameters where
  toJSON MappingParameters' {..} =
    object
      ( catMaybes
          [ ("CSVMappingParameters" .=) <$> _mpCSVMappingParameters,
            ("JSONMappingParameters" .=) <$> _mpJSONMappingParameters
          ]
      )

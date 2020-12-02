{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordFormat where

import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the record format and relevant mapping information that should be applied to schematize the records on the stream.
--
--
--
-- /See:/ 'recordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { _rfMappingParameters ::
      !(Maybe MappingParameters),
    _rfRecordFormatType :: !RecordFormatType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfMappingParameters' - When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- * 'rfRecordFormatType' - The type of record format.
recordFormat ::
  -- | 'rfRecordFormatType'
  RecordFormatType ->
  RecordFormat
recordFormat pRecordFormatType_ =
  RecordFormat'
    { _rfMappingParameters = Nothing,
      _rfRecordFormatType = pRecordFormatType_
    }

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
rfMappingParameters :: Lens' RecordFormat (Maybe MappingParameters)
rfMappingParameters = lens _rfMappingParameters (\s a -> s {_rfMappingParameters = a})

-- | The type of record format.
rfRecordFormatType :: Lens' RecordFormat RecordFormatType
rfRecordFormatType = lens _rfRecordFormatType (\s a -> s {_rfRecordFormatType = a})

instance FromJSON RecordFormat where
  parseJSON =
    withObject
      "RecordFormat"
      ( \x ->
          RecordFormat'
            <$> (x .:? "MappingParameters") <*> (x .: "RecordFormatType")
      )

instance Hashable RecordFormat

instance NFData RecordFormat

instance ToJSON RecordFormat where
  toJSON RecordFormat' {..} =
    object
      ( catMaybes
          [ ("MappingParameters" .=) <$> _rfMappingParameters,
            Just ("RecordFormatType" .= _rfRecordFormatType)
          ]
      )

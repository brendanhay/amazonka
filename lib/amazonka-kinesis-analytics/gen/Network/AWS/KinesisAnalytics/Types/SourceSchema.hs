{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.SourceSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.SourceSchema where

import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
--
--
-- /See:/ 'sourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { _ssRecordEncoding ::
      !(Maybe Text),
    _ssRecordFormat :: !RecordFormat,
    _ssRecordColumns :: !(List1 RecordColumn)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssRecordEncoding' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- * 'ssRecordFormat' - Specifies the format of the records on the streaming source.
--
-- * 'ssRecordColumns' - A list of @RecordColumn@ objects.
sourceSchema ::
  -- | 'ssRecordFormat'
  RecordFormat ->
  -- | 'ssRecordColumns'
  NonEmpty RecordColumn ->
  SourceSchema
sourceSchema pRecordFormat_ pRecordColumns_ =
  SourceSchema'
    { _ssRecordEncoding = Nothing,
      _ssRecordFormat = pRecordFormat_,
      _ssRecordColumns = _List1 # pRecordColumns_
    }

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
ssRecordEncoding :: Lens' SourceSchema (Maybe Text)
ssRecordEncoding = lens _ssRecordEncoding (\s a -> s {_ssRecordEncoding = a})

-- | Specifies the format of the records on the streaming source.
ssRecordFormat :: Lens' SourceSchema RecordFormat
ssRecordFormat = lens _ssRecordFormat (\s a -> s {_ssRecordFormat = a})

-- | A list of @RecordColumn@ objects.
ssRecordColumns :: Lens' SourceSchema (NonEmpty RecordColumn)
ssRecordColumns = lens _ssRecordColumns (\s a -> s {_ssRecordColumns = a}) . _List1

instance FromJSON SourceSchema where
  parseJSON =
    withObject
      "SourceSchema"
      ( \x ->
          SourceSchema'
            <$> (x .:? "RecordEncoding")
            <*> (x .: "RecordFormat")
            <*> (x .: "RecordColumns")
      )

instance Hashable SourceSchema

instance NFData SourceSchema

instance ToJSON SourceSchema where
  toJSON SourceSchema' {..} =
    object
      ( catMaybes
          [ ("RecordEncoding" .=) <$> _ssRecordEncoding,
            Just ("RecordFormat" .= _ssRecordFormat),
            Just ("RecordColumns" .= _ssRecordColumns)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordColumn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordColumn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.
--
--
-- Also used to describe the format of the reference data source.
--
--
-- /See:/ 'recordColumn' smart constructor.
data RecordColumn = RecordColumn'
  { _rcMapping :: !(Maybe Text),
    _rcName :: !Text,
    _rcSqlType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordColumn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcMapping' - Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
--
-- * 'rcName' - Name of the column created in the in-application input stream or reference table.
--
-- * 'rcSqlType' - Type of column created in the in-application input stream or reference table.
recordColumn ::
  -- | 'rcName'
  Text ->
  -- | 'rcSqlType'
  Text ->
  RecordColumn
recordColumn pName_ pSqlType_ =
  RecordColumn'
    { _rcMapping = Nothing,
      _rcName = pName_,
      _rcSqlType = pSqlType_
    }

-- | Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
rcMapping :: Lens' RecordColumn (Maybe Text)
rcMapping = lens _rcMapping (\s a -> s {_rcMapping = a})

-- | Name of the column created in the in-application input stream or reference table.
rcName :: Lens' RecordColumn Text
rcName = lens _rcName (\s a -> s {_rcName = a})

-- | Type of column created in the in-application input stream or reference table.
rcSqlType :: Lens' RecordColumn Text
rcSqlType = lens _rcSqlType (\s a -> s {_rcSqlType = a})

instance FromJSON RecordColumn where
  parseJSON =
    withObject
      "RecordColumn"
      ( \x ->
          RecordColumn'
            <$> (x .:? "Mapping") <*> (x .: "Name") <*> (x .: "SqlType")
      )

instance Hashable RecordColumn

instance NFData RecordColumn

instance ToJSON RecordColumn where
  toJSON RecordColumn' {..} =
    object
      ( catMaybes
          [ ("Mapping" .=) <$> _rcMapping,
            Just ("Name" .= _rcName),
            Just ("SqlType" .= _rcSqlType)
          ]
      )

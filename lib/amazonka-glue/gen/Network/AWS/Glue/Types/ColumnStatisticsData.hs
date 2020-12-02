{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsData where

import Network.AWS.Glue.Types.BinaryColumnStatisticsData
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
import Network.AWS.Glue.Types.ColumnStatisticsType
import Network.AWS.Glue.Types.DateColumnStatisticsData
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
import Network.AWS.Glue.Types.LongColumnStatisticsData
import Network.AWS.Glue.Types.StringColumnStatisticsData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the individual types of column statistics data. Only one data object should be set and indicated by the @Type@ attribute.
--
--
--
-- /See:/ 'columnStatisticsData' smart constructor.
data ColumnStatisticsData = ColumnStatisticsData'
  { _csdBinaryColumnStatisticsData ::
      !(Maybe BinaryColumnStatisticsData),
    _csdDateColumnStatisticsData ::
      !(Maybe DateColumnStatisticsData),
    _csdBooleanColumnStatisticsData ::
      !(Maybe BooleanColumnStatisticsData),
    _csdDecimalColumnStatisticsData ::
      !(Maybe DecimalColumnStatisticsData),
    _csdDoubleColumnStatisticsData ::
      !(Maybe DoubleColumnStatisticsData),
    _csdStringColumnStatisticsData ::
      !(Maybe StringColumnStatisticsData),
    _csdLongColumnStatisticsData ::
      !(Maybe LongColumnStatisticsData),
    _csdType :: !ColumnStatisticsType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdBinaryColumnStatisticsData' - Binary column statistics data.
--
-- * 'csdDateColumnStatisticsData' - Date column statistics data.
--
-- * 'csdBooleanColumnStatisticsData' - Boolean column statistics data.
--
-- * 'csdDecimalColumnStatisticsData' - Decimal column statistics data.
--
-- * 'csdDoubleColumnStatisticsData' - Double column statistics data.
--
-- * 'csdStringColumnStatisticsData' - String column statistics data.
--
-- * 'csdLongColumnStatisticsData' - Long column statistics data.
--
-- * 'csdType' - The type of column statistics data.
columnStatisticsData ::
  -- | 'csdType'
  ColumnStatisticsType ->
  ColumnStatisticsData
columnStatisticsData pType_ =
  ColumnStatisticsData'
    { _csdBinaryColumnStatisticsData = Nothing,
      _csdDateColumnStatisticsData = Nothing,
      _csdBooleanColumnStatisticsData = Nothing,
      _csdDecimalColumnStatisticsData = Nothing,
      _csdDoubleColumnStatisticsData = Nothing,
      _csdStringColumnStatisticsData = Nothing,
      _csdLongColumnStatisticsData = Nothing,
      _csdType = pType_
    }

-- | Binary column statistics data.
csdBinaryColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe BinaryColumnStatisticsData)
csdBinaryColumnStatisticsData = lens _csdBinaryColumnStatisticsData (\s a -> s {_csdBinaryColumnStatisticsData = a})

-- | Date column statistics data.
csdDateColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe DateColumnStatisticsData)
csdDateColumnStatisticsData = lens _csdDateColumnStatisticsData (\s a -> s {_csdDateColumnStatisticsData = a})

-- | Boolean column statistics data.
csdBooleanColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe BooleanColumnStatisticsData)
csdBooleanColumnStatisticsData = lens _csdBooleanColumnStatisticsData (\s a -> s {_csdBooleanColumnStatisticsData = a})

-- | Decimal column statistics data.
csdDecimalColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe DecimalColumnStatisticsData)
csdDecimalColumnStatisticsData = lens _csdDecimalColumnStatisticsData (\s a -> s {_csdDecimalColumnStatisticsData = a})

-- | Double column statistics data.
csdDoubleColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe DoubleColumnStatisticsData)
csdDoubleColumnStatisticsData = lens _csdDoubleColumnStatisticsData (\s a -> s {_csdDoubleColumnStatisticsData = a})

-- | String column statistics data.
csdStringColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe StringColumnStatisticsData)
csdStringColumnStatisticsData = lens _csdStringColumnStatisticsData (\s a -> s {_csdStringColumnStatisticsData = a})

-- | Long column statistics data.
csdLongColumnStatisticsData :: Lens' ColumnStatisticsData (Maybe LongColumnStatisticsData)
csdLongColumnStatisticsData = lens _csdLongColumnStatisticsData (\s a -> s {_csdLongColumnStatisticsData = a})

-- | The type of column statistics data.
csdType :: Lens' ColumnStatisticsData ColumnStatisticsType
csdType = lens _csdType (\s a -> s {_csdType = a})

instance FromJSON ColumnStatisticsData where
  parseJSON =
    withObject
      "ColumnStatisticsData"
      ( \x ->
          ColumnStatisticsData'
            <$> (x .:? "BinaryColumnStatisticsData")
            <*> (x .:? "DateColumnStatisticsData")
            <*> (x .:? "BooleanColumnStatisticsData")
            <*> (x .:? "DecimalColumnStatisticsData")
            <*> (x .:? "DoubleColumnStatisticsData")
            <*> (x .:? "StringColumnStatisticsData")
            <*> (x .:? "LongColumnStatisticsData")
            <*> (x .: "Type")
      )

instance Hashable ColumnStatisticsData

instance NFData ColumnStatisticsData

instance ToJSON ColumnStatisticsData where
  toJSON ColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ ("BinaryColumnStatisticsData" .=)
              <$> _csdBinaryColumnStatisticsData,
            ("DateColumnStatisticsData" .=) <$> _csdDateColumnStatisticsData,
            ("BooleanColumnStatisticsData" .=)
              <$> _csdBooleanColumnStatisticsData,
            ("DecimalColumnStatisticsData" .=)
              <$> _csdDecimalColumnStatisticsData,
            ("DoubleColumnStatisticsData" .=)
              <$> _csdDoubleColumnStatisticsData,
            ("StringColumnStatisticsData" .=)
              <$> _csdStringColumnStatisticsData,
            ("LongColumnStatisticsData" .=) <$> _csdLongColumnStatisticsData,
            Just ("Type" .= _csdType)
          ]
      )

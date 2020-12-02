{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DateColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DateColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for timestamp data columns.
--
--
--
-- /See:/ 'dateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { _dcsdMaximumValue ::
      !(Maybe POSIX),
    _dcsdMinimumValue :: !(Maybe POSIX),
    _dcsdNumberOfNulls :: !Nat,
    _dcsdNumberOfDistinctValues :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DateColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsdMaximumValue' - The highest value in the column.
--
-- * 'dcsdMinimumValue' - The lowest value in the column.
--
-- * 'dcsdNumberOfNulls' - The number of null values in the column.
--
-- * 'dcsdNumberOfDistinctValues' - The number of distinct values in a column.
dateColumnStatisticsData ::
  -- | 'dcsdNumberOfNulls'
  Natural ->
  -- | 'dcsdNumberOfDistinctValues'
  Natural ->
  DateColumnStatisticsData
dateColumnStatisticsData pNumberOfNulls_ pNumberOfDistinctValues_ =
  DateColumnStatisticsData'
    { _dcsdMaximumValue = Nothing,
      _dcsdMinimumValue = Nothing,
      _dcsdNumberOfNulls = _Nat # pNumberOfNulls_,
      _dcsdNumberOfDistinctValues = _Nat # pNumberOfDistinctValues_
    }

-- | The highest value in the column.
dcsdMaximumValue :: Lens' DateColumnStatisticsData (Maybe UTCTime)
dcsdMaximumValue = lens _dcsdMaximumValue (\s a -> s {_dcsdMaximumValue = a}) . mapping _Time

-- | The lowest value in the column.
dcsdMinimumValue :: Lens' DateColumnStatisticsData (Maybe UTCTime)
dcsdMinimumValue = lens _dcsdMinimumValue (\s a -> s {_dcsdMinimumValue = a}) . mapping _Time

-- | The number of null values in the column.
dcsdNumberOfNulls :: Lens' DateColumnStatisticsData Natural
dcsdNumberOfNulls = lens _dcsdNumberOfNulls (\s a -> s {_dcsdNumberOfNulls = a}) . _Nat

-- | The number of distinct values in a column.
dcsdNumberOfDistinctValues :: Lens' DateColumnStatisticsData Natural
dcsdNumberOfDistinctValues = lens _dcsdNumberOfDistinctValues (\s a -> s {_dcsdNumberOfDistinctValues = a}) . _Nat

instance FromJSON DateColumnStatisticsData where
  parseJSON =
    withObject
      "DateColumnStatisticsData"
      ( \x ->
          DateColumnStatisticsData'
            <$> (x .:? "MaximumValue")
            <*> (x .:? "MinimumValue")
            <*> (x .: "NumberOfNulls")
            <*> (x .: "NumberOfDistinctValues")
      )

instance Hashable DateColumnStatisticsData

instance NFData DateColumnStatisticsData

instance ToJSON DateColumnStatisticsData where
  toJSON DateColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ ("MaximumValue" .=) <$> _dcsdMaximumValue,
            ("MinimumValue" .=) <$> _dcsdMinimumValue,
            Just ("NumberOfNulls" .= _dcsdNumberOfNulls),
            Just ("NumberOfDistinctValues" .= _dcsdNumberOfDistinctValues)
          ]
      )

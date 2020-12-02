{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DoubleColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DoubleColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for floating-point number data columns.
--
--
--
-- /See:/ 'doubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { _douMaximumValue ::
      !(Maybe Double),
    _douMinimumValue :: !(Maybe Double),
    _douNumberOfNulls :: !Nat,
    _douNumberOfDistinctValues :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DoubleColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'douMaximumValue' - The highest value in the column.
--
-- * 'douMinimumValue' - The lowest value in the column.
--
-- * 'douNumberOfNulls' - The number of null values in the column.
--
-- * 'douNumberOfDistinctValues' - The number of distinct values in a column.
doubleColumnStatisticsData ::
  -- | 'douNumberOfNulls'
  Natural ->
  -- | 'douNumberOfDistinctValues'
  Natural ->
  DoubleColumnStatisticsData
doubleColumnStatisticsData pNumberOfNulls_ pNumberOfDistinctValues_ =
  DoubleColumnStatisticsData'
    { _douMaximumValue = Nothing,
      _douMinimumValue = Nothing,
      _douNumberOfNulls = _Nat # pNumberOfNulls_,
      _douNumberOfDistinctValues = _Nat # pNumberOfDistinctValues_
    }

-- | The highest value in the column.
douMaximumValue :: Lens' DoubleColumnStatisticsData (Maybe Double)
douMaximumValue = lens _douMaximumValue (\s a -> s {_douMaximumValue = a})

-- | The lowest value in the column.
douMinimumValue :: Lens' DoubleColumnStatisticsData (Maybe Double)
douMinimumValue = lens _douMinimumValue (\s a -> s {_douMinimumValue = a})

-- | The number of null values in the column.
douNumberOfNulls :: Lens' DoubleColumnStatisticsData Natural
douNumberOfNulls = lens _douNumberOfNulls (\s a -> s {_douNumberOfNulls = a}) . _Nat

-- | The number of distinct values in a column.
douNumberOfDistinctValues :: Lens' DoubleColumnStatisticsData Natural
douNumberOfDistinctValues = lens _douNumberOfDistinctValues (\s a -> s {_douNumberOfDistinctValues = a}) . _Nat

instance FromJSON DoubleColumnStatisticsData where
  parseJSON =
    withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            <$> (x .:? "MaximumValue")
            <*> (x .:? "MinimumValue")
            <*> (x .: "NumberOfNulls")
            <*> (x .: "NumberOfDistinctValues")
      )

instance Hashable DoubleColumnStatisticsData

instance NFData DoubleColumnStatisticsData

instance ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ ("MaximumValue" .=) <$> _douMaximumValue,
            ("MinimumValue" .=) <$> _douMinimumValue,
            Just ("NumberOfNulls" .= _douNumberOfNulls),
            Just ("NumberOfDistinctValues" .= _douNumberOfDistinctValues)
          ]
      )

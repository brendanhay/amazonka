{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LongColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for integer data columns.
--
--
--
-- /See:/ 'longColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { _lcsdMaximumValue ::
      !(Maybe Integer),
    _lcsdMinimumValue :: !(Maybe Integer),
    _lcsdNumberOfNulls :: !Nat,
    _lcsdNumberOfDistinctValues :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LongColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsdMaximumValue' - The highest value in the column.
--
-- * 'lcsdMinimumValue' - The lowest value in the column.
--
-- * 'lcsdNumberOfNulls' - The number of null values in the column.
--
-- * 'lcsdNumberOfDistinctValues' - The number of distinct values in a column.
longColumnStatisticsData ::
  -- | 'lcsdNumberOfNulls'
  Natural ->
  -- | 'lcsdNumberOfDistinctValues'
  Natural ->
  LongColumnStatisticsData
longColumnStatisticsData pNumberOfNulls_ pNumberOfDistinctValues_ =
  LongColumnStatisticsData'
    { _lcsdMaximumValue = Nothing,
      _lcsdMinimumValue = Nothing,
      _lcsdNumberOfNulls = _Nat # pNumberOfNulls_,
      _lcsdNumberOfDistinctValues = _Nat # pNumberOfDistinctValues_
    }

-- | The highest value in the column.
lcsdMaximumValue :: Lens' LongColumnStatisticsData (Maybe Integer)
lcsdMaximumValue = lens _lcsdMaximumValue (\s a -> s {_lcsdMaximumValue = a})

-- | The lowest value in the column.
lcsdMinimumValue :: Lens' LongColumnStatisticsData (Maybe Integer)
lcsdMinimumValue = lens _lcsdMinimumValue (\s a -> s {_lcsdMinimumValue = a})

-- | The number of null values in the column.
lcsdNumberOfNulls :: Lens' LongColumnStatisticsData Natural
lcsdNumberOfNulls = lens _lcsdNumberOfNulls (\s a -> s {_lcsdNumberOfNulls = a}) . _Nat

-- | The number of distinct values in a column.
lcsdNumberOfDistinctValues :: Lens' LongColumnStatisticsData Natural
lcsdNumberOfDistinctValues = lens _lcsdNumberOfDistinctValues (\s a -> s {_lcsdNumberOfDistinctValues = a}) . _Nat

instance FromJSON LongColumnStatisticsData where
  parseJSON =
    withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            <$> (x .:? "MaximumValue")
            <*> (x .:? "MinimumValue")
            <*> (x .: "NumberOfNulls")
            <*> (x .: "NumberOfDistinctValues")
      )

instance Hashable LongColumnStatisticsData

instance NFData LongColumnStatisticsData

instance ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ ("MaximumValue" .=) <$> _lcsdMaximumValue,
            ("MinimumValue" .=) <$> _lcsdMinimumValue,
            Just ("NumberOfNulls" .= _lcsdNumberOfNulls),
            Just ("NumberOfDistinctValues" .= _lcsdNumberOfDistinctValues)
          ]
      )

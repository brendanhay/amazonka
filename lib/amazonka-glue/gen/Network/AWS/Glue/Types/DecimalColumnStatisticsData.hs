{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DecimalColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DecimalColumnStatisticsData where

import Network.AWS.Glue.Types.DecimalNumber
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for fixed-point number data columns.
--
--
--
-- /See:/ 'decimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { _dMaximumValue ::
      !(Maybe DecimalNumber),
    _dMinimumValue ::
      !(Maybe DecimalNumber),
    _dNumberOfNulls :: !Nat,
    _dNumberOfDistinctValues :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecimalColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMaximumValue' - The highest value in the column.
--
-- * 'dMinimumValue' - The lowest value in the column.
--
-- * 'dNumberOfNulls' - The number of null values in the column.
--
-- * 'dNumberOfDistinctValues' - The number of distinct values in a column.
decimalColumnStatisticsData ::
  -- | 'dNumberOfNulls'
  Natural ->
  -- | 'dNumberOfDistinctValues'
  Natural ->
  DecimalColumnStatisticsData
decimalColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DecimalColumnStatisticsData'
      { _dMaximumValue = Nothing,
        _dMinimumValue = Nothing,
        _dNumberOfNulls = _Nat # pNumberOfNulls_,
        _dNumberOfDistinctValues = _Nat # pNumberOfDistinctValues_
      }

-- | The highest value in the column.
dMaximumValue :: Lens' DecimalColumnStatisticsData (Maybe DecimalNumber)
dMaximumValue = lens _dMaximumValue (\s a -> s {_dMaximumValue = a})

-- | The lowest value in the column.
dMinimumValue :: Lens' DecimalColumnStatisticsData (Maybe DecimalNumber)
dMinimumValue = lens _dMinimumValue (\s a -> s {_dMinimumValue = a})

-- | The number of null values in the column.
dNumberOfNulls :: Lens' DecimalColumnStatisticsData Natural
dNumberOfNulls = lens _dNumberOfNulls (\s a -> s {_dNumberOfNulls = a}) . _Nat

-- | The number of distinct values in a column.
dNumberOfDistinctValues :: Lens' DecimalColumnStatisticsData Natural
dNumberOfDistinctValues = lens _dNumberOfDistinctValues (\s a -> s {_dNumberOfDistinctValues = a}) . _Nat

instance FromJSON DecimalColumnStatisticsData where
  parseJSON =
    withObject
      "DecimalColumnStatisticsData"
      ( \x ->
          DecimalColumnStatisticsData'
            <$> (x .:? "MaximumValue")
            <*> (x .:? "MinimumValue")
            <*> (x .: "NumberOfNulls")
            <*> (x .: "NumberOfDistinctValues")
      )

instance Hashable DecimalColumnStatisticsData

instance NFData DecimalColumnStatisticsData

instance ToJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ ("MaximumValue" .=) <$> _dMaximumValue,
            ("MinimumValue" .=) <$> _dMinimumValue,
            Just ("NumberOfNulls" .= _dNumberOfNulls),
            Just ("NumberOfDistinctValues" .= _dNumberOfDistinctValues)
          ]
      )

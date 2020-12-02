{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.StringColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StringColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for character sequence data values.
--
--
--
-- /See:/ 'stringColumnStatisticsData' smart constructor.
data StringColumnStatisticsData = StringColumnStatisticsData'
  { _scsdMaximumLength ::
      !Nat,
    _scsdAverageLength :: !Double,
    _scsdNumberOfNulls :: !Nat,
    _scsdNumberOfDistinctValues :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StringColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsdMaximumLength' - The size of the longest string in the column.
--
-- * 'scsdAverageLength' - The average string length in the column.
--
-- * 'scsdNumberOfNulls' - The number of null values in the column.
--
-- * 'scsdNumberOfDistinctValues' - The number of distinct values in a column.
stringColumnStatisticsData ::
  -- | 'scsdMaximumLength'
  Natural ->
  -- | 'scsdAverageLength'
  Double ->
  -- | 'scsdNumberOfNulls'
  Natural ->
  -- | 'scsdNumberOfDistinctValues'
  Natural ->
  StringColumnStatisticsData
stringColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    StringColumnStatisticsData'
      { _scsdMaximumLength =
          _Nat # pMaximumLength_,
        _scsdAverageLength = pAverageLength_,
        _scsdNumberOfNulls = _Nat # pNumberOfNulls_,
        _scsdNumberOfDistinctValues = _Nat # pNumberOfDistinctValues_
      }

-- | The size of the longest string in the column.
scsdMaximumLength :: Lens' StringColumnStatisticsData Natural
scsdMaximumLength = lens _scsdMaximumLength (\s a -> s {_scsdMaximumLength = a}) . _Nat

-- | The average string length in the column.
scsdAverageLength :: Lens' StringColumnStatisticsData Double
scsdAverageLength = lens _scsdAverageLength (\s a -> s {_scsdAverageLength = a})

-- | The number of null values in the column.
scsdNumberOfNulls :: Lens' StringColumnStatisticsData Natural
scsdNumberOfNulls = lens _scsdNumberOfNulls (\s a -> s {_scsdNumberOfNulls = a}) . _Nat

-- | The number of distinct values in a column.
scsdNumberOfDistinctValues :: Lens' StringColumnStatisticsData Natural
scsdNumberOfDistinctValues = lens _scsdNumberOfDistinctValues (\s a -> s {_scsdNumberOfDistinctValues = a}) . _Nat

instance FromJSON StringColumnStatisticsData where
  parseJSON =
    withObject
      "StringColumnStatisticsData"
      ( \x ->
          StringColumnStatisticsData'
            <$> (x .: "MaximumLength")
            <*> (x .: "AverageLength")
            <*> (x .: "NumberOfNulls")
            <*> (x .: "NumberOfDistinctValues")
      )

instance Hashable StringColumnStatisticsData

instance NFData StringColumnStatisticsData

instance ToJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ Just ("MaximumLength" .= _scsdMaximumLength),
            Just ("AverageLength" .= _scsdAverageLength),
            Just ("NumberOfNulls" .= _scsdNumberOfNulls),
            Just ("NumberOfDistinctValues" .= _scsdNumberOfDistinctValues)
          ]
      )

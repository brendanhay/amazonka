{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BinaryColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BinaryColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for bit sequence data values.
--
--
--
-- /See:/ 'binaryColumnStatisticsData' smart constructor.
data BinaryColumnStatisticsData = BinaryColumnStatisticsData'
  { _bcsdMaximumLength ::
      !Nat,
    _bcsdAverageLength :: !Double,
    _bcsdNumberOfNulls :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BinaryColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcsdMaximumLength' - The size of the longest bit sequence in the column.
--
-- * 'bcsdAverageLength' - The average bit sequence length in the column.
--
-- * 'bcsdNumberOfNulls' - The number of null values in the column.
binaryColumnStatisticsData ::
  -- | 'bcsdMaximumLength'
  Natural ->
  -- | 'bcsdAverageLength'
  Double ->
  -- | 'bcsdNumberOfNulls'
  Natural ->
  BinaryColumnStatisticsData
binaryColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_ =
    BinaryColumnStatisticsData'
      { _bcsdMaximumLength =
          _Nat # pMaximumLength_,
        _bcsdAverageLength = pAverageLength_,
        _bcsdNumberOfNulls = _Nat # pNumberOfNulls_
      }

-- | The size of the longest bit sequence in the column.
bcsdMaximumLength :: Lens' BinaryColumnStatisticsData Natural
bcsdMaximumLength = lens _bcsdMaximumLength (\s a -> s {_bcsdMaximumLength = a}) . _Nat

-- | The average bit sequence length in the column.
bcsdAverageLength :: Lens' BinaryColumnStatisticsData Double
bcsdAverageLength = lens _bcsdAverageLength (\s a -> s {_bcsdAverageLength = a})

-- | The number of null values in the column.
bcsdNumberOfNulls :: Lens' BinaryColumnStatisticsData Natural
bcsdNumberOfNulls = lens _bcsdNumberOfNulls (\s a -> s {_bcsdNumberOfNulls = a}) . _Nat

instance FromJSON BinaryColumnStatisticsData where
  parseJSON =
    withObject
      "BinaryColumnStatisticsData"
      ( \x ->
          BinaryColumnStatisticsData'
            <$> (x .: "MaximumLength")
            <*> (x .: "AverageLength")
            <*> (x .: "NumberOfNulls")
      )

instance Hashable BinaryColumnStatisticsData

instance NFData BinaryColumnStatisticsData

instance ToJSON BinaryColumnStatisticsData where
  toJSON BinaryColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ Just ("MaximumLength" .= _bcsdMaximumLength),
            Just ("AverageLength" .= _bcsdAverageLength),
            Just ("NumberOfNulls" .= _bcsdNumberOfNulls)
          ]
      )

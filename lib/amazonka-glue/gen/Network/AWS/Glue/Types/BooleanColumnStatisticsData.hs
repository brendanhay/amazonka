{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BooleanColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BooleanColumnStatisticsData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines column statistics supported for Boolean data columns.
--
--
--
-- /See:/ 'booleanColumnStatisticsData' smart constructor.
data BooleanColumnStatisticsData = BooleanColumnStatisticsData'
  { _bNumberOfTrues ::
      !Nat,
    _bNumberOfFalses :: !Nat,
    _bNumberOfNulls :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BooleanColumnStatisticsData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bNumberOfTrues' - The number of true values in the column.
--
-- * 'bNumberOfFalses' - The number of false values in the column.
--
-- * 'bNumberOfNulls' - The number of null values in the column.
booleanColumnStatisticsData ::
  -- | 'bNumberOfTrues'
  Natural ->
  -- | 'bNumberOfFalses'
  Natural ->
  -- | 'bNumberOfNulls'
  Natural ->
  BooleanColumnStatisticsData
booleanColumnStatisticsData
  pNumberOfTrues_
  pNumberOfFalses_
  pNumberOfNulls_ =
    BooleanColumnStatisticsData'
      { _bNumberOfTrues =
          _Nat # pNumberOfTrues_,
        _bNumberOfFalses = _Nat # pNumberOfFalses_,
        _bNumberOfNulls = _Nat # pNumberOfNulls_
      }

-- | The number of true values in the column.
bNumberOfTrues :: Lens' BooleanColumnStatisticsData Natural
bNumberOfTrues = lens _bNumberOfTrues (\s a -> s {_bNumberOfTrues = a}) . _Nat

-- | The number of false values in the column.
bNumberOfFalses :: Lens' BooleanColumnStatisticsData Natural
bNumberOfFalses = lens _bNumberOfFalses (\s a -> s {_bNumberOfFalses = a}) . _Nat

-- | The number of null values in the column.
bNumberOfNulls :: Lens' BooleanColumnStatisticsData Natural
bNumberOfNulls = lens _bNumberOfNulls (\s a -> s {_bNumberOfNulls = a}) . _Nat

instance FromJSON BooleanColumnStatisticsData where
  parseJSON =
    withObject
      "BooleanColumnStatisticsData"
      ( \x ->
          BooleanColumnStatisticsData'
            <$> (x .: "NumberOfTrues")
            <*> (x .: "NumberOfFalses")
            <*> (x .: "NumberOfNulls")
      )

instance Hashable BooleanColumnStatisticsData

instance NFData BooleanColumnStatisticsData

instance ToJSON BooleanColumnStatisticsData where
  toJSON BooleanColumnStatisticsData' {..} =
    object
      ( catMaybes
          [ Just ("NumberOfTrues" .= _bNumberOfTrues),
            Just ("NumberOfFalses" .= _bNumberOfFalses),
            Just ("NumberOfNulls" .= _bNumberOfNulls)
          ]
      )

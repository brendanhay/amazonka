{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Statistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Statistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A map of key-value pairs for all supported statistics. Currently, only count is supported.
--
--
--
-- /See:/ 'statistics' smart constructor.
data Statistics = Statistics'
  { _sStdDeviation :: !(Maybe Double),
    _sMaximum :: !(Maybe Double),
    _sAverage :: !(Maybe Double),
    _sCount :: !(Maybe Int),
    _sMinimum :: !(Maybe Double),
    _sVariance :: !(Maybe Double),
    _sSumOfSquares :: !(Maybe Double),
    _sSum :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Statistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStdDeviation' - The standard deviation of the aggregated field values.
--
-- * 'sMaximum' - The maximum aggregated field value.
--
-- * 'sAverage' - The average of the aggregated field values.
--
-- * 'sCount' - The count of things that match the query.
--
-- * 'sMinimum' - The minimum aggregated field value.
--
-- * 'sVariance' - The variance of the aggregated field values.
--
-- * 'sSumOfSquares' - The sum of the squares of the aggregated field values.
--
-- * 'sSum' - The sum of the aggregated field values.
statistics ::
  Statistics
statistics =
  Statistics'
    { _sStdDeviation = Nothing,
      _sMaximum = Nothing,
      _sAverage = Nothing,
      _sCount = Nothing,
      _sMinimum = Nothing,
      _sVariance = Nothing,
      _sSumOfSquares = Nothing,
      _sSum = Nothing
    }

-- | The standard deviation of the aggregated field values.
sStdDeviation :: Lens' Statistics (Maybe Double)
sStdDeviation = lens _sStdDeviation (\s a -> s {_sStdDeviation = a})

-- | The maximum aggregated field value.
sMaximum :: Lens' Statistics (Maybe Double)
sMaximum = lens _sMaximum (\s a -> s {_sMaximum = a})

-- | The average of the aggregated field values.
sAverage :: Lens' Statistics (Maybe Double)
sAverage = lens _sAverage (\s a -> s {_sAverage = a})

-- | The count of things that match the query.
sCount :: Lens' Statistics (Maybe Int)
sCount = lens _sCount (\s a -> s {_sCount = a})

-- | The minimum aggregated field value.
sMinimum :: Lens' Statistics (Maybe Double)
sMinimum = lens _sMinimum (\s a -> s {_sMinimum = a})

-- | The variance of the aggregated field values.
sVariance :: Lens' Statistics (Maybe Double)
sVariance = lens _sVariance (\s a -> s {_sVariance = a})

-- | The sum of the squares of the aggregated field values.
sSumOfSquares :: Lens' Statistics (Maybe Double)
sSumOfSquares = lens _sSumOfSquares (\s a -> s {_sSumOfSquares = a})

-- | The sum of the aggregated field values.
sSum :: Lens' Statistics (Maybe Double)
sSum = lens _sSum (\s a -> s {_sSum = a})

instance FromJSON Statistics where
  parseJSON =
    withObject
      "Statistics"
      ( \x ->
          Statistics'
            <$> (x .:? "stdDeviation")
            <*> (x .:? "maximum")
            <*> (x .:? "average")
            <*> (x .:? "count")
            <*> (x .:? "minimum")
            <*> (x .:? "variance")
            <*> (x .:? "sumOfSquares")
            <*> (x .:? "sum")
      )

instance Hashable Statistics

instance NFData Statistics

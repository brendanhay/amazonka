{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedCounter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The counter that describes a DDoS attack.
--
--
--
-- /See:/ 'summarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { _scMax ::
      !(Maybe Double),
    _scAverage :: !(Maybe Double),
    _scN :: !(Maybe Int),
    _scName :: !(Maybe Text),
    _scSum :: !(Maybe Double),
    _scUnit :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SummarizedCounter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMax' - The maximum value of the counter for a specified time period.
--
-- * 'scAverage' - The average value of the counter for a specified time period.
--
-- * 'scN' - The number of counters for a specified time period.
--
-- * 'scName' - The counter name.
--
-- * 'scSum' - The total of counter values for a specified time period.
--
-- * 'scUnit' - The unit of the counters.
summarizedCounter ::
  SummarizedCounter
summarizedCounter =
  SummarizedCounter'
    { _scMax = Nothing,
      _scAverage = Nothing,
      _scN = Nothing,
      _scName = Nothing,
      _scSum = Nothing,
      _scUnit = Nothing
    }

-- | The maximum value of the counter for a specified time period.
scMax :: Lens' SummarizedCounter (Maybe Double)
scMax = lens _scMax (\s a -> s {_scMax = a})

-- | The average value of the counter for a specified time period.
scAverage :: Lens' SummarizedCounter (Maybe Double)
scAverage = lens _scAverage (\s a -> s {_scAverage = a})

-- | The number of counters for a specified time period.
scN :: Lens' SummarizedCounter (Maybe Int)
scN = lens _scN (\s a -> s {_scN = a})

-- | The counter name.
scName :: Lens' SummarizedCounter (Maybe Text)
scName = lens _scName (\s a -> s {_scName = a})

-- | The total of counter values for a specified time period.
scSum :: Lens' SummarizedCounter (Maybe Double)
scSum = lens _scSum (\s a -> s {_scSum = a})

-- | The unit of the counters.
scUnit :: Lens' SummarizedCounter (Maybe Text)
scUnit = lens _scUnit (\s a -> s {_scUnit = a})

instance FromJSON SummarizedCounter where
  parseJSON =
    withObject
      "SummarizedCounter"
      ( \x ->
          SummarizedCounter'
            <$> (x .:? "Max")
            <*> (x .:? "Average")
            <*> (x .:? "N")
            <*> (x .:? "Name")
            <*> (x .:? "Sum")
            <*> (x .:? "Unit")
      )

instance Hashable SummarizedCounter

instance NFData SummarizedCounter

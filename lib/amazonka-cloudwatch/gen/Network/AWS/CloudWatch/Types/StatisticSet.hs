{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StatisticSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StatisticSet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a set of statistics that describes a specific metric.
--
--
--
-- /See:/ 'statisticSet' smart constructor.
data StatisticSet = StatisticSet'
  { _ssSampleCount :: !Double,
    _ssSum :: !Double,
    _ssMinimum :: !Double,
    _ssMaximum :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatisticSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSampleCount' - The number of samples used for the statistic set.
--
-- * 'ssSum' - The sum of values for the sample set.
--
-- * 'ssMinimum' - The minimum value of the sample set.
--
-- * 'ssMaximum' - The maximum value of the sample set.
statisticSet ::
  -- | 'ssSampleCount'
  Double ->
  -- | 'ssSum'
  Double ->
  -- | 'ssMinimum'
  Double ->
  -- | 'ssMaximum'
  Double ->
  StatisticSet
statisticSet pSampleCount_ pSum_ pMinimum_ pMaximum_ =
  StatisticSet'
    { _ssSampleCount = pSampleCount_,
      _ssSum = pSum_,
      _ssMinimum = pMinimum_,
      _ssMaximum = pMaximum_
    }

-- | The number of samples used for the statistic set.
ssSampleCount :: Lens' StatisticSet Double
ssSampleCount = lens _ssSampleCount (\s a -> s {_ssSampleCount = a})

-- | The sum of values for the sample set.
ssSum :: Lens' StatisticSet Double
ssSum = lens _ssSum (\s a -> s {_ssSum = a})

-- | The minimum value of the sample set.
ssMinimum :: Lens' StatisticSet Double
ssMinimum = lens _ssMinimum (\s a -> s {_ssMinimum = a})

-- | The maximum value of the sample set.
ssMaximum :: Lens' StatisticSet Double
ssMaximum = lens _ssMaximum (\s a -> s {_ssMaximum = a})

instance Hashable StatisticSet

instance NFData StatisticSet

instance ToQuery StatisticSet where
  toQuery StatisticSet' {..} =
    mconcat
      [ "SampleCount" =: _ssSampleCount,
        "Sum" =: _ssSum,
        "Minimum" =: _ssMinimum,
        "Maximum" =: _ssMaximum
      ]

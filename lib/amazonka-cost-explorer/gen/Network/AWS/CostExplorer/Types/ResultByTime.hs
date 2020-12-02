{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResultByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResultByTime where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result that is associated with a time period.
--
--
--
-- /See:/ 'resultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { _rbtGroups :: !(Maybe [Group]),
    _rbtTimePeriod :: !(Maybe DateInterval),
    _rbtTotal :: !(Maybe (Map Text (MetricValue))),
    _rbtEstimated :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbtGroups' - The groups that this time period includes.
--
-- * 'rbtTimePeriod' - The time period that the result covers.
--
-- * 'rbtTotal' - The total amount of cost or usage accrued during the time period.
--
-- * 'rbtEstimated' - Whether the result is estimated.
resultByTime ::
  ResultByTime
resultByTime =
  ResultByTime'
    { _rbtGroups = Nothing,
      _rbtTimePeriod = Nothing,
      _rbtTotal = Nothing,
      _rbtEstimated = Nothing
    }

-- | The groups that this time period includes.
rbtGroups :: Lens' ResultByTime [Group]
rbtGroups = lens _rbtGroups (\s a -> s {_rbtGroups = a}) . _Default . _Coerce

-- | The time period that the result covers.
rbtTimePeriod :: Lens' ResultByTime (Maybe DateInterval)
rbtTimePeriod = lens _rbtTimePeriod (\s a -> s {_rbtTimePeriod = a})

-- | The total amount of cost or usage accrued during the time period.
rbtTotal :: Lens' ResultByTime (HashMap Text (MetricValue))
rbtTotal = lens _rbtTotal (\s a -> s {_rbtTotal = a}) . _Default . _Map

-- | Whether the result is estimated.
rbtEstimated :: Lens' ResultByTime (Maybe Bool)
rbtEstimated = lens _rbtEstimated (\s a -> s {_rbtEstimated = a})

instance FromJSON ResultByTime where
  parseJSON =
    withObject
      "ResultByTime"
      ( \x ->
          ResultByTime'
            <$> (x .:? "Groups" .!= mempty)
            <*> (x .:? "TimePeriod")
            <*> (x .:? "Total" .!= mempty)
            <*> (x .:? "Estimated")
      )

instance Hashable ResultByTime

instance NFData ResultByTime

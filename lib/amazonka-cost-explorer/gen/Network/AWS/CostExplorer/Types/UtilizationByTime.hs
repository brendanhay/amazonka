{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.UtilizationByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.UtilizationByTime where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of utilization, in hours.
--
--
--
-- /See:/ 'utilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { _ubtGroups ::
      !(Maybe [ReservationUtilizationGroup]),
    _ubtTimePeriod :: !(Maybe DateInterval),
    _ubtTotal :: !(Maybe ReservationAggregates)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UtilizationByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubtGroups' - The groups that this utilization result uses.
--
-- * 'ubtTimePeriod' - The period of time that this utilization was used for.
--
-- * 'ubtTotal' - The total number of reservation hours that were used.
utilizationByTime ::
  UtilizationByTime
utilizationByTime =
  UtilizationByTime'
    { _ubtGroups = Nothing,
      _ubtTimePeriod = Nothing,
      _ubtTotal = Nothing
    }

-- | The groups that this utilization result uses.
ubtGroups :: Lens' UtilizationByTime [ReservationUtilizationGroup]
ubtGroups = lens _ubtGroups (\s a -> s {_ubtGroups = a}) . _Default . _Coerce

-- | The period of time that this utilization was used for.
ubtTimePeriod :: Lens' UtilizationByTime (Maybe DateInterval)
ubtTimePeriod = lens _ubtTimePeriod (\s a -> s {_ubtTimePeriod = a})

-- | The total number of reservation hours that were used.
ubtTotal :: Lens' UtilizationByTime (Maybe ReservationAggregates)
ubtTotal = lens _ubtTotal (\s a -> s {_ubtTotal = a})

instance FromJSON UtilizationByTime where
  parseJSON =
    withObject
      "UtilizationByTime"
      ( \x ->
          UtilizationByTime'
            <$> (x .:? "Groups" .!= mempty)
            <*> (x .:? "TimePeriod")
            <*> (x .:? "Total")
      )

instance Hashable UtilizationByTime

instance NFData UtilizationByTime

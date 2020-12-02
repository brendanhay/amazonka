{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageByTime where

import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Reservation coverage for a specified period, in hours.
--
--
--
-- /See:/ 'coverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { _cbtGroups ::
      !(Maybe [ReservationCoverageGroup]),
    _cbtTimePeriod :: !(Maybe DateInterval),
    _cbtTotal :: !(Maybe Coverage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoverageByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtGroups' - The groups of instances that the reservation covered.
--
-- * 'cbtTimePeriod' - The period that this coverage was used over.
--
-- * 'cbtTotal' - The total reservation coverage, in hours.
coverageByTime ::
  CoverageByTime
coverageByTime =
  CoverageByTime'
    { _cbtGroups = Nothing,
      _cbtTimePeriod = Nothing,
      _cbtTotal = Nothing
    }

-- | The groups of instances that the reservation covered.
cbtGroups :: Lens' CoverageByTime [ReservationCoverageGroup]
cbtGroups = lens _cbtGroups (\s a -> s {_cbtGroups = a}) . _Default . _Coerce

-- | The period that this coverage was used over.
cbtTimePeriod :: Lens' CoverageByTime (Maybe DateInterval)
cbtTimePeriod = lens _cbtTimePeriod (\s a -> s {_cbtTimePeriod = a})

-- | The total reservation coverage, in hours.
cbtTotal :: Lens' CoverageByTime (Maybe Coverage)
cbtTotal = lens _cbtTotal (\s a -> s {_cbtTotal = a})

instance FromJSON CoverageByTime where
  parseJSON =
    withObject
      "CoverageByTime"
      ( \x ->
          CoverageByTime'
            <$> (x .:? "Groups" .!= mempty)
            <*> (x .:? "TimePeriod")
            <*> (x .:? "Total")
      )

instance Hashable CoverageByTime

instance NFData CoverageByTime

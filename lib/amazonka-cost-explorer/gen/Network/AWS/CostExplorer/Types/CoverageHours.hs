{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageHours
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageHours where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | How long a running instance either used a reservation or was On-Demand.
--
--
--
-- /See:/ 'coverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { _chCoverageHoursPercentage ::
      !(Maybe Text),
    _chOnDemandHours :: !(Maybe Text),
    _chTotalRunningHours :: !(Maybe Text),
    _chReservedHours :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoverageHours' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chCoverageHoursPercentage' - The percentage of instance hours that a reservation covered.
--
-- * 'chOnDemandHours' - The number of instance running hours that On-Demand Instances covered.
--
-- * 'chTotalRunningHours' - The total instance usage, in hours.
--
-- * 'chReservedHours' - The number of instance running hours that reservations covered.
coverageHours ::
  CoverageHours
coverageHours =
  CoverageHours'
    { _chCoverageHoursPercentage = Nothing,
      _chOnDemandHours = Nothing,
      _chTotalRunningHours = Nothing,
      _chReservedHours = Nothing
    }

-- | The percentage of instance hours that a reservation covered.
chCoverageHoursPercentage :: Lens' CoverageHours (Maybe Text)
chCoverageHoursPercentage = lens _chCoverageHoursPercentage (\s a -> s {_chCoverageHoursPercentage = a})

-- | The number of instance running hours that On-Demand Instances covered.
chOnDemandHours :: Lens' CoverageHours (Maybe Text)
chOnDemandHours = lens _chOnDemandHours (\s a -> s {_chOnDemandHours = a})

-- | The total instance usage, in hours.
chTotalRunningHours :: Lens' CoverageHours (Maybe Text)
chTotalRunningHours = lens _chTotalRunningHours (\s a -> s {_chTotalRunningHours = a})

-- | The number of instance running hours that reservations covered.
chReservedHours :: Lens' CoverageHours (Maybe Text)
chReservedHours = lens _chReservedHours (\s a -> s {_chReservedHours = a})

instance FromJSON CoverageHours where
  parseJSON =
    withObject
      "CoverageHours"
      ( \x ->
          CoverageHours'
            <$> (x .:? "CoverageHoursPercentage")
            <*> (x .:? "OnDemandHours")
            <*> (x .:? "TotalRunningHours")
            <*> (x .:? "ReservedHours")
      )

instance Hashable CoverageHours

instance NFData CoverageHours

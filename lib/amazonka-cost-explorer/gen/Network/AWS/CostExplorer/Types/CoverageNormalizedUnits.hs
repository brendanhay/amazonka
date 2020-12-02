{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageNormalizedUnits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of instance usage, in normalized units. Normalized units enable you to see your EC2 usage for multiple sizes of instances in a uniform way. For example, suppose you run an xlarge instance and a 2xlarge instance. If you run both instances for the same amount of time, the 2xlarge instance uses twice as much of your reservation as the xlarge instance, even though both instances show only one instance-hour. Using normalized units instead of instance-hours, the xlarge instance used 8 normalized units, and the 2xlarge instance used 16 normalized units.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ .
--
--
-- /See:/ 'coverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { _cnuReservedNormalizedUnits ::
      !(Maybe Text),
    _cnuTotalRunningNormalizedUnits ::
      !(Maybe Text),
    _cnuCoverageNormalizedUnitsPercentage ::
      !(Maybe Text),
    _cnuOnDemandNormalizedUnits ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoverageNormalizedUnits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnuReservedNormalizedUnits' - The number of normalized units that a reservation covers.
--
-- * 'cnuTotalRunningNormalizedUnits' - The total number of normalized units that you used.
--
-- * 'cnuCoverageNormalizedUnitsPercentage' - The percentage of your used instance normalized units that a reservation covers.
--
-- * 'cnuOnDemandNormalizedUnits' - The number of normalized units that are covered by On-Demand Instances instead of a reservation.
coverageNormalizedUnits ::
  CoverageNormalizedUnits
coverageNormalizedUnits =
  CoverageNormalizedUnits'
    { _cnuReservedNormalizedUnits = Nothing,
      _cnuTotalRunningNormalizedUnits = Nothing,
      _cnuCoverageNormalizedUnitsPercentage = Nothing,
      _cnuOnDemandNormalizedUnits = Nothing
    }

-- | The number of normalized units that a reservation covers.
cnuReservedNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuReservedNormalizedUnits = lens _cnuReservedNormalizedUnits (\s a -> s {_cnuReservedNormalizedUnits = a})

-- | The total number of normalized units that you used.
cnuTotalRunningNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuTotalRunningNormalizedUnits = lens _cnuTotalRunningNormalizedUnits (\s a -> s {_cnuTotalRunningNormalizedUnits = a})

-- | The percentage of your used instance normalized units that a reservation covers.
cnuCoverageNormalizedUnitsPercentage :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuCoverageNormalizedUnitsPercentage = lens _cnuCoverageNormalizedUnitsPercentage (\s a -> s {_cnuCoverageNormalizedUnitsPercentage = a})

-- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
cnuOnDemandNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuOnDemandNormalizedUnits = lens _cnuOnDemandNormalizedUnits (\s a -> s {_cnuOnDemandNormalizedUnits = a})

instance FromJSON CoverageNormalizedUnits where
  parseJSON =
    withObject
      "CoverageNormalizedUnits"
      ( \x ->
          CoverageNormalizedUnits'
            <$> (x .:? "ReservedNormalizedUnits")
            <*> (x .:? "TotalRunningNormalizedUnits")
            <*> (x .:? "CoverageNormalizedUnitsPercentage")
            <*> (x .:? "OnDemandNormalizedUnits")
      )

instance Hashable CoverageNormalizedUnits

instance NFData CoverageNormalizedUnits

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetCapacitySpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DefaultTargetCapacityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot Instance @MaxTotalPrice@ , or both to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, EC2 Fleet will launch instances until it reaches the maximum amount that you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity. The @MaxTotalPrice@ parameters are located in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptions.html OnDemandOptions> and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptions SpotOptions>
--
--
-- /See:/ 'targetCapacitySpecification' smart constructor.
data TargetCapacitySpecification = TargetCapacitySpecification'
  { _tcsOnDemandTargetCapacity ::
      !(Maybe Int),
    _tcsDefaultTargetCapacityType ::
      !(Maybe DefaultTargetCapacityType),
    _tcsTotalTargetCapacity ::
      !(Maybe Int),
    _tcsSpotTargetCapacity ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetCapacitySpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsOnDemandTargetCapacity' - The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
--
-- * 'tcsDefaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- * 'tcsTotalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
--
-- * 'tcsSpotTargetCapacity' - The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
targetCapacitySpecification ::
  TargetCapacitySpecification
targetCapacitySpecification =
  TargetCapacitySpecification'
    { _tcsOnDemandTargetCapacity =
        Nothing,
      _tcsDefaultTargetCapacityType = Nothing,
      _tcsTotalTargetCapacity = Nothing,
      _tcsSpotTargetCapacity = Nothing
    }

-- | The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
tcsOnDemandTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsOnDemandTargetCapacity = lens _tcsOnDemandTargetCapacity (\s a -> s {_tcsOnDemandTargetCapacity = a})

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
tcsDefaultTargetCapacityType :: Lens' TargetCapacitySpecification (Maybe DefaultTargetCapacityType)
tcsDefaultTargetCapacityType = lens _tcsDefaultTargetCapacityType (\s a -> s {_tcsDefaultTargetCapacityType = a})

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
tcsTotalTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsTotalTargetCapacity = lens _tcsTotalTargetCapacity (\s a -> s {_tcsTotalTargetCapacity = a})

-- | The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
tcsSpotTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsSpotTargetCapacity = lens _tcsSpotTargetCapacity (\s a -> s {_tcsSpotTargetCapacity = a})

instance FromXML TargetCapacitySpecification where
  parseXML x =
    TargetCapacitySpecification'
      <$> (x .@? "onDemandTargetCapacity")
      <*> (x .@? "defaultTargetCapacityType")
      <*> (x .@? "totalTargetCapacity")
      <*> (x .@? "spotTargetCapacity")

instance Hashable TargetCapacitySpecification

instance NFData TargetCapacitySpecification

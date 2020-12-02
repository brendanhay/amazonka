{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetCapacitySpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DefaultTargetCapacityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of units to request. You can choose to set the target capacity as the number of instances. Or you can set the target capacity to a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot Instance @MaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, EC2 Fleet will launch instances until it reaches the maximum amount that you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity. The @MaxTotalPrice@ parameters are located in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptionsRequest OnDemandOptionsRequest> and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptionsRequest SpotOptionsRequest> .
--
--
-- /See:/ 'targetCapacitySpecificationRequest' smart constructor.
data TargetCapacitySpecificationRequest = TargetCapacitySpecificationRequest'
  { _tcsrOnDemandTargetCapacity ::
      !(Maybe Int),
    _tcsrDefaultTargetCapacityType ::
      !( Maybe
           DefaultTargetCapacityType
       ),
    _tcsrSpotTargetCapacity ::
      !(Maybe Int),
    _tcsrTotalTargetCapacity ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetCapacitySpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsrOnDemandTargetCapacity' - The number of On-Demand units to request.
--
-- * 'tcsrDefaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- * 'tcsrSpotTargetCapacity' - The number of Spot units to request.
--
-- * 'tcsrTotalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
targetCapacitySpecificationRequest ::
  -- | 'tcsrTotalTargetCapacity'
  Int ->
  TargetCapacitySpecificationRequest
targetCapacitySpecificationRequest pTotalTargetCapacity_ =
  TargetCapacitySpecificationRequest'
    { _tcsrOnDemandTargetCapacity =
        Nothing,
      _tcsrDefaultTargetCapacityType = Nothing,
      _tcsrSpotTargetCapacity = Nothing,
      _tcsrTotalTargetCapacity = pTotalTargetCapacity_
    }

-- | The number of On-Demand units to request.
tcsrOnDemandTargetCapacity :: Lens' TargetCapacitySpecificationRequest (Maybe Int)
tcsrOnDemandTargetCapacity = lens _tcsrOnDemandTargetCapacity (\s a -> s {_tcsrOnDemandTargetCapacity = a})

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
tcsrDefaultTargetCapacityType :: Lens' TargetCapacitySpecificationRequest (Maybe DefaultTargetCapacityType)
tcsrDefaultTargetCapacityType = lens _tcsrDefaultTargetCapacityType (\s a -> s {_tcsrDefaultTargetCapacityType = a})

-- | The number of Spot units to request.
tcsrSpotTargetCapacity :: Lens' TargetCapacitySpecificationRequest (Maybe Int)
tcsrSpotTargetCapacity = lens _tcsrSpotTargetCapacity (\s a -> s {_tcsrSpotTargetCapacity = a})

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
tcsrTotalTargetCapacity :: Lens' TargetCapacitySpecificationRequest Int
tcsrTotalTargetCapacity = lens _tcsrTotalTargetCapacity (\s a -> s {_tcsrTotalTargetCapacity = a})

instance Hashable TargetCapacitySpecificationRequest

instance NFData TargetCapacitySpecificationRequest

instance ToQuery TargetCapacitySpecificationRequest where
  toQuery TargetCapacitySpecificationRequest' {..} =
    mconcat
      [ "OnDemandTargetCapacity" =: _tcsrOnDemandTargetCapacity,
        "DefaultTargetCapacityType" =: _tcsrDefaultTargetCapacityType,
        "SpotTargetCapacity" =: _tcsrSpotTargetCapacity,
        "TotalTargetCapacity" =: _tcsrTotalTargetCapacity
      ]

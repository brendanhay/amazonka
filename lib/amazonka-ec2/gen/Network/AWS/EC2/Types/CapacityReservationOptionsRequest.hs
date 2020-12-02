{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the strategy for using unused Capacity Reservations for fulfilling On-Demand capacity.
--
--
-- For more information about Capacity Reservations, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html On-Demand Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ . For examples of using Capacity Reservations in an EC2 Fleet, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-examples.html EC2 Fleet example configurations> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- /See:/ 'capacityReservationOptionsRequest' smart constructor.
newtype CapacityReservationOptionsRequest = CapacityReservationOptionsRequest'
  { _crorUsageStrategy ::
      Maybe
        FleetCapacityReservationUsageStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crorUsageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity. If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ). If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
capacityReservationOptionsRequest ::
  CapacityReservationOptionsRequest
capacityReservationOptionsRequest =
  CapacityReservationOptionsRequest' {_crorUsageStrategy = Nothing}

-- | Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity. If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ). If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
crorUsageStrategy :: Lens' CapacityReservationOptionsRequest (Maybe FleetCapacityReservationUsageStrategy)
crorUsageStrategy = lens _crorUsageStrategy (\s a -> s {_crorUsageStrategy = a})

instance Hashable CapacityReservationOptionsRequest

instance NFData CapacityReservationOptionsRequest

instance ToQuery CapacityReservationOptionsRequest where
  toQuery CapacityReservationOptionsRequest' {..} =
    mconcat ["UsageStrategy" =: _crorUsageStrategy]

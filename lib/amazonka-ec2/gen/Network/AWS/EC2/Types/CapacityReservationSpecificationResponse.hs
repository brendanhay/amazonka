{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecificationResponse where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the instance's Capacity Reservation targeting preferences. The action returns the @capacityReservationPreference@ response element if the instance is configured to run in On-Demand capacity, or if it is configured in run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). The action returns the @capacityReservationTarget@ response element if the instance explicily targets a specific Capacity Reservation or Capacity Reservation group.
--
--
--
-- /See:/ 'capacityReservationSpecificationResponse' smart constructor.
data CapacityReservationSpecificationResponse = CapacityReservationSpecificationResponse'
  { _crsCapacityReservationTarget ::
      !( Maybe
           CapacityReservationTargetResponse
       ),
    _crsCapacityReservationPreference ::
      !( Maybe
           CapacityReservationPreference
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsCapacityReservationTarget' - Information about the targeted Capacity Reservation or Capacity Reservation group.
--
-- * 'crsCapacityReservationPreference' - Describes the instance's Capacity Reservation preferences. Possible preferences include:     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
capacityReservationSpecificationResponse ::
  CapacityReservationSpecificationResponse
capacityReservationSpecificationResponse =
  CapacityReservationSpecificationResponse'
    { _crsCapacityReservationTarget =
        Nothing,
      _crsCapacityReservationPreference = Nothing
    }

-- | Information about the targeted Capacity Reservation or Capacity Reservation group.
crsCapacityReservationTarget :: Lens' CapacityReservationSpecificationResponse (Maybe CapacityReservationTargetResponse)
crsCapacityReservationTarget = lens _crsCapacityReservationTarget (\s a -> s {_crsCapacityReservationTarget = a})

-- | Describes the instance's Capacity Reservation preferences. Possible preferences include:     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
crsCapacityReservationPreference :: Lens' CapacityReservationSpecificationResponse (Maybe CapacityReservationPreference)
crsCapacityReservationPreference = lens _crsCapacityReservationPreference (\s a -> s {_crsCapacityReservationPreference = a})

instance FromXML CapacityReservationSpecificationResponse where
  parseXML x =
    CapacityReservationSpecificationResponse'
      <$> (x .@? "capacityReservationTarget")
      <*> (x .@? "capacityReservationPreference")

instance Hashable CapacityReservationSpecificationResponse

instance NFData CapacityReservationSpecificationResponse

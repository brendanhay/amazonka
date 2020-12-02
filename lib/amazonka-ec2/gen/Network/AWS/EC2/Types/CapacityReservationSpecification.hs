{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance's Capacity Reservation targeting option. You can specify only one parameter at a time. If you specify @CapacityReservationPreference@ and @CapacityReservationTarget@ , the request fails.
--
--
-- Use the @CapacityReservationPreference@ parameter to configure the instance to run as an On-Demand Instance or to run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). Use the @CapacityReservationTarget@ parameter to explicitly target a specific Capacity Reservation or a Capacity Reservation group.
--
--
-- /See:/ 'capacityReservationSpecification' smart constructor.
data CapacityReservationSpecification = CapacityReservationSpecification'
  { _cCapacityReservationTarget ::
      !( Maybe
           CapacityReservationTarget
       ),
    _cCapacityReservationPreference ::
      !( Maybe
           CapacityReservationPreference
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCapacityReservationTarget' - Information about the target Capacity Reservation or Capacity Reservation group.
--
-- * 'cCapacityReservationPreference' - Indicates the instance's Capacity Reservation preferences. Possible preferences include:     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs as an On-Demand Instance.
capacityReservationSpecification ::
  CapacityReservationSpecification
capacityReservationSpecification =
  CapacityReservationSpecification'
    { _cCapacityReservationTarget =
        Nothing,
      _cCapacityReservationPreference = Nothing
    }

-- | Information about the target Capacity Reservation or Capacity Reservation group.
cCapacityReservationTarget :: Lens' CapacityReservationSpecification (Maybe CapacityReservationTarget)
cCapacityReservationTarget = lens _cCapacityReservationTarget (\s a -> s {_cCapacityReservationTarget = a})

-- | Indicates the instance's Capacity Reservation preferences. Possible preferences include:     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs as an On-Demand Instance.
cCapacityReservationPreference :: Lens' CapacityReservationSpecification (Maybe CapacityReservationPreference)
cCapacityReservationPreference = lens _cCapacityReservationPreference (\s a -> s {_cCapacityReservationPreference = a})

instance Hashable CapacityReservationSpecification

instance NFData CapacityReservationSpecification

instance ToQuery CapacityReservationSpecification where
  toQuery CapacityReservationSpecification' {..} =
    mconcat
      [ "CapacityReservationTarget" =: _cCapacityReservationTarget,
        "CapacityReservationPreference" =: _cCapacityReservationPreference
      ]

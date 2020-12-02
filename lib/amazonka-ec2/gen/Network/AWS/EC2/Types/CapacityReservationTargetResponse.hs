{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTargetResponse where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
--
--
-- /See:/ 'capacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
  { _cCapacityReservationId ::
      !(Maybe Text),
    _cCapacityReservationResourceGroupARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCapacityReservationId' - The ID of the targeted Capacity Reservation.
--
-- * 'cCapacityReservationResourceGroupARN' - The ARN of the targeted Capacity Reservation group.
capacityReservationTargetResponse ::
  CapacityReservationTargetResponse
capacityReservationTargetResponse =
  CapacityReservationTargetResponse'
    { _cCapacityReservationId =
        Nothing,
      _cCapacityReservationResourceGroupARN = Nothing
    }

-- | The ID of the targeted Capacity Reservation.
cCapacityReservationId :: Lens' CapacityReservationTargetResponse (Maybe Text)
cCapacityReservationId = lens _cCapacityReservationId (\s a -> s {_cCapacityReservationId = a})

-- | The ARN of the targeted Capacity Reservation group.
cCapacityReservationResourceGroupARN :: Lens' CapacityReservationTargetResponse (Maybe Text)
cCapacityReservationResourceGroupARN = lens _cCapacityReservationResourceGroupARN (\s a -> s {_cCapacityReservationResourceGroupARN = a})

instance FromXML CapacityReservationTargetResponse where
  parseXML x =
    CapacityReservationTargetResponse'
      <$> (x .@? "capacityReservationId")
      <*> (x .@? "capacityReservationResourceGroupArn")

instance Hashable CapacityReservationTargetResponse

instance NFData CapacityReservationTargetResponse

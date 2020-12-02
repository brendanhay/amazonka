{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceReservationValue where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The total value of the Convertible Reserved Instance.
--
--
--
-- /See:/ 'reservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { _rirvReservationValue ::
      !(Maybe ReservationValue),
    _rirvReservedInstanceId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstanceReservationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirvReservationValue' - The total value of the Convertible Reserved Instance that you are exchanging.
--
-- * 'rirvReservedInstanceId' - The ID of the Convertible Reserved Instance that you are exchanging.
reservedInstanceReservationValue ::
  ReservedInstanceReservationValue
reservedInstanceReservationValue =
  ReservedInstanceReservationValue'
    { _rirvReservationValue =
        Nothing,
      _rirvReservedInstanceId = Nothing
    }

-- | The total value of the Convertible Reserved Instance that you are exchanging.
rirvReservationValue :: Lens' ReservedInstanceReservationValue (Maybe ReservationValue)
rirvReservationValue = lens _rirvReservationValue (\s a -> s {_rirvReservationValue = a})

-- | The ID of the Convertible Reserved Instance that you are exchanging.
rirvReservedInstanceId :: Lens' ReservedInstanceReservationValue (Maybe Text)
rirvReservedInstanceId = lens _rirvReservedInstanceId (\s a -> s {_rirvReservedInstanceId = a})

instance FromXML ReservedInstanceReservationValue where
  parseXML x =
    ReservedInstanceReservationValue'
      <$> (x .@? "reservationValue") <*> (x .@? "reservedInstanceId")

instance Hashable ReservedInstanceReservationValue

instance NFData ReservedInstanceReservationValue

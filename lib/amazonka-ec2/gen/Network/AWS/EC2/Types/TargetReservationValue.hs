{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetReservationValue where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.EC2.Types.TargetConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The total value of the new Convertible Reserved Instances.
--
--
--
-- /See:/ 'targetReservationValue' smart constructor.
data TargetReservationValue = TargetReservationValue'
  { _trvReservationValue ::
      !(Maybe ReservationValue),
    _trvTargetConfiguration ::
      !(Maybe TargetConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetReservationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trvReservationValue' - The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
--
-- * 'trvTargetConfiguration' - The configuration of the Convertible Reserved Instances that make up the exchange.
targetReservationValue ::
  TargetReservationValue
targetReservationValue =
  TargetReservationValue'
    { _trvReservationValue = Nothing,
      _trvTargetConfiguration = Nothing
    }

-- | The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
trvReservationValue :: Lens' TargetReservationValue (Maybe ReservationValue)
trvReservationValue = lens _trvReservationValue (\s a -> s {_trvReservationValue = a})

-- | The configuration of the Convertible Reserved Instances that make up the exchange.
trvTargetConfiguration :: Lens' TargetReservationValue (Maybe TargetConfiguration)
trvTargetConfiguration = lens _trvTargetConfiguration (\s a -> s {_trvTargetConfiguration = a})

instance FromXML TargetReservationValue where
  parseXML x =
    TargetReservationValue'
      <$> (x .@? "reservationValue") <*> (x .@? "targetConfiguration")

instance Hashable TargetReservationValue

instance NFData TargetReservationValue

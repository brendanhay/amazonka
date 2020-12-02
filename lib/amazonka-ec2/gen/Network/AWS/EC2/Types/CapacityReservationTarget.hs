{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTarget where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
--
--
-- /See:/ 'capacityReservationTarget' smart constructor.
data CapacityReservationTarget = CapacityReservationTarget'
  { _crtCapacityReservationId ::
      !(Maybe Text),
    _crtCapacityReservationResourceGroupARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtCapacityReservationId' - The ID of the Capacity Reservation in which to run the instance.
--
-- * 'crtCapacityReservationResourceGroupARN' - The ARN of the Capacity Reservation resource group in which to run the instance.
capacityReservationTarget ::
  CapacityReservationTarget
capacityReservationTarget =
  CapacityReservationTarget'
    { _crtCapacityReservationId = Nothing,
      _crtCapacityReservationResourceGroupARN = Nothing
    }

-- | The ID of the Capacity Reservation in which to run the instance.
crtCapacityReservationId :: Lens' CapacityReservationTarget (Maybe Text)
crtCapacityReservationId = lens _crtCapacityReservationId (\s a -> s {_crtCapacityReservationId = a})

-- | The ARN of the Capacity Reservation resource group in which to run the instance.
crtCapacityReservationResourceGroupARN :: Lens' CapacityReservationTarget (Maybe Text)
crtCapacityReservationResourceGroupARN = lens _crtCapacityReservationResourceGroupARN (\s a -> s {_crtCapacityReservationResourceGroupARN = a})

instance Hashable CapacityReservationTarget

instance NFData CapacityReservationTarget

instance ToQuery CapacityReservationTarget where
  toQuery CapacityReservationTarget' {..} =
    mconcat
      [ "CapacityReservationId" =: _crtCapacityReservationId,
        "CapacityReservationResourceGroupArn"
          =: _crtCapacityReservationResourceGroupARN
      ]

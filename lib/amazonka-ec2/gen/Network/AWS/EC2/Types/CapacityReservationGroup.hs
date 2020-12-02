{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationGroup where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a resource group to which a Capacity Reservation has been added.
--
--
--
-- /See:/ 'capacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { _crgOwnerId ::
      !(Maybe Text),
    _crgGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgOwnerId' - The ID of the AWS account that owns the resource group.
--
-- * 'crgGroupARN' - The ARN of the resource group.
capacityReservationGroup ::
  CapacityReservationGroup
capacityReservationGroup =
  CapacityReservationGroup'
    { _crgOwnerId = Nothing,
      _crgGroupARN = Nothing
    }

-- | The ID of the AWS account that owns the resource group.
crgOwnerId :: Lens' CapacityReservationGroup (Maybe Text)
crgOwnerId = lens _crgOwnerId (\s a -> s {_crgOwnerId = a})

-- | The ARN of the resource group.
crgGroupARN :: Lens' CapacityReservationGroup (Maybe Text)
crgGroupARN = lens _crgGroupARN (\s a -> s {_crgGroupARN = a})

instance FromXML CapacityReservationGroup where
  parseXML x =
    CapacityReservationGroup'
      <$> (x .@? "ownerId") <*> (x .@? "groupArn")

instance Hashable CapacityReservationGroup

instance NFData CapacityReservationGroup

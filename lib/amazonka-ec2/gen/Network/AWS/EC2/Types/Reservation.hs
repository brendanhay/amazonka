{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Reservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Reservation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Instance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch request for one or more instances, and includes owner, requester, and security group information that applies to all instances in the launch request.
--
--
--
-- /See:/ 'reservation' smart constructor.
data Reservation = Reservation'
  { _rGroups ::
      !(Maybe [GroupIdentifier]),
    _rInstances :: !(Maybe [Instance]),
    _rRequesterId :: !(Maybe Text),
    _rReservationId :: !Text,
    _rOwnerId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rGroups' - [EC2-Classic only] The security groups.
--
-- * 'rInstances' - The instances.
--
-- * 'rRequesterId' - The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- * 'rReservationId' - The ID of the reservation.
--
-- * 'rOwnerId' - The ID of the AWS account that owns the reservation.
reservation ::
  -- | 'rReservationId'
  Text ->
  -- | 'rOwnerId'
  Text ->
  Reservation
reservation pReservationId_ pOwnerId_ =
  Reservation'
    { _rGroups = Nothing,
      _rInstances = Nothing,
      _rRequesterId = Nothing,
      _rReservationId = pReservationId_,
      _rOwnerId = pOwnerId_
    }

-- | [EC2-Classic only] The security groups.
rGroups :: Lens' Reservation [GroupIdentifier]
rGroups = lens _rGroups (\s a -> s {_rGroups = a}) . _Default . _Coerce

-- | The instances.
rInstances :: Lens' Reservation [Instance]
rInstances = lens _rInstances (\s a -> s {_rInstances = a}) . _Default . _Coerce

-- | The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
rRequesterId :: Lens' Reservation (Maybe Text)
rRequesterId = lens _rRequesterId (\s a -> s {_rRequesterId = a})

-- | The ID of the reservation.
rReservationId :: Lens' Reservation Text
rReservationId = lens _rReservationId (\s a -> s {_rReservationId = a})

-- | The ID of the AWS account that owns the reservation.
rOwnerId :: Lens' Reservation Text
rOwnerId = lens _rOwnerId (\s a -> s {_rOwnerId = a})

instance FromXML Reservation where
  parseXML x =
    Reservation'
      <$> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "instancesSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "requesterId")
      <*> (x .@ "reservationId")
      <*> (x .@ "ownerId")

instance Hashable Reservation

instance NFData Reservation

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Host
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Host where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostRecovery
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of the Dedicated Host.
--
--
--
-- /See:/ 'host' smart constructor.
data Host = Host'
  { _hReleaseTime :: !(Maybe ISO8601),
    _hState :: !(Maybe AllocationState),
    _hClientToken :: !(Maybe Text),
    _hAvailabilityZoneId :: !(Maybe Text),
    _hHostId :: !(Maybe Text),
    _hAvailableCapacity :: !(Maybe AvailableCapacity),
    _hHostReservationId :: !(Maybe Text),
    _hAllowsMultipleInstanceTypes ::
      !(Maybe AllowsMultipleInstanceTypes),
    _hHostProperties :: !(Maybe HostProperties),
    _hOwnerId :: !(Maybe Text),
    _hAvailabilityZone :: !(Maybe Text),
    _hInstances :: !(Maybe [HostInstance]),
    _hAllocationTime :: !(Maybe ISO8601),
    _hMemberOfServiceLinkedResourceGroup :: !(Maybe Bool),
    _hHostRecovery :: !(Maybe HostRecovery),
    _hAutoPlacement :: !(Maybe AutoPlacement),
    _hTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Host' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hReleaseTime' - The time that the Dedicated Host was released.
--
-- * 'hState' - The Dedicated Host's state.
--
-- * 'hClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'hAvailabilityZoneId' - The ID of the Availability Zone in which the Dedicated Host is allocated.
--
-- * 'hHostId' - The ID of the Dedicated Host.
--
-- * 'hAvailableCapacity' - Information about the instances running on the Dedicated Host.
--
-- * 'hHostReservationId' - The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
--
-- * 'hAllowsMultipleInstanceTypes' - Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
--
-- * 'hHostProperties' - The hardware specifications of the Dedicated Host.
--
-- * 'hOwnerId' - The ID of the AWS account that owns the Dedicated Host.
--
-- * 'hAvailabilityZone' - The Availability Zone of the Dedicated Host.
--
-- * 'hInstances' - The IDs and instance type that are currently running on the Dedicated Host.
--
-- * 'hAllocationTime' - The time that the Dedicated Host was allocated.
--
-- * 'hMemberOfServiceLinkedResourceGroup' - Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
--
-- * 'hHostRecovery' - Indicates whether host recovery is enabled or disabled for the Dedicated Host.
--
-- * 'hAutoPlacement' - Whether auto-placement is on or off.
--
-- * 'hTags' - Any tags assigned to the Dedicated Host.
host ::
  Host
host =
  Host'
    { _hReleaseTime = Nothing,
      _hState = Nothing,
      _hClientToken = Nothing,
      _hAvailabilityZoneId = Nothing,
      _hHostId = Nothing,
      _hAvailableCapacity = Nothing,
      _hHostReservationId = Nothing,
      _hAllowsMultipleInstanceTypes = Nothing,
      _hHostProperties = Nothing,
      _hOwnerId = Nothing,
      _hAvailabilityZone = Nothing,
      _hInstances = Nothing,
      _hAllocationTime = Nothing,
      _hMemberOfServiceLinkedResourceGroup = Nothing,
      _hHostRecovery = Nothing,
      _hAutoPlacement = Nothing,
      _hTags = Nothing
    }

-- | The time that the Dedicated Host was released.
hReleaseTime :: Lens' Host (Maybe UTCTime)
hReleaseTime = lens _hReleaseTime (\s a -> s {_hReleaseTime = a}) . mapping _Time

-- | The Dedicated Host's state.
hState :: Lens' Host (Maybe AllocationState)
hState = lens _hState (\s a -> s {_hState = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
hClientToken :: Lens' Host (Maybe Text)
hClientToken = lens _hClientToken (\s a -> s {_hClientToken = a})

-- | The ID of the Availability Zone in which the Dedicated Host is allocated.
hAvailabilityZoneId :: Lens' Host (Maybe Text)
hAvailabilityZoneId = lens _hAvailabilityZoneId (\s a -> s {_hAvailabilityZoneId = a})

-- | The ID of the Dedicated Host.
hHostId :: Lens' Host (Maybe Text)
hHostId = lens _hHostId (\s a -> s {_hHostId = a})

-- | Information about the instances running on the Dedicated Host.
hAvailableCapacity :: Lens' Host (Maybe AvailableCapacity)
hAvailableCapacity = lens _hAvailableCapacity (\s a -> s {_hAvailableCapacity = a})

-- | The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
hHostReservationId :: Lens' Host (Maybe Text)
hHostReservationId = lens _hHostReservationId (\s a -> s {_hHostReservationId = a})

-- | Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
hAllowsMultipleInstanceTypes :: Lens' Host (Maybe AllowsMultipleInstanceTypes)
hAllowsMultipleInstanceTypes = lens _hAllowsMultipleInstanceTypes (\s a -> s {_hAllowsMultipleInstanceTypes = a})

-- | The hardware specifications of the Dedicated Host.
hHostProperties :: Lens' Host (Maybe HostProperties)
hHostProperties = lens _hHostProperties (\s a -> s {_hHostProperties = a})

-- | The ID of the AWS account that owns the Dedicated Host.
hOwnerId :: Lens' Host (Maybe Text)
hOwnerId = lens _hOwnerId (\s a -> s {_hOwnerId = a})

-- | The Availability Zone of the Dedicated Host.
hAvailabilityZone :: Lens' Host (Maybe Text)
hAvailabilityZone = lens _hAvailabilityZone (\s a -> s {_hAvailabilityZone = a})

-- | The IDs and instance type that are currently running on the Dedicated Host.
hInstances :: Lens' Host [HostInstance]
hInstances = lens _hInstances (\s a -> s {_hInstances = a}) . _Default . _Coerce

-- | The time that the Dedicated Host was allocated.
hAllocationTime :: Lens' Host (Maybe UTCTime)
hAllocationTime = lens _hAllocationTime (\s a -> s {_hAllocationTime = a}) . mapping _Time

-- | Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
hMemberOfServiceLinkedResourceGroup :: Lens' Host (Maybe Bool)
hMemberOfServiceLinkedResourceGroup = lens _hMemberOfServiceLinkedResourceGroup (\s a -> s {_hMemberOfServiceLinkedResourceGroup = a})

-- | Indicates whether host recovery is enabled or disabled for the Dedicated Host.
hHostRecovery :: Lens' Host (Maybe HostRecovery)
hHostRecovery = lens _hHostRecovery (\s a -> s {_hHostRecovery = a})

-- | Whether auto-placement is on or off.
hAutoPlacement :: Lens' Host (Maybe AutoPlacement)
hAutoPlacement = lens _hAutoPlacement (\s a -> s {_hAutoPlacement = a})

-- | Any tags assigned to the Dedicated Host.
hTags :: Lens' Host [Tag]
hTags = lens _hTags (\s a -> s {_hTags = a}) . _Default . _Coerce

instance FromXML Host where
  parseXML x =
    Host'
      <$> (x .@? "releaseTime")
      <*> (x .@? "state")
      <*> (x .@? "clientToken")
      <*> (x .@? "availabilityZoneId")
      <*> (x .@? "hostId")
      <*> (x .@? "availableCapacity")
      <*> (x .@? "hostReservationId")
      <*> (x .@? "allowsMultipleInstanceTypes")
      <*> (x .@? "hostProperties")
      <*> (x .@? "ownerId")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "instances" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "allocationTime")
      <*> (x .@? "memberOfServiceLinkedResourceGroup")
      <*> (x .@? "hostRecovery")
      <*> (x .@? "autoPlacement")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable Host

instance NFData Host

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationInstancePlatform
import Network.AWS.EC2.Types.CapacityReservationState
import Network.AWS.EC2.Types.CapacityReservationTenancy
import Network.AWS.EC2.Types.EndDateType
import Network.AWS.EC2.Types.InstanceMatchCriteria
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Capacity Reservation.
--
--
--
-- /See:/ 'capacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { _crState ::
      !(Maybe CapacityReservationState),
    _crAvailabilityZoneId :: !(Maybe Text),
    _crCreateDate :: !(Maybe ISO8601),
    _crEndDate :: !(Maybe ISO8601),
    _crAvailableInstanceCount :: !(Maybe Int),
    _crEphemeralStorage :: !(Maybe Bool),
    _crInstancePlatform ::
      !(Maybe CapacityReservationInstancePlatform),
    _crInstanceMatchCriteria ::
      !(Maybe InstanceMatchCriteria),
    _crCapacityReservationId :: !(Maybe Text),
    _crInstanceType :: !(Maybe Text),
    _crEBSOptimized :: !(Maybe Bool),
    _crOwnerId :: !(Maybe Text),
    _crAvailabilityZone :: !(Maybe Text),
    _crTenancy :: !(Maybe CapacityReservationTenancy),
    _crTotalInstanceCount :: !(Maybe Int),
    _crEndDateType :: !(Maybe EndDateType),
    _crTags :: !(Maybe [Tag]),
    _crCapacityReservationARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crState' - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:     * @active@ - The Capacity Reservation is active and the capacity is available for your use.     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
-- * 'crAvailabilityZoneId' - The Availability Zone ID of the Capacity Reservation.
--
-- * 'crCreateDate' - The date and time at which the Capacity Reservation was created.
--
-- * 'crEndDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- * 'crAvailableInstanceCount' - The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- * 'crEphemeralStorage' - Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- * 'crInstancePlatform' - The type of operating system for which the Capacity Reservation reserves capacity.
--
-- * 'crInstanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
-- * 'crCapacityReservationId' - The ID of the Capacity Reservation.
--
-- * 'crInstanceType' - The type of instance for which the Capacity Reservation reserves capacity.
--
-- * 'crEBSOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- * 'crOwnerId' - The ID of the AWS account that owns the Capacity Reservation.
--
-- * 'crAvailabilityZone' - The Availability Zone in which the capacity is reserved.
--
-- * 'crTenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
-- * 'crTotalInstanceCount' - The total number of instances for which the Capacity Reservation reserves capacity.
--
-- * 'crEndDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
-- * 'crTags' - Any tags assigned to the Capacity Reservation.
--
-- * 'crCapacityReservationARN' - The Amazon Resource Name (ARN) of the Capacity Reservation.
capacityReservation ::
  CapacityReservation
capacityReservation =
  CapacityReservation'
    { _crState = Nothing,
      _crAvailabilityZoneId = Nothing,
      _crCreateDate = Nothing,
      _crEndDate = Nothing,
      _crAvailableInstanceCount = Nothing,
      _crEphemeralStorage = Nothing,
      _crInstancePlatform = Nothing,
      _crInstanceMatchCriteria = Nothing,
      _crCapacityReservationId = Nothing,
      _crInstanceType = Nothing,
      _crEBSOptimized = Nothing,
      _crOwnerId = Nothing,
      _crAvailabilityZone = Nothing,
      _crTenancy = Nothing,
      _crTotalInstanceCount = Nothing,
      _crEndDateType = Nothing,
      _crTags = Nothing,
      _crCapacityReservationARN = Nothing
    }

-- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:     * @active@ - The Capacity Reservation is active and the capacity is available for your use.     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
crState :: Lens' CapacityReservation (Maybe CapacityReservationState)
crState = lens _crState (\s a -> s {_crState = a})

-- | The Availability Zone ID of the Capacity Reservation.
crAvailabilityZoneId :: Lens' CapacityReservation (Maybe Text)
crAvailabilityZoneId = lens _crAvailabilityZoneId (\s a -> s {_crAvailabilityZoneId = a})

-- | The date and time at which the Capacity Reservation was created.
crCreateDate :: Lens' CapacityReservation (Maybe UTCTime)
crCreateDate = lens _crCreateDate (\s a -> s {_crCreateDate = a}) . mapping _Time

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
crEndDate :: Lens' CapacityReservation (Maybe UTCTime)
crEndDate = lens _crEndDate (\s a -> s {_crEndDate = a}) . mapping _Time

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
crAvailableInstanceCount :: Lens' CapacityReservation (Maybe Int)
crAvailableInstanceCount = lens _crAvailableInstanceCount (\s a -> s {_crAvailableInstanceCount = a})

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
crEphemeralStorage :: Lens' CapacityReservation (Maybe Bool)
crEphemeralStorage = lens _crEphemeralStorage (\s a -> s {_crEphemeralStorage = a})

-- | The type of operating system for which the Capacity Reservation reserves capacity.
crInstancePlatform :: Lens' CapacityReservation (Maybe CapacityReservationInstancePlatform)
crInstancePlatform = lens _crInstancePlatform (\s a -> s {_crInstancePlatform = a})

-- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
crInstanceMatchCriteria :: Lens' CapacityReservation (Maybe InstanceMatchCriteria)
crInstanceMatchCriteria = lens _crInstanceMatchCriteria (\s a -> s {_crInstanceMatchCriteria = a})

-- | The ID of the Capacity Reservation.
crCapacityReservationId :: Lens' CapacityReservation (Maybe Text)
crCapacityReservationId = lens _crCapacityReservationId (\s a -> s {_crCapacityReservationId = a})

-- | The type of instance for which the Capacity Reservation reserves capacity.
crInstanceType :: Lens' CapacityReservation (Maybe Text)
crInstanceType = lens _crInstanceType (\s a -> s {_crInstanceType = a})

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
crEBSOptimized :: Lens' CapacityReservation (Maybe Bool)
crEBSOptimized = lens _crEBSOptimized (\s a -> s {_crEBSOptimized = a})

-- | The ID of the AWS account that owns the Capacity Reservation.
crOwnerId :: Lens' CapacityReservation (Maybe Text)
crOwnerId = lens _crOwnerId (\s a -> s {_crOwnerId = a})

-- | The Availability Zone in which the capacity is reserved.
crAvailabilityZone :: Lens' CapacityReservation (Maybe Text)
crAvailabilityZone = lens _crAvailabilityZone (\s a -> s {_crAvailabilityZone = a})

-- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
crTenancy :: Lens' CapacityReservation (Maybe CapacityReservationTenancy)
crTenancy = lens _crTenancy (\s a -> s {_crTenancy = a})

-- | The total number of instances for which the Capacity Reservation reserves capacity.
crTotalInstanceCount :: Lens' CapacityReservation (Maybe Int)
crTotalInstanceCount = lens _crTotalInstanceCount (\s a -> s {_crTotalInstanceCount = a})

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
crEndDateType :: Lens' CapacityReservation (Maybe EndDateType)
crEndDateType = lens _crEndDateType (\s a -> s {_crEndDateType = a})

-- | Any tags assigned to the Capacity Reservation.
crTags :: Lens' CapacityReservation [Tag]
crTags = lens _crTags (\s a -> s {_crTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
crCapacityReservationARN :: Lens' CapacityReservation (Maybe Text)
crCapacityReservationARN = lens _crCapacityReservationARN (\s a -> s {_crCapacityReservationARN = a})

instance FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      <$> (x .@? "state")
      <*> (x .@? "availabilityZoneId")
      <*> (x .@? "createDate")
      <*> (x .@? "endDate")
      <*> (x .@? "availableInstanceCount")
      <*> (x .@? "ephemeralStorage")
      <*> (x .@? "instancePlatform")
      <*> (x .@? "instanceMatchCriteria")
      <*> (x .@? "capacityReservationId")
      <*> (x .@? "instanceType")
      <*> (x .@? "ebsOptimized")
      <*> (x .@? "ownerId")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "tenancy")
      <*> (x .@? "totalInstanceCount")
      <*> (x .@? "endDateType")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "capacityReservationArn")

instance Hashable CapacityReservation

instance NFData CapacityReservation

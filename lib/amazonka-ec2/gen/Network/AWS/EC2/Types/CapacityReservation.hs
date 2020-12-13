{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservation
  ( CapacityReservation (..),

    -- * Smart constructor
    mkCapacityReservation,

    -- * Lenses
    crState,
    crAvailabilityZoneId,
    crCreateDate,
    crEndDate,
    crAvailableInstanceCount,
    crEphemeralStorage,
    crInstancePlatform,
    crInstanceMatchCriteria,
    crCapacityReservationId,
    crInstanceType,
    crEBSOptimized,
    crOwnerId,
    crAvailabilityZone,
    crTenancy,
    crTotalInstanceCount,
    crEndDateType,
    crTags,
    crCapacityReservationARN,
  )
where

import Network.AWS.EC2.Types.CapacityReservationInstancePlatform
import Network.AWS.EC2.Types.CapacityReservationState
import Network.AWS.EC2.Types.CapacityReservationTenancy
import Network.AWS.EC2.Types.EndDateType
import Network.AWS.EC2.Types.InstanceMatchCriteria
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Capacity Reservation.
--
-- /See:/ 'mkCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { -- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
    --
    --
    --     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
    --
    --
    --     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
    --
    --
    --     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
    --
    --
    --     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
    --
    --
    --     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
    state :: Lude.Maybe CapacityReservationState,
    -- | The Availability Zone ID of the Capacity Reservation.
    availabilityZoneId :: Lude.Maybe Lude.Text,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Lude.Maybe Lude.DateTime,
    -- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
    endDate :: Lude.Maybe Lude.DateTime,
    -- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
    availableInstanceCount :: Lude.Maybe Lude.Int,
    -- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
    ephemeralStorage :: Lude.Maybe Lude.Bool,
    -- | The type of operating system for which the Capacity Reservation reserves capacity.
    instancePlatform :: Lude.Maybe CapacityReservationInstancePlatform,
    -- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
    --
    --
    --     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
    --
    --
    --     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
    instanceMatchCriteria :: Lude.Maybe InstanceMatchCriteria,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Lude.Maybe Lude.Text,
    -- | The type of instance for which the Capacity Reservation reserves capacity.
    instanceType :: Lude.Maybe Lude.Text,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The ID of the AWS account that owns the Capacity Reservation.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The Availability Zone in which the capacity is reserved.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    tenancy :: Lude.Maybe CapacityReservationTenancy,
    -- | The total number of instances for which the Capacity Reservation reserves capacity.
    totalInstanceCount :: Lude.Maybe Lude.Int,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
    endDateType :: Lude.Maybe EndDateType,
    -- | Any tags assigned to the Capacity Reservation.
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation.
    capacityReservationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservation' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
-- * 'availabilityZoneId' - The Availability Zone ID of the Capacity Reservation.
-- * 'createDate' - The date and time at which the Capacity Reservation was created.
-- * 'endDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
-- * 'availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
-- * 'ephemeralStorage' - Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
-- * 'instancePlatform' - The type of operating system for which the Capacity Reservation reserves capacity.
-- * 'instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--
--     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'instanceType' - The type of instance for which the Capacity Reservation reserves capacity.
-- * 'ebsOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
-- * 'ownerId' - The ID of the AWS account that owns the Capacity Reservation.
-- * 'availabilityZone' - The Availability Zone in which the capacity is reserved.
-- * 'tenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
-- * 'totalInstanceCount' - The total number of instances for which the Capacity Reservation reserves capacity.
-- * 'endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
--
-- * 'tags' - Any tags assigned to the Capacity Reservation.
-- * 'capacityReservationARN' - The Amazon Resource Name (ARN) of the Capacity Reservation.
mkCapacityReservation ::
  CapacityReservation
mkCapacityReservation =
  CapacityReservation'
    { state = Lude.Nothing,
      availabilityZoneId = Lude.Nothing,
      createDate = Lude.Nothing,
      endDate = Lude.Nothing,
      availableInstanceCount = Lude.Nothing,
      ephemeralStorage = Lude.Nothing,
      instancePlatform = Lude.Nothing,
      instanceMatchCriteria = Lude.Nothing,
      capacityReservationId = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      ownerId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      totalInstanceCount = Lude.Nothing,
      endDateType = Lude.Nothing,
      tags = Lude.Nothing,
      capacityReservationARN = Lude.Nothing
    }

-- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crState :: Lens.Lens' CapacityReservation (Lude.Maybe CapacityReservationState)
crState = Lens.lens (state :: CapacityReservation -> Lude.Maybe CapacityReservationState) (\s a -> s {state = a} :: CapacityReservation)
{-# DEPRECATED crState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Availability Zone ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailabilityZoneId :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crAvailabilityZoneId = Lens.lens (availabilityZoneId :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: CapacityReservation)
{-# DEPRECATED crAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The date and time at which the Capacity Reservation was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreateDate :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.DateTime)
crCreateDate = Lens.lens (createDate :: CapacityReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: CapacityReservation)
{-# DEPRECATED crCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEndDate :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.DateTime)
crEndDate = Lens.lens (endDate :: CapacityReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {endDate = a} :: CapacityReservation)
{-# DEPRECATED crEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailableInstanceCount :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Int)
crAvailableInstanceCount = Lens.lens (availableInstanceCount :: CapacityReservation -> Lude.Maybe Lude.Int) (\s a -> s {availableInstanceCount = a} :: CapacityReservation)
{-# DEPRECATED crAvailableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead." #-}

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- /Note:/ Consider using 'ephemeralStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEphemeralStorage :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Bool)
crEphemeralStorage = Lens.lens (ephemeralStorage :: CapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {ephemeralStorage = a} :: CapacityReservation)
{-# DEPRECATED crEphemeralStorage "Use generic-lens or generic-optics with 'ephemeralStorage' instead." #-}

-- | The type of operating system for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instancePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstancePlatform :: Lens.Lens' CapacityReservation (Lude.Maybe CapacityReservationInstancePlatform)
crInstancePlatform = Lens.lens (instancePlatform :: CapacityReservation -> Lude.Maybe CapacityReservationInstancePlatform) (\s a -> s {instancePlatform = a} :: CapacityReservation)
{-# DEPRECATED crInstancePlatform "Use generic-lens or generic-optics with 'instancePlatform' instead." #-}

-- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--
--     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
--
-- /Note:/ Consider using 'instanceMatchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceMatchCriteria :: Lens.Lens' CapacityReservation (Lude.Maybe InstanceMatchCriteria)
crInstanceMatchCriteria = Lens.lens (instanceMatchCriteria :: CapacityReservation -> Lude.Maybe InstanceMatchCriteria) (\s a -> s {instanceMatchCriteria = a} :: CapacityReservation)
{-# DEPRECATED crInstanceMatchCriteria "Use generic-lens or generic-optics with 'instanceMatchCriteria' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCapacityReservationId :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crCapacityReservationId = Lens.lens (capacityReservationId :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: CapacityReservation)
{-# DEPRECATED crCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The type of instance for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceType :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crInstanceType = Lens.lens (instanceType :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: CapacityReservation)
{-# DEPRECATED crInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEBSOptimized :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Bool)
crEBSOptimized = Lens.lens (ebsOptimized :: CapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: CapacityReservation)
{-# DEPRECATED crEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The ID of the AWS account that owns the Capacity Reservation.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crOwnerId :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crOwnerId = Lens.lens (ownerId :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: CapacityReservation)
{-# DEPRECATED crOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Availability Zone in which the capacity is reserved.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailabilityZone :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crAvailabilityZone = Lens.lens (availabilityZone :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CapacityReservation)
{-# DEPRECATED crAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTenancy :: Lens.Lens' CapacityReservation (Lude.Maybe CapacityReservationTenancy)
crTenancy = Lens.lens (tenancy :: CapacityReservation -> Lude.Maybe CapacityReservationTenancy) (\s a -> s {tenancy = a} :: CapacityReservation)
{-# DEPRECATED crTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The total number of instances for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'totalInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTotalInstanceCount :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Int)
crTotalInstanceCount = Lens.lens (totalInstanceCount :: CapacityReservation -> Lude.Maybe Lude.Int) (\s a -> s {totalInstanceCount = a} :: CapacityReservation)
{-# DEPRECATED crTotalInstanceCount "Use generic-lens or generic-optics with 'totalInstanceCount' instead." #-}

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
--
--
-- /Note:/ Consider using 'endDateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEndDateType :: Lens.Lens' CapacityReservation (Lude.Maybe EndDateType)
crEndDateType = Lens.lens (endDateType :: CapacityReservation -> Lude.Maybe EndDateType) (\s a -> s {endDateType = a} :: CapacityReservation)
{-# DEPRECATED crEndDateType "Use generic-lens or generic-optics with 'endDateType' instead." #-}

-- | Any tags assigned to the Capacity Reservation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CapacityReservation (Lude.Maybe [Tag])
crTags = Lens.lens (tags :: CapacityReservation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CapacityReservation)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCapacityReservationARN :: Lens.Lens' CapacityReservation (Lude.Maybe Lude.Text)
crCapacityReservationARN = Lens.lens (capacityReservationARN :: CapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationARN = a} :: CapacityReservation)
{-# DEPRECATED crCapacityReservationARN "Use generic-lens or generic-optics with 'capacityReservationARN' instead." #-}

instance Lude.FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "availabilityZoneId")
      Lude.<*> (x Lude..@? "createDate")
      Lude.<*> (x Lude..@? "endDate")
      Lude.<*> (x Lude..@? "availableInstanceCount")
      Lude.<*> (x Lude..@? "ephemeralStorage")
      Lude.<*> (x Lude..@? "instancePlatform")
      Lude.<*> (x Lude..@? "instanceMatchCriteria")
      Lude.<*> (x Lude..@? "capacityReservationId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "tenancy")
      Lude.<*> (x Lude..@? "totalInstanceCount")
      Lude.<*> (x Lude..@? "endDateType")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "capacityReservationArn")

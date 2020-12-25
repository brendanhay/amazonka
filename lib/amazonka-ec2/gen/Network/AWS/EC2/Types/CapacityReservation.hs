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
    crAvailabilityZone,
    crAvailabilityZoneId,
    crAvailableInstanceCount,
    crCapacityReservationArn,
    crCapacityReservationId,
    crCreateDate,
    crEbsOptimized,
    crEndDate,
    crEndDateType,
    crEphemeralStorage,
    crInstanceMatchCriteria,
    crInstancePlatform,
    crInstanceType,
    crOwnerId,
    crState,
    crTags,
    crTenancy,
    crTotalInstanceCount,
  )
where

import qualified Network.AWS.EC2.Types.CapacityReservationInstancePlatform as Types
import qualified Network.AWS.EC2.Types.CapacityReservationState as Types
import qualified Network.AWS.EC2.Types.CapacityReservationTenancy as Types
import qualified Network.AWS.EC2.Types.EndDateType as Types
import qualified Network.AWS.EC2.Types.InstanceMatchCriteria as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Capacity Reservation.
--
-- /See:/ 'mkCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { -- | The Availability Zone in which the capacity is reserved.
    availabilityZone :: Core.Maybe Types.String,
    -- | The Availability Zone ID of the Capacity Reservation.
    availabilityZoneId :: Core.Maybe Types.String,
    -- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
    availableInstanceCount :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation.
    capacityReservationArn :: Core.Maybe Types.String,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Core.Maybe Types.String,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
    endDate :: Core.Maybe Core.UTCTime,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
    endDateType :: Core.Maybe Types.EndDateType,
    -- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
    ephemeralStorage :: Core.Maybe Core.Bool,
    -- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
    --
    --
    --     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
    --
    --
    --     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
    instanceMatchCriteria :: Core.Maybe Types.InstanceMatchCriteria,
    -- | The type of operating system for which the Capacity Reservation reserves capacity.
    instancePlatform :: Core.Maybe Types.CapacityReservationInstancePlatform,
    -- | The type of instance for which the Capacity Reservation reserves capacity.
    instanceType :: Core.Maybe Types.String,
    -- | The ID of the AWS account that owns the Capacity Reservation.
    ownerId :: Core.Maybe Types.String,
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
    state :: Core.Maybe Types.CapacityReservationState,
    -- | Any tags assigned to the Capacity Reservation.
    tags :: Core.Maybe [Types.Tag],
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    tenancy :: Core.Maybe Types.CapacityReservationTenancy,
    -- | The total number of instances for which the Capacity Reservation reserves capacity.
    totalInstanceCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CapacityReservation' value with any optional fields omitted.
mkCapacityReservation ::
  CapacityReservation
mkCapacityReservation =
  CapacityReservation'
    { availabilityZone = Core.Nothing,
      availabilityZoneId = Core.Nothing,
      availableInstanceCount = Core.Nothing,
      capacityReservationArn = Core.Nothing,
      capacityReservationId = Core.Nothing,
      createDate = Core.Nothing,
      ebsOptimized = Core.Nothing,
      endDate = Core.Nothing,
      endDateType = Core.Nothing,
      ephemeralStorage = Core.Nothing,
      instanceMatchCriteria = Core.Nothing,
      instancePlatform = Core.Nothing,
      instanceType = Core.Nothing,
      ownerId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      tenancy = Core.Nothing,
      totalInstanceCount = Core.Nothing
    }

-- | The Availability Zone in which the capacity is reserved.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailabilityZone :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED crAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The Availability Zone ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailabilityZoneId :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# DEPRECATED crAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAvailableInstanceCount :: Lens.Lens' CapacityReservation (Core.Maybe Core.Int)
crAvailableInstanceCount = Lens.field @"availableInstanceCount"
{-# DEPRECATED crAvailableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCapacityReservationArn :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crCapacityReservationArn = Lens.field @"capacityReservationArn"
{-# DEPRECATED crCapacityReservationArn "Use generic-lens or generic-optics with 'capacityReservationArn' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCapacityReservationId :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED crCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The date and time at which the Capacity Reservation was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreateDate :: Lens.Lens' CapacityReservation (Core.Maybe Core.UTCTime)
crCreateDate = Lens.field @"createDate"
{-# DEPRECATED crCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEbsOptimized :: Lens.Lens' CapacityReservation (Core.Maybe Core.Bool)
crEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED crEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEndDate :: Lens.Lens' CapacityReservation (Core.Maybe Core.UTCTime)
crEndDate = Lens.field @"endDate"
{-# DEPRECATED crEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

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
crEndDateType :: Lens.Lens' CapacityReservation (Core.Maybe Types.EndDateType)
crEndDateType = Lens.field @"endDateType"
{-# DEPRECATED crEndDateType "Use generic-lens or generic-optics with 'endDateType' instead." #-}

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- /Note:/ Consider using 'ephemeralStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEphemeralStorage :: Lens.Lens' CapacityReservation (Core.Maybe Core.Bool)
crEphemeralStorage = Lens.field @"ephemeralStorage"
{-# DEPRECATED crEphemeralStorage "Use generic-lens or generic-optics with 'ephemeralStorage' instead." #-}

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
crInstanceMatchCriteria :: Lens.Lens' CapacityReservation (Core.Maybe Types.InstanceMatchCriteria)
crInstanceMatchCriteria = Lens.field @"instanceMatchCriteria"
{-# DEPRECATED crInstanceMatchCriteria "Use generic-lens or generic-optics with 'instanceMatchCriteria' instead." #-}

-- | The type of operating system for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instancePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstancePlatform :: Lens.Lens' CapacityReservation (Core.Maybe Types.CapacityReservationInstancePlatform)
crInstancePlatform = Lens.field @"instancePlatform"
{-# DEPRECATED crInstancePlatform "Use generic-lens or generic-optics with 'instancePlatform' instead." #-}

-- | The type of instance for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceType :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crInstanceType = Lens.field @"instanceType"
{-# DEPRECATED crInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the AWS account that owns the Capacity Reservation.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crOwnerId :: Lens.Lens' CapacityReservation (Core.Maybe Types.String)
crOwnerId = Lens.field @"ownerId"
{-# DEPRECATED crOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

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
crState :: Lens.Lens' CapacityReservation (Core.Maybe Types.CapacityReservationState)
crState = Lens.field @"state"
{-# DEPRECATED crState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Any tags assigned to the Capacity Reservation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CapacityReservation (Core.Maybe [Types.Tag])
crTags = Lens.field @"tags"
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
crTenancy :: Lens.Lens' CapacityReservation (Core.Maybe Types.CapacityReservationTenancy)
crTenancy = Lens.field @"tenancy"
{-# DEPRECATED crTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The total number of instances for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'totalInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTotalInstanceCount :: Lens.Lens' CapacityReservation (Core.Maybe Core.Int)
crTotalInstanceCount = Lens.field @"totalInstanceCount"
{-# DEPRECATED crTotalInstanceCount "Use generic-lens or generic-optics with 'totalInstanceCount' instead." #-}

instance Core.FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "availabilityZoneId")
      Core.<*> (x Core..@? "availableInstanceCount")
      Core.<*> (x Core..@? "capacityReservationArn")
      Core.<*> (x Core..@? "capacityReservationId")
      Core.<*> (x Core..@? "createDate")
      Core.<*> (x Core..@? "ebsOptimized")
      Core.<*> (x Core..@? "endDate")
      Core.<*> (x Core..@? "endDateType")
      Core.<*> (x Core..@? "ephemeralStorage")
      Core.<*> (x Core..@? "instanceMatchCriteria")
      Core.<*> (x Core..@? "instancePlatform")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "tenancy")
      Core.<*> (x Core..@? "totalInstanceCount")

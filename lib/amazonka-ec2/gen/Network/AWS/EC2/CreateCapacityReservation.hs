{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Capacity Reservation with the specified attributes.
--
-- Capacity Reservations enable you to reserve capacity for your Amazon EC2 instances in a specific Availability Zone for any duration. This gives you the flexibility to selectively add capacity reservations and still get the Regional RI discounts for that usage. By creating Capacity Reservations, you ensure that you always have access to Amazon EC2 capacity when you need it, for as long as you need it. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Your request to create a Capacity Reservation could fail if Amazon EC2 does not have sufficient capacity to fulfill the request. If your request fails due to Amazon EC2 capacity constraints, either try again at a later time, try in a different Availability Zone, or request a smaller capacity reservation. If your application is flexible across instance types and sizes, try to create a Capacity Reservation with different instance attributes.
-- Your request could also fail if the requested quantity exceeds your On-Demand Instance limit for the selected instance type. If your request fails due to limit constraints, increase your On-Demand Instance limit for the required instance type and try again. For more information about increasing your instance limits, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 Service Limits> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateCapacityReservation
  ( -- * Creating a request
    CreateCapacityReservation (..),
    mkCreateCapacityReservation,

    -- ** Request lenses
    ccrInstanceType,
    ccrInstancePlatform,
    ccrInstanceCount,
    ccrAvailabilityZone,
    ccrAvailabilityZoneId,
    ccrClientToken,
    ccrDryRun,
    ccrEbsOptimized,
    ccrEndDate,
    ccrEndDateType,
    ccrEphemeralStorage,
    ccrInstanceMatchCriteria,
    ccrTagSpecifications,
    ccrTenancy,

    -- * Destructuring the response
    CreateCapacityReservationResponse (..),
    mkCreateCapacityReservationResponse,

    -- ** Response lenses
    ccrrrsCapacityReservation,
    ccrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCapacityReservation' smart constructor.
data CreateCapacityReservation = CreateCapacityReservation'
  { -- | The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceType :: Types.String,
    -- | The type of operating system for which to reserve capacity.
    instancePlatform :: Types.CapacityReservationInstancePlatform,
    -- | The number of instances for which to reserve capacity.
    instanceCount :: Core.Int,
    -- | The Availability Zone in which to create the Capacity Reservation.
    availabilityZone :: Core.Maybe Types.String,
    -- | The ID of the Availability Zone in which to create the Capacity Reservation.
    availabilityZoneId :: Core.Maybe Types.String,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
    --
    -- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
    -- If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
    endDate :: Core.Maybe Core.UTCTime,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
    endDateType :: Core.Maybe Types.EndDateType,
    -- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
    ephemeralStorage :: Core.Maybe Core.Bool,
    -- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
    --
    --
    --     * @open@ - The Capacity Reservation automatically matches all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes run in the Capacity Reservation automatically without specifying any additional parameters.
    --
    --
    --     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
    --
    --
    -- Default: @open@
    instanceMatchCriteria :: Core.Maybe Types.InstanceMatchCriteria,
    -- | The tags to apply to the Capacity Reservation during launch.
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    tenancy :: Core.Maybe Types.CapacityReservationTenancy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateCapacityReservation' value with any optional fields omitted.
mkCreateCapacityReservation ::
  -- | 'instanceType'
  Types.String ->
  -- | 'instancePlatform'
  Types.CapacityReservationInstancePlatform ->
  -- | 'instanceCount'
  Core.Int ->
  CreateCapacityReservation
mkCreateCapacityReservation
  instanceType
  instancePlatform
  instanceCount =
    CreateCapacityReservation'
      { instanceType,
        instancePlatform,
        instanceCount,
        availabilityZone = Core.Nothing,
        availabilityZoneId = Core.Nothing,
        clientToken = Core.Nothing,
        dryRun = Core.Nothing,
        ebsOptimized = Core.Nothing,
        endDate = Core.Nothing,
        endDateType = Core.Nothing,
        ephemeralStorage = Core.Nothing,
        instanceMatchCriteria = Core.Nothing,
        tagSpecifications = Core.Nothing,
        tenancy = Core.Nothing
      }

-- | The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrInstanceType :: Lens.Lens' CreateCapacityReservation Types.String
ccrInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ccrInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The type of operating system for which to reserve capacity.
--
-- /Note:/ Consider using 'instancePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrInstancePlatform :: Lens.Lens' CreateCapacityReservation Types.CapacityReservationInstancePlatform
ccrInstancePlatform = Lens.field @"instancePlatform"
{-# DEPRECATED ccrInstancePlatform "Use generic-lens or generic-optics with 'instancePlatform' instead." #-}

-- | The number of instances for which to reserve capacity.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrInstanceCount :: Lens.Lens' CreateCapacityReservation Core.Int
ccrInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED ccrInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Availability Zone in which to create the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrAvailabilityZone :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.String)
ccrAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ccrAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the Availability Zone in which to create the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrAvailabilityZoneId :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.String)
ccrAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# DEPRECATED ccrAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrClientToken :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.String)
ccrClientToken = Lens.field @"clientToken"
{-# DEPRECATED ccrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrDryRun :: Lens.Lens' CreateCapacityReservation (Core.Maybe Core.Bool)
ccrDryRun = Lens.field @"dryRun"
{-# DEPRECATED ccrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEbsOptimized :: Lens.Lens' CreateCapacityReservation (Core.Maybe Core.Bool)
ccrEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED ccrEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
-- If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEndDate :: Lens.Lens' CreateCapacityReservation (Core.Maybe Core.UTCTime)
ccrEndDate = Lens.field @"endDate"
{-# DEPRECATED ccrEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
--
--
--
-- /Note:/ Consider using 'endDateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEndDateType :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.EndDateType)
ccrEndDateType = Lens.field @"endDateType"
{-# DEPRECATED ccrEndDateType "Use generic-lens or generic-optics with 'endDateType' instead." #-}

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- /Note:/ Consider using 'ephemeralStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEphemeralStorage :: Lens.Lens' CreateCapacityReservation (Core.Maybe Core.Bool)
ccrEphemeralStorage = Lens.field @"ephemeralStorage"
{-# DEPRECATED ccrEphemeralStorage "Use generic-lens or generic-optics with 'ephemeralStorage' instead." #-}

-- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--
--     * @open@ - The Capacity Reservation automatically matches all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes run in the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
-- Default: @open@
--
-- /Note:/ Consider using 'instanceMatchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrInstanceMatchCriteria :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.InstanceMatchCriteria)
ccrInstanceMatchCriteria = Lens.field @"instanceMatchCriteria"
{-# DEPRECATED ccrInstanceMatchCriteria "Use generic-lens or generic-optics with 'instanceMatchCriteria' instead." #-}

-- | The tags to apply to the Capacity Reservation during launch.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrTagSpecifications :: Lens.Lens' CreateCapacityReservation (Core.Maybe [Types.TagSpecification])
ccrTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ccrTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

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
ccrTenancy :: Lens.Lens' CreateCapacityReservation (Core.Maybe Types.CapacityReservationTenancy)
ccrTenancy = Lens.field @"tenancy"
{-# DEPRECATED ccrTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

instance Core.AWSRequest CreateCapacityReservation where
  type
    Rs CreateCapacityReservation =
      CreateCapacityReservationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateCapacityReservation")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceType" instanceType)
                Core.<> (Core.toQueryValue "InstancePlatform" instancePlatform)
                Core.<> (Core.toQueryValue "InstanceCount" instanceCount)
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> ( Core.toQueryValue "AvailabilityZoneId"
                            Core.<$> availabilityZoneId
                        )
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "EbsOptimized" Core.<$> ebsOptimized)
                Core.<> (Core.toQueryValue "EndDate" Core.<$> endDate)
                Core.<> (Core.toQueryValue "EndDateType" Core.<$> endDateType)
                Core.<> (Core.toQueryValue "EphemeralStorage" Core.<$> ephemeralStorage)
                Core.<> ( Core.toQueryValue "InstanceMatchCriteria"
                            Core.<$> instanceMatchCriteria
                        )
                Core.<> (Core.toQueryList "TagSpecifications" Core.<$> tagSpecifications)
                Core.<> (Core.toQueryValue "Tenancy" Core.<$> tenancy)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCapacityReservationResponse'
            Core.<$> (x Core..@? "capacityReservation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCapacityReservationResponse' smart constructor.
data CreateCapacityReservationResponse = CreateCapacityReservationResponse'
  { -- | Information about the Capacity Reservation.
    capacityReservation :: Core.Maybe Types.CapacityReservation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateCapacityReservationResponse' value with any optional fields omitted.
mkCreateCapacityReservationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCapacityReservationResponse
mkCreateCapacityReservationResponse responseStatus =
  CreateCapacityReservationResponse'
    { capacityReservation =
        Core.Nothing,
      responseStatus
    }

-- | Information about the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrrsCapacityReservation :: Lens.Lens' CreateCapacityReservationResponse (Core.Maybe Types.CapacityReservation)
ccrrrsCapacityReservation = Lens.field @"capacityReservation"
{-# DEPRECATED ccrrrsCapacityReservation "Use generic-lens or generic-optics with 'capacityReservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrrsResponseStatus :: Lens.Lens' CreateCapacityReservationResponse Core.Int
ccrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

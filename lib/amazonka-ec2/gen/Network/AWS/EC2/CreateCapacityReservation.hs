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
    ccrfClientToken,
    ccrfInstanceCount,
    ccrfAvailabilityZoneId,
    ccrfEndDate,
    ccrfEphemeralStorage,
    ccrfInstancePlatform,
    ccrfInstanceMatchCriteria,
    ccrfInstanceType,
    ccrfEBSOptimized,
    ccrfTagSpecifications,
    ccrfAvailabilityZone,
    ccrfTenancy,
    ccrfEndDateType,
    ccrfDryRun,

    -- * Destructuring the response
    CreateCapacityReservationResponse (..),
    mkCreateCapacityReservationResponse,

    -- ** Response lenses
    ccrrsCapacityReservation,
    ccrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCapacityReservation' smart constructor.
data CreateCapacityReservation = CreateCapacityReservation'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The number of instances for which to reserve capacity.
    instanceCount :: Lude.Int,
    -- | The ID of the Availability Zone in which to create the Capacity Reservation.
    availabilityZoneId :: Lude.Maybe Lude.Text,
    -- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
    --
    -- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
    -- If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
    endDate :: Lude.Maybe Lude.DateTime,
    -- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
    ephemeralStorage :: Lude.Maybe Lude.Bool,
    -- | The type of operating system for which to reserve capacity.
    instancePlatform :: CapacityReservationInstancePlatform,
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
    instanceMatchCriteria :: Lude.Maybe InstanceMatchCriteria,
    -- | The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceType :: Lude.Text,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The tags to apply to the Capacity Reservation during launch.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The Availability Zone in which to create the Capacity Reservation.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    tenancy :: Lude.Maybe CapacityReservationTenancy,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
    endDateType :: Lude.Maybe EndDateType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCapacityReservation' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'instanceCount' - The number of instances for which to reserve capacity.
-- * 'availabilityZoneId' - The ID of the Availability Zone in which to create the Capacity Reservation.
-- * 'endDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
-- If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
-- * 'ephemeralStorage' - Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
-- * 'instancePlatform' - The type of operating system for which to reserve capacity.
-- * 'instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--
--     * @open@ - The Capacity Reservation automatically matches all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes run in the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
-- Default: @open@
-- * 'instanceType' - The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'ebsOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
-- * 'tagSpecifications' - The tags to apply to the Capacity Reservation during launch.
-- * 'availabilityZone' - The Availability Zone in which to create the Capacity Reservation.
-- * 'tenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
-- * 'endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
--
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateCapacityReservation ::
  -- | 'instanceCount'
  Lude.Int ->
  -- | 'instancePlatform'
  CapacityReservationInstancePlatform ->
  -- | 'instanceType'
  Lude.Text ->
  CreateCapacityReservation
mkCreateCapacityReservation
  pInstanceCount_
  pInstancePlatform_
  pInstanceType_ =
    CreateCapacityReservation'
      { clientToken = Lude.Nothing,
        instanceCount = pInstanceCount_,
        availabilityZoneId = Lude.Nothing,
        endDate = Lude.Nothing,
        ephemeralStorage = Lude.Nothing,
        instancePlatform = pInstancePlatform_,
        instanceMatchCriteria = Lude.Nothing,
        instanceType = pInstanceType_,
        ebsOptimized = Lude.Nothing,
        tagSpecifications = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        tenancy = Lude.Nothing,
        endDateType = Lude.Nothing,
        dryRun = Lude.Nothing
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfClientToken :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Text)
ccrfClientToken = Lens.lens (clientToken :: CreateCapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of instances for which to reserve capacity.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfInstanceCount :: Lens.Lens' CreateCapacityReservation Lude.Int
ccrfInstanceCount = Lens.lens (instanceCount :: CreateCapacityReservation -> Lude.Int) (\s a -> s {instanceCount = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ID of the Availability Zone in which to create the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfAvailabilityZoneId :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Text)
ccrfAvailabilityZoneId = Lens.lens (availabilityZoneId :: CreateCapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
-- If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfEndDate :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.DateTime)
ccrfEndDate = Lens.lens (endDate :: CreateCapacityReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {endDate = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- /Note:/ Consider using 'ephemeralStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfEphemeralStorage :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Bool)
ccrfEphemeralStorage = Lens.lens (ephemeralStorage :: CreateCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {ephemeralStorage = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfEphemeralStorage "Use generic-lens or generic-optics with 'ephemeralStorage' instead." #-}

-- | The type of operating system for which to reserve capacity.
--
-- /Note:/ Consider using 'instancePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfInstancePlatform :: Lens.Lens' CreateCapacityReservation CapacityReservationInstancePlatform
ccrfInstancePlatform = Lens.lens (instancePlatform :: CreateCapacityReservation -> CapacityReservationInstancePlatform) (\s a -> s {instancePlatform = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfInstancePlatform "Use generic-lens or generic-optics with 'instancePlatform' instead." #-}

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
ccrfInstanceMatchCriteria :: Lens.Lens' CreateCapacityReservation (Lude.Maybe InstanceMatchCriteria)
ccrfInstanceMatchCriteria = Lens.lens (instanceMatchCriteria :: CreateCapacityReservation -> Lude.Maybe InstanceMatchCriteria) (\s a -> s {instanceMatchCriteria = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfInstanceMatchCriteria "Use generic-lens or generic-optics with 'instanceMatchCriteria' instead." #-}

-- | The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfInstanceType :: Lens.Lens' CreateCapacityReservation Lude.Text
ccrfInstanceType = Lens.lens (instanceType :: CreateCapacityReservation -> Lude.Text) (\s a -> s {instanceType = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfEBSOptimized :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Bool)
ccrfEBSOptimized = Lens.lens (ebsOptimized :: CreateCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The tags to apply to the Capacity Reservation during launch.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfTagSpecifications :: Lens.Lens' CreateCapacityReservation (Lude.Maybe [TagSpecification])
ccrfTagSpecifications = Lens.lens (tagSpecifications :: CreateCapacityReservation -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Availability Zone in which to create the Capacity Reservation.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfAvailabilityZone :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Text)
ccrfAvailabilityZone = Lens.lens (availabilityZone :: CreateCapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

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
ccrfTenancy :: Lens.Lens' CreateCapacityReservation (Lude.Maybe CapacityReservationTenancy)
ccrfTenancy = Lens.lens (tenancy :: CreateCapacityReservation -> Lude.Maybe CapacityReservationTenancy) (\s a -> s {tenancy = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

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
ccrfEndDateType :: Lens.Lens' CreateCapacityReservation (Lude.Maybe EndDateType)
ccrfEndDateType = Lens.lens (endDateType :: CreateCapacityReservation -> Lude.Maybe EndDateType) (\s a -> s {endDateType = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfEndDateType "Use generic-lens or generic-optics with 'endDateType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfDryRun :: Lens.Lens' CreateCapacityReservation (Lude.Maybe Lude.Bool)
ccrfDryRun = Lens.lens (dryRun :: CreateCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateCapacityReservation)
{-# DEPRECATED ccrfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateCapacityReservation where
  type
    Rs CreateCapacityReservation =
      CreateCapacityReservationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateCapacityReservationResponse'
            Lude.<$> (x Lude..@? "capacityReservation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCapacityReservation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCapacityReservation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCapacityReservation where
  toQuery CreateCapacityReservation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCapacityReservation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "InstanceCount" Lude.=: instanceCount,
        "AvailabilityZoneId" Lude.=: availabilityZoneId,
        "EndDate" Lude.=: endDate,
        "EphemeralStorage" Lude.=: ephemeralStorage,
        "InstancePlatform" Lude.=: instancePlatform,
        "InstanceMatchCriteria" Lude.=: instanceMatchCriteria,
        "InstanceType" Lude.=: instanceType,
        "EbsOptimized" Lude.=: ebsOptimized,
        Lude.toQuery
          (Lude.toQueryList "TagSpecifications" Lude.<$> tagSpecifications),
        "AvailabilityZone" Lude.=: availabilityZone,
        "Tenancy" Lude.=: tenancy,
        "EndDateType" Lude.=: endDateType,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateCapacityReservationResponse' smart constructor.
data CreateCapacityReservationResponse = CreateCapacityReservationResponse'
  { -- | Information about the Capacity Reservation.
    capacityReservation :: Lude.Maybe CapacityReservation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCapacityReservationResponse' with the minimum fields required to make a request.
--
-- * 'capacityReservation' - Information about the Capacity Reservation.
-- * 'responseStatus' - The response status code.
mkCreateCapacityReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCapacityReservationResponse
mkCreateCapacityReservationResponse pResponseStatus_ =
  CreateCapacityReservationResponse'
    { capacityReservation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCapacityReservation :: Lens.Lens' CreateCapacityReservationResponse (Lude.Maybe CapacityReservation)
ccrrsCapacityReservation = Lens.lens (capacityReservation :: CreateCapacityReservationResponse -> Lude.Maybe CapacityReservation) (\s a -> s {capacityReservation = a} :: CreateCapacityReservationResponse)
{-# DEPRECATED ccrrsCapacityReservation "Use generic-lens or generic-optics with 'capacityReservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCapacityReservationResponse Lude.Int
ccrrsResponseStatus = Lens.lens (responseStatus :: CreateCapacityReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCapacityReservationResponse)
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

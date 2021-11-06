{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateCapacityReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Capacity Reservation with the specified attributes.
--
-- Capacity Reservations enable you to reserve capacity for your Amazon EC2
-- instances in a specific Availability Zone for any duration. This gives
-- you the flexibility to selectively add capacity reservations and still
-- get the Regional RI discounts for that usage. By creating Capacity
-- Reservations, you ensure that you always have access to Amazon EC2
-- capacity when you need it, for as long as you need it. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html Capacity Reservations>
-- in the /Amazon EC2 User Guide/.
--
-- Your request to create a Capacity Reservation could fail if Amazon EC2
-- does not have sufficient capacity to fulfill the request. If your
-- request fails due to Amazon EC2 capacity constraints, either try again
-- at a later time, try in a different Availability Zone, or request a
-- smaller capacity reservation. If your application is flexible across
-- instance types and sizes, try to create a Capacity Reservation with
-- different instance attributes.
--
-- Your request could also fail if the requested quantity exceeds your
-- On-Demand Instance limit for the selected instance type. If your request
-- fails due to limit constraints, increase your On-Demand Instance limit
-- for the required instance type and try again. For more information about
-- increasing your instance limits, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 Service Quotas>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.CreateCapacityReservation
  ( -- * Creating a Request
    CreateCapacityReservation (..),
    newCreateCapacityReservation,

    -- * Request Lenses
    createCapacityReservation_clientToken,
    createCapacityReservation_availabilityZoneId,
    createCapacityReservation_outpostArn,
    createCapacityReservation_endDate,
    createCapacityReservation_ephemeralStorage,
    createCapacityReservation_instanceMatchCriteria,
    createCapacityReservation_ebsOptimized,
    createCapacityReservation_tagSpecifications,
    createCapacityReservation_availabilityZone,
    createCapacityReservation_tenancy,
    createCapacityReservation_endDateType,
    createCapacityReservation_dryRun,
    createCapacityReservation_instanceType,
    createCapacityReservation_instancePlatform,
    createCapacityReservation_instanceCount,

    -- * Destructuring the Response
    CreateCapacityReservationResponse (..),
    newCreateCapacityReservationResponse,

    -- * Response Lenses
    createCapacityReservationResponse_capacityReservation,
    createCapacityReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCapacityReservation' smart constructor.
data CreateCapacityReservation = CreateCapacityReservation'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone in which to create the Capacity
    -- Reservation.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost on which to create the
    -- Capacity Reservation.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation expires. When a
    -- Capacity Reservation expires, the reserved capacity is released and you
    -- can no longer launch instances into it. The Capacity Reservation\'s
    -- state changes to @expired@ when it reaches its end date and time.
    --
    -- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
    -- @EndDate@ if @EndDateType@ is @unlimited@.
    --
    -- If the @EndDateType@ is @limited@, the Capacity Reservation is cancelled
    -- within an hour from the specified time. For example, if you specify
    -- 5\/31\/2019, 13:30:55, the Capacity Reservation is guaranteed to end
    -- between 13:30:55 and 14:30:55 on 5\/31\/2019.
    endDate :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether the Capacity Reservation supports instances with
    -- temporary, block-level storage.
    ephemeralStorage :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the type of instance launches that the Capacity Reservation
    -- accepts. The options include:
    --
    -- -   @open@ - The Capacity Reservation automatically matches all
    --     instances that have matching attributes (instance type, platform,
    --     and Availability Zone). Instances that have matching attributes run
    --     in the Capacity Reservation automatically without specifying any
    --     additional parameters.
    --
    -- -   @targeted@ - The Capacity Reservation only accepts instances that
    --     have matching attributes (instance type, platform, and Availability
    --     Zone), and explicitly target the Capacity Reservation. This ensures
    --     that only permitted instances can use the reserved capacity.
    --
    -- Default: @open@
    instanceMatchCriteria :: Prelude.Maybe InstanceMatchCriteria,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized
    -- instances. This optimization provides dedicated throughput to Amazon EBS
    -- and an optimized configuration stack to provide optimal I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the Capacity Reservation during launch.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Availability Zone in which to create the Capacity Reservation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity
    -- Reservation can have one of the following tenancy settings:
    --
    -- -   @default@ - The Capacity Reservation is created on hardware that is
    --     shared with other Amazon Web Services accounts.
    --
    -- -   @dedicated@ - The Capacity Reservation is created on single-tenant
    --     hardware that is dedicated to a single Amazon Web Services account.
    tenancy :: Prelude.Maybe CapacityReservationTenancy,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity
    -- Reservation can have one of the following end types:
    --
    -- -   @unlimited@ - The Capacity Reservation remains active until you
    --     explicitly cancel it. Do not provide an @EndDate@ if the
    --     @EndDateType@ is @unlimited@.
    --
    -- -   @limited@ - The Capacity Reservation expires automatically at a
    --     specified date and time. You must provide an @EndDate@ value if the
    --     @EndDateType@ value is @limited@.
    endDateType :: Prelude.Maybe EndDateType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for which to reserve capacity. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Prelude.Text,
    -- | The type of operating system for which to reserve capacity.
    instancePlatform :: CapacityReservationInstancePlatform,
    -- | The number of instances for which to reserve capacity.
    --
    -- Valid range: 1 - 1000
    instanceCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createCapacityReservation_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
--
-- 'availabilityZoneId', 'createCapacityReservation_availabilityZoneId' - The ID of the Availability Zone in which to create the Capacity
-- Reservation.
--
-- 'outpostArn', 'createCapacityReservation_outpostArn' - The Amazon Resource Name (ARN) of the Outpost on which to create the
-- Capacity Reservation.
--
-- 'endDate', 'createCapacityReservation_endDate' - The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
-- @EndDate@ if @EndDateType@ is @unlimited@.
--
-- If the @EndDateType@ is @limited@, the Capacity Reservation is cancelled
-- within an hour from the specified time. For example, if you specify
-- 5\/31\/2019, 13:30:55, the Capacity Reservation is guaranteed to end
-- between 13:30:55 and 14:30:55 on 5\/31\/2019.
--
-- 'ephemeralStorage', 'createCapacityReservation_ephemeralStorage' - Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
--
-- 'instanceMatchCriteria', 'createCapacityReservation_instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation
-- accepts. The options include:
--
-- -   @open@ - The Capacity Reservation automatically matches all
--     instances that have matching attributes (instance type, platform,
--     and Availability Zone). Instances that have matching attributes run
--     in the Capacity Reservation automatically without specifying any
--     additional parameters.
--
-- -   @targeted@ - The Capacity Reservation only accepts instances that
--     have matching attributes (instance type, platform, and Availability
--     Zone), and explicitly target the Capacity Reservation. This ensures
--     that only permitted instances can use the reserved capacity.
--
-- Default: @open@
--
-- 'ebsOptimized', 'createCapacityReservation_ebsOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
--
-- 'tagSpecifications', 'createCapacityReservation_tagSpecifications' - The tags to apply to the Capacity Reservation during launch.
--
-- 'availabilityZone', 'createCapacityReservation_availabilityZone' - The Availability Zone in which to create the Capacity Reservation.
--
-- 'tenancy', 'createCapacityReservation_tenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
--
-- 'endDateType', 'createCapacityReservation_endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it. Do not provide an @EndDate@ if the
--     @EndDateType@ is @unlimited@.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time. You must provide an @EndDate@ value if the
--     @EndDateType@ value is @limited@.
--
-- 'dryRun', 'createCapacityReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceType', 'createCapacityReservation_instanceType' - The instance type for which to reserve capacity. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'instancePlatform', 'createCapacityReservation_instancePlatform' - The type of operating system for which to reserve capacity.
--
-- 'instanceCount', 'createCapacityReservation_instanceCount' - The number of instances for which to reserve capacity.
--
-- Valid range: 1 - 1000
newCreateCapacityReservation ::
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'instancePlatform'
  CapacityReservationInstancePlatform ->
  -- | 'instanceCount'
  Prelude.Int ->
  CreateCapacityReservation
newCreateCapacityReservation
  pInstanceType_
  pInstancePlatform_
  pInstanceCount_ =
    CreateCapacityReservation'
      { clientToken =
          Prelude.Nothing,
        availabilityZoneId = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        endDate = Prelude.Nothing,
        ephemeralStorage = Prelude.Nothing,
        instanceMatchCriteria = Prelude.Nothing,
        ebsOptimized = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        tenancy = Prelude.Nothing,
        endDateType = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        instanceType = pInstanceType_,
        instancePlatform = pInstancePlatform_,
        instanceCount = pInstanceCount_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
createCapacityReservation_clientToken :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Text)
createCapacityReservation_clientToken = Lens.lens (\CreateCapacityReservation' {clientToken} -> clientToken) (\s@CreateCapacityReservation' {} a -> s {clientToken = a} :: CreateCapacityReservation)

-- | The ID of the Availability Zone in which to create the Capacity
-- Reservation.
createCapacityReservation_availabilityZoneId :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Text)
createCapacityReservation_availabilityZoneId = Lens.lens (\CreateCapacityReservation' {availabilityZoneId} -> availabilityZoneId) (\s@CreateCapacityReservation' {} a -> s {availabilityZoneId = a} :: CreateCapacityReservation)

-- | The Amazon Resource Name (ARN) of the Outpost on which to create the
-- Capacity Reservation.
createCapacityReservation_outpostArn :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Text)
createCapacityReservation_outpostArn = Lens.lens (\CreateCapacityReservation' {outpostArn} -> outpostArn) (\s@CreateCapacityReservation' {} a -> s {outpostArn = a} :: CreateCapacityReservation)

-- | The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
-- @EndDate@ if @EndDateType@ is @unlimited@.
--
-- If the @EndDateType@ is @limited@, the Capacity Reservation is cancelled
-- within an hour from the specified time. For example, if you specify
-- 5\/31\/2019, 13:30:55, the Capacity Reservation is guaranteed to end
-- between 13:30:55 and 14:30:55 on 5\/31\/2019.
createCapacityReservation_endDate :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.UTCTime)
createCapacityReservation_endDate = Lens.lens (\CreateCapacityReservation' {endDate} -> endDate) (\s@CreateCapacityReservation' {} a -> s {endDate = a} :: CreateCapacityReservation) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
createCapacityReservation_ephemeralStorage :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Bool)
createCapacityReservation_ephemeralStorage = Lens.lens (\CreateCapacityReservation' {ephemeralStorage} -> ephemeralStorage) (\s@CreateCapacityReservation' {} a -> s {ephemeralStorage = a} :: CreateCapacityReservation)

-- | Indicates the type of instance launches that the Capacity Reservation
-- accepts. The options include:
--
-- -   @open@ - The Capacity Reservation automatically matches all
--     instances that have matching attributes (instance type, platform,
--     and Availability Zone). Instances that have matching attributes run
--     in the Capacity Reservation automatically without specifying any
--     additional parameters.
--
-- -   @targeted@ - The Capacity Reservation only accepts instances that
--     have matching attributes (instance type, platform, and Availability
--     Zone), and explicitly target the Capacity Reservation. This ensures
--     that only permitted instances can use the reserved capacity.
--
-- Default: @open@
createCapacityReservation_instanceMatchCriteria :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe InstanceMatchCriteria)
createCapacityReservation_instanceMatchCriteria = Lens.lens (\CreateCapacityReservation' {instanceMatchCriteria} -> instanceMatchCriteria) (\s@CreateCapacityReservation' {} a -> s {instanceMatchCriteria = a} :: CreateCapacityReservation)

-- | Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
createCapacityReservation_ebsOptimized :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Bool)
createCapacityReservation_ebsOptimized = Lens.lens (\CreateCapacityReservation' {ebsOptimized} -> ebsOptimized) (\s@CreateCapacityReservation' {} a -> s {ebsOptimized = a} :: CreateCapacityReservation)

-- | The tags to apply to the Capacity Reservation during launch.
createCapacityReservation_tagSpecifications :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe [TagSpecification])
createCapacityReservation_tagSpecifications = Lens.lens (\CreateCapacityReservation' {tagSpecifications} -> tagSpecifications) (\s@CreateCapacityReservation' {} a -> s {tagSpecifications = a} :: CreateCapacityReservation) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone in which to create the Capacity Reservation.
createCapacityReservation_availabilityZone :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Text)
createCapacityReservation_availabilityZone = Lens.lens (\CreateCapacityReservation' {availabilityZone} -> availabilityZone) (\s@CreateCapacityReservation' {} a -> s {availabilityZone = a} :: CreateCapacityReservation)

-- | Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
createCapacityReservation_tenancy :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe CapacityReservationTenancy)
createCapacityReservation_tenancy = Lens.lens (\CreateCapacityReservation' {tenancy} -> tenancy) (\s@CreateCapacityReservation' {} a -> s {tenancy = a} :: CreateCapacityReservation)

-- | Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it. Do not provide an @EndDate@ if the
--     @EndDateType@ is @unlimited@.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time. You must provide an @EndDate@ value if the
--     @EndDateType@ value is @limited@.
createCapacityReservation_endDateType :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe EndDateType)
createCapacityReservation_endDateType = Lens.lens (\CreateCapacityReservation' {endDateType} -> endDateType) (\s@CreateCapacityReservation' {} a -> s {endDateType = a} :: CreateCapacityReservation)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCapacityReservation_dryRun :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe Prelude.Bool)
createCapacityReservation_dryRun = Lens.lens (\CreateCapacityReservation' {dryRun} -> dryRun) (\s@CreateCapacityReservation' {} a -> s {dryRun = a} :: CreateCapacityReservation)

-- | The instance type for which to reserve capacity. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
createCapacityReservation_instanceType :: Lens.Lens' CreateCapacityReservation Prelude.Text
createCapacityReservation_instanceType = Lens.lens (\CreateCapacityReservation' {instanceType} -> instanceType) (\s@CreateCapacityReservation' {} a -> s {instanceType = a} :: CreateCapacityReservation)

-- | The type of operating system for which to reserve capacity.
createCapacityReservation_instancePlatform :: Lens.Lens' CreateCapacityReservation CapacityReservationInstancePlatform
createCapacityReservation_instancePlatform = Lens.lens (\CreateCapacityReservation' {instancePlatform} -> instancePlatform) (\s@CreateCapacityReservation' {} a -> s {instancePlatform = a} :: CreateCapacityReservation)

-- | The number of instances for which to reserve capacity.
--
-- Valid range: 1 - 1000
createCapacityReservation_instanceCount :: Lens.Lens' CreateCapacityReservation Prelude.Int
createCapacityReservation_instanceCount = Lens.lens (\CreateCapacityReservation' {instanceCount} -> instanceCount) (\s@CreateCapacityReservation' {} a -> s {instanceCount = a} :: CreateCapacityReservation)

instance Core.AWSRequest CreateCapacityReservation where
  type
    AWSResponse CreateCapacityReservation =
      CreateCapacityReservationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCapacityReservationResponse'
            Prelude.<$> (x Core..@? "capacityReservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCapacityReservation

instance Prelude.NFData CreateCapacityReservation

instance Core.ToHeaders CreateCapacityReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCapacityReservation where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCapacityReservation where
  toQuery CreateCapacityReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCapacityReservation" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Core.=: clientToken,
        "AvailabilityZoneId" Core.=: availabilityZoneId,
        "OutpostArn" Core.=: outpostArn,
        "EndDate" Core.=: endDate,
        "EphemeralStorage" Core.=: ephemeralStorage,
        "InstanceMatchCriteria"
          Core.=: instanceMatchCriteria,
        "EbsOptimized" Core.=: ebsOptimized,
        Core.toQuery
          ( Core.toQueryList "TagSpecifications"
              Prelude.<$> tagSpecifications
          ),
        "AvailabilityZone" Core.=: availabilityZone,
        "Tenancy" Core.=: tenancy,
        "EndDateType" Core.=: endDateType,
        "DryRun" Core.=: dryRun,
        "InstanceType" Core.=: instanceType,
        "InstancePlatform" Core.=: instancePlatform,
        "InstanceCount" Core.=: instanceCount
      ]

-- | /See:/ 'newCreateCapacityReservationResponse' smart constructor.
data CreateCapacityReservationResponse = CreateCapacityReservationResponse'
  { -- | Information about the Capacity Reservation.
    capacityReservation :: Prelude.Maybe CapacityReservation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservation', 'createCapacityReservationResponse_capacityReservation' - Information about the Capacity Reservation.
--
-- 'httpStatus', 'createCapacityReservationResponse_httpStatus' - The response's http status code.
newCreateCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCapacityReservationResponse
newCreateCapacityReservationResponse pHttpStatus_ =
  CreateCapacityReservationResponse'
    { capacityReservation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Capacity Reservation.
createCapacityReservationResponse_capacityReservation :: Lens.Lens' CreateCapacityReservationResponse (Prelude.Maybe CapacityReservation)
createCapacityReservationResponse_capacityReservation = Lens.lens (\CreateCapacityReservationResponse' {capacityReservation} -> capacityReservation) (\s@CreateCapacityReservationResponse' {} a -> s {capacityReservation = a} :: CreateCapacityReservationResponse)

-- | The response's http status code.
createCapacityReservationResponse_httpStatus :: Lens.Lens' CreateCapacityReservationResponse Prelude.Int
createCapacityReservationResponse_httpStatus = Lens.lens (\CreateCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@CreateCapacityReservationResponse' {} a -> s {httpStatus = a} :: CreateCapacityReservationResponse)

instance
  Prelude.NFData
    CreateCapacityReservationResponse

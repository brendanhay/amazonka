{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.CapacityReservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityAllocation
import Amazonka.EC2.Types.CapacityReservationInstancePlatform
import Amazonka.EC2.Types.CapacityReservationState
import Amazonka.EC2.Types.CapacityReservationTenancy
import Amazonka.EC2.Types.EndDateType
import Amazonka.EC2.Types.InstanceMatchCriteria
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Capacity Reservation.
--
-- /See:/ 'newCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { -- | Indicates whether the Capacity Reservation supports EBS-optimized
    -- instances. This optimization provides dedicated throughput to Amazon EBS
    -- and an optimized configuration stack to provide optimal I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | Any tags assigned to the Capacity Reservation.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Capacity Reservation Fleet to which the Capacity
    -- Reservation belongs. Only valid for Capacity Reservations that were
    -- created by a Capacity Reservation Fleet.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
    -- | Information about instance capacity usage.
    capacityAllocations :: Prelude.Maybe [CapacityAllocation],
    -- | /Deprecated./
    ephemeralStorage :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the cluster placement group in which
    -- the Capacity Reservation was created. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/cr-cpg.html Capacity Reservations for cluster placement groups>
    -- in the /Amazon EC2 User Guide/.
    placementGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost on which the Capacity
    -- Reservation was created.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the Capacity
    -- Reservation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The total number of instances for which the Capacity Reservation
    -- reserves capacity.
    totalInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The date and time at which the Capacity Reservation expires. When a
    -- Capacity Reservation expires, the reserved capacity is released and you
    -- can no longer launch instances into it. The Capacity Reservation\'s
    -- state changes to @expired@ when it reaches its end date and time.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | The current state of the Capacity Reservation. A Capacity Reservation
    -- can be in one of the following states:
    --
    -- -   @active@ - The Capacity Reservation is active and the capacity is
    --     available for your use.
    --
    -- -   @expired@ - The Capacity Reservation expired automatically at the
    --     date and time specified in your request. The reserved capacity is no
    --     longer available for your use.
    --
    -- -   @cancelled@ - The Capacity Reservation was cancelled. The reserved
    --     capacity is no longer available for your use.
    --
    -- -   @pending@ - The Capacity Reservation request was successful but the
    --     capacity provisioning is still pending.
    --
    -- -   @failed@ - The Capacity Reservation request has failed. A request
    --     might fail due to invalid request parameters, capacity constraints,
    --     or instance limit constraints. Failed requests are retained for 60
    --     minutes.
    state :: Prelude.Maybe CapacityReservationState,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the capacity is reserved.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation.
    capacityReservationArn :: Prelude.Maybe Prelude.Text,
    -- | The type of instance for which the Capacity Reservation reserves
    -- capacity.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The type of operating system for which the Capacity Reservation reserves
    -- capacity.
    instancePlatform :: Prelude.Maybe CapacityReservationInstancePlatform,
    -- | The date and time at which the Capacity Reservation was started.
    startDate :: Prelude.Maybe Data.ISO8601,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The remaining capacity. Indicates the number of instances that can be
    -- launched in the Capacity Reservation.
    availableInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates the type of instance launches that the Capacity Reservation
    -- accepts. The options include:
    --
    -- -   @open@ - The Capacity Reservation accepts all instances that have
    --     matching attributes (instance type, platform, and Availability
    --     Zone). Instances that have matching attributes launch into the
    --     Capacity Reservation automatically without specifying any additional
    --     parameters.
    --
    -- -   @targeted@ - The Capacity Reservation only accepts instances that
    --     have matching attributes (instance type, platform, and Availability
    --     Zone), and explicitly target the Capacity Reservation. This ensures
    --     that only permitted instances can use the reserved capacity.
    instanceMatchCriteria :: Prelude.Maybe InstanceMatchCriteria,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity
    -- Reservation can have one of the following end types:
    --
    -- -   @unlimited@ - The Capacity Reservation remains active until you
    --     explicitly cancel it.
    --
    -- -   @limited@ - The Capacity Reservation expires automatically at a
    --     specified date and time.
    endDateType :: Prelude.Maybe EndDateType,
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity
    -- Reservation can have one of the following tenancy settings:
    --
    -- -   @default@ - The Capacity Reservation is created on hardware that is
    --     shared with other Amazon Web Services accounts.
    --
    -- -   @dedicated@ - The Capacity Reservation is created on single-tenant
    --     hardware that is dedicated to a single Amazon Web Services account.
    tenancy :: Prelude.Maybe CapacityReservationTenancy,
    -- | The Availability Zone ID of the Capacity Reservation.
    availabilityZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'capacityReservation_ebsOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
--
-- 'tags', 'capacityReservation_tags' - Any tags assigned to the Capacity Reservation.
--
-- 'capacityReservationFleetId', 'capacityReservation_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet to which the Capacity
-- Reservation belongs. Only valid for Capacity Reservations that were
-- created by a Capacity Reservation Fleet.
--
-- 'capacityAllocations', 'capacityReservation_capacityAllocations' - Information about instance capacity usage.
--
-- 'ephemeralStorage', 'capacityReservation_ephemeralStorage' - /Deprecated./
--
-- 'placementGroupArn', 'capacityReservation_placementGroupArn' - The Amazon Resource Name (ARN) of the cluster placement group in which
-- the Capacity Reservation was created. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/cr-cpg.html Capacity Reservations for cluster placement groups>
-- in the /Amazon EC2 User Guide/.
--
-- 'outpostArn', 'capacityReservation_outpostArn' - The Amazon Resource Name (ARN) of the Outpost on which the Capacity
-- Reservation was created.
--
-- 'ownerId', 'capacityReservation_ownerId' - The ID of the Amazon Web Services account that owns the Capacity
-- Reservation.
--
-- 'totalInstanceCount', 'capacityReservation_totalInstanceCount' - The total number of instances for which the Capacity Reservation
-- reserves capacity.
--
-- 'endDate', 'capacityReservation_endDate' - The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- 'state', 'capacityReservation_state' - The current state of the Capacity Reservation. A Capacity Reservation
-- can be in one of the following states:
--
-- -   @active@ - The Capacity Reservation is active and the capacity is
--     available for your use.
--
-- -   @expired@ - The Capacity Reservation expired automatically at the
--     date and time specified in your request. The reserved capacity is no
--     longer available for your use.
--
-- -   @cancelled@ - The Capacity Reservation was cancelled. The reserved
--     capacity is no longer available for your use.
--
-- -   @pending@ - The Capacity Reservation request was successful but the
--     capacity provisioning is still pending.
--
-- -   @failed@ - The Capacity Reservation request has failed. A request
--     might fail due to invalid request parameters, capacity constraints,
--     or instance limit constraints. Failed requests are retained for 60
--     minutes.
--
-- 'capacityReservationId', 'capacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'availabilityZone', 'capacityReservation_availabilityZone' - The Availability Zone in which the capacity is reserved.
--
-- 'capacityReservationArn', 'capacityReservation_capacityReservationArn' - The Amazon Resource Name (ARN) of the Capacity Reservation.
--
-- 'instanceType', 'capacityReservation_instanceType' - The type of instance for which the Capacity Reservation reserves
-- capacity.
--
-- 'instancePlatform', 'capacityReservation_instancePlatform' - The type of operating system for which the Capacity Reservation reserves
-- capacity.
--
-- 'startDate', 'capacityReservation_startDate' - The date and time at which the Capacity Reservation was started.
--
-- 'createDate', 'capacityReservation_createDate' - The date and time at which the Capacity Reservation was created.
--
-- 'availableInstanceCount', 'capacityReservation_availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
--
-- 'instanceMatchCriteria', 'capacityReservation_instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation
-- accepts. The options include:
--
-- -   @open@ - The Capacity Reservation accepts all instances that have
--     matching attributes (instance type, platform, and Availability
--     Zone). Instances that have matching attributes launch into the
--     Capacity Reservation automatically without specifying any additional
--     parameters.
--
-- -   @targeted@ - The Capacity Reservation only accepts instances that
--     have matching attributes (instance type, platform, and Availability
--     Zone), and explicitly target the Capacity Reservation. This ensures
--     that only permitted instances can use the reserved capacity.
--
-- 'endDateType', 'capacityReservation_endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time.
--
-- 'tenancy', 'capacityReservation_tenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
--
-- 'availabilityZoneId', 'capacityReservation_availabilityZoneId' - The Availability Zone ID of the Capacity Reservation.
newCapacityReservation ::
  CapacityReservation
newCapacityReservation =
  CapacityReservation'
    { ebsOptimized =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      capacityReservationFleetId = Prelude.Nothing,
      capacityAllocations = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      placementGroupArn = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      totalInstanceCount = Prelude.Nothing,
      endDate = Prelude.Nothing,
      state = Prelude.Nothing,
      capacityReservationId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      capacityReservationArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instancePlatform = Prelude.Nothing,
      startDate = Prelude.Nothing,
      createDate = Prelude.Nothing,
      availableInstanceCount = Prelude.Nothing,
      instanceMatchCriteria = Prelude.Nothing,
      endDateType = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing
    }

-- | Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
capacityReservation_ebsOptimized :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Bool)
capacityReservation_ebsOptimized = Lens.lens (\CapacityReservation' {ebsOptimized} -> ebsOptimized) (\s@CapacityReservation' {} a -> s {ebsOptimized = a} :: CapacityReservation)

-- | Any tags assigned to the Capacity Reservation.
capacityReservation_tags :: Lens.Lens' CapacityReservation (Prelude.Maybe [Tag])
capacityReservation_tags = Lens.lens (\CapacityReservation' {tags} -> tags) (\s@CapacityReservation' {} a -> s {tags = a} :: CapacityReservation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Capacity Reservation Fleet to which the Capacity
-- Reservation belongs. Only valid for Capacity Reservations that were
-- created by a Capacity Reservation Fleet.
capacityReservation_capacityReservationFleetId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationFleetId = Lens.lens (\CapacityReservation' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@CapacityReservation' {} a -> s {capacityReservationFleetId = a} :: CapacityReservation)

-- | Information about instance capacity usage.
capacityReservation_capacityAllocations :: Lens.Lens' CapacityReservation (Prelude.Maybe [CapacityAllocation])
capacityReservation_capacityAllocations = Lens.lens (\CapacityReservation' {capacityAllocations} -> capacityAllocations) (\s@CapacityReservation' {} a -> s {capacityAllocations = a} :: CapacityReservation) Prelude.. Lens.mapping Lens.coerced

-- | /Deprecated./
capacityReservation_ephemeralStorage :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Bool)
capacityReservation_ephemeralStorage = Lens.lens (\CapacityReservation' {ephemeralStorage} -> ephemeralStorage) (\s@CapacityReservation' {} a -> s {ephemeralStorage = a} :: CapacityReservation)

-- | The Amazon Resource Name (ARN) of the cluster placement group in which
-- the Capacity Reservation was created. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/cr-cpg.html Capacity Reservations for cluster placement groups>
-- in the /Amazon EC2 User Guide/.
capacityReservation_placementGroupArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_placementGroupArn = Lens.lens (\CapacityReservation' {placementGroupArn} -> placementGroupArn) (\s@CapacityReservation' {} a -> s {placementGroupArn = a} :: CapacityReservation)

-- | The Amazon Resource Name (ARN) of the Outpost on which the Capacity
-- Reservation was created.
capacityReservation_outpostArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_outpostArn = Lens.lens (\CapacityReservation' {outpostArn} -> outpostArn) (\s@CapacityReservation' {} a -> s {outpostArn = a} :: CapacityReservation)

-- | The ID of the Amazon Web Services account that owns the Capacity
-- Reservation.
capacityReservation_ownerId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_ownerId = Lens.lens (\CapacityReservation' {ownerId} -> ownerId) (\s@CapacityReservation' {} a -> s {ownerId = a} :: CapacityReservation)

-- | The total number of instances for which the Capacity Reservation
-- reserves capacity.
capacityReservation_totalInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_totalInstanceCount = Lens.lens (\CapacityReservation' {totalInstanceCount} -> totalInstanceCount) (\s@CapacityReservation' {} a -> s {totalInstanceCount = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
capacityReservation_endDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_endDate = Lens.lens (\CapacityReservation' {endDate} -> endDate) (\s@CapacityReservation' {} a -> s {endDate = a} :: CapacityReservation) Prelude.. Lens.mapping Data._Time

-- | The current state of the Capacity Reservation. A Capacity Reservation
-- can be in one of the following states:
--
-- -   @active@ - The Capacity Reservation is active and the capacity is
--     available for your use.
--
-- -   @expired@ - The Capacity Reservation expired automatically at the
--     date and time specified in your request. The reserved capacity is no
--     longer available for your use.
--
-- -   @cancelled@ - The Capacity Reservation was cancelled. The reserved
--     capacity is no longer available for your use.
--
-- -   @pending@ - The Capacity Reservation request was successful but the
--     capacity provisioning is still pending.
--
-- -   @failed@ - The Capacity Reservation request has failed. A request
--     might fail due to invalid request parameters, capacity constraints,
--     or instance limit constraints. Failed requests are retained for 60
--     minutes.
capacityReservation_state :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationState)
capacityReservation_state = Lens.lens (\CapacityReservation' {state} -> state) (\s@CapacityReservation' {} a -> s {state = a} :: CapacityReservation)

-- | The ID of the Capacity Reservation.
capacityReservation_capacityReservationId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationId = Lens.lens (\CapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservation' {} a -> s {capacityReservationId = a} :: CapacityReservation)

-- | The Availability Zone in which the capacity is reserved.
capacityReservation_availabilityZone :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZone = Lens.lens (\CapacityReservation' {availabilityZone} -> availabilityZone) (\s@CapacityReservation' {} a -> s {availabilityZone = a} :: CapacityReservation)

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
capacityReservation_capacityReservationArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationArn = Lens.lens (\CapacityReservation' {capacityReservationArn} -> capacityReservationArn) (\s@CapacityReservation' {} a -> s {capacityReservationArn = a} :: CapacityReservation)

-- | The type of instance for which the Capacity Reservation reserves
-- capacity.
capacityReservation_instanceType :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_instanceType = Lens.lens (\CapacityReservation' {instanceType} -> instanceType) (\s@CapacityReservation' {} a -> s {instanceType = a} :: CapacityReservation)

-- | The type of operating system for which the Capacity Reservation reserves
-- capacity.
capacityReservation_instancePlatform :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationInstancePlatform)
capacityReservation_instancePlatform = Lens.lens (\CapacityReservation' {instancePlatform} -> instancePlatform) (\s@CapacityReservation' {} a -> s {instancePlatform = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation was started.
capacityReservation_startDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_startDate = Lens.lens (\CapacityReservation' {startDate} -> startDate) (\s@CapacityReservation' {} a -> s {startDate = a} :: CapacityReservation) Prelude.. Lens.mapping Data._Time

-- | The date and time at which the Capacity Reservation was created.
capacityReservation_createDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_createDate = Lens.lens (\CapacityReservation' {createDate} -> createDate) (\s@CapacityReservation' {} a -> s {createDate = a} :: CapacityReservation) Prelude.. Lens.mapping Data._Time

-- | The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
capacityReservation_availableInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_availableInstanceCount = Lens.lens (\CapacityReservation' {availableInstanceCount} -> availableInstanceCount) (\s@CapacityReservation' {} a -> s {availableInstanceCount = a} :: CapacityReservation)

-- | Indicates the type of instance launches that the Capacity Reservation
-- accepts. The options include:
--
-- -   @open@ - The Capacity Reservation accepts all instances that have
--     matching attributes (instance type, platform, and Availability
--     Zone). Instances that have matching attributes launch into the
--     Capacity Reservation automatically without specifying any additional
--     parameters.
--
-- -   @targeted@ - The Capacity Reservation only accepts instances that
--     have matching attributes (instance type, platform, and Availability
--     Zone), and explicitly target the Capacity Reservation. This ensures
--     that only permitted instances can use the reserved capacity.
capacityReservation_instanceMatchCriteria :: Lens.Lens' CapacityReservation (Prelude.Maybe InstanceMatchCriteria)
capacityReservation_instanceMatchCriteria = Lens.lens (\CapacityReservation' {instanceMatchCriteria} -> instanceMatchCriteria) (\s@CapacityReservation' {} a -> s {instanceMatchCriteria = a} :: CapacityReservation)

-- | Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time.
capacityReservation_endDateType :: Lens.Lens' CapacityReservation (Prelude.Maybe EndDateType)
capacityReservation_endDateType = Lens.lens (\CapacityReservation' {endDateType} -> endDateType) (\s@CapacityReservation' {} a -> s {endDateType = a} :: CapacityReservation)

-- | Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
capacityReservation_tenancy :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationTenancy)
capacityReservation_tenancy = Lens.lens (\CapacityReservation' {tenancy} -> tenancy) (\s@CapacityReservation' {} a -> s {tenancy = a} :: CapacityReservation)

-- | The Availability Zone ID of the Capacity Reservation.
capacityReservation_availabilityZoneId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZoneId = Lens.lens (\CapacityReservation' {availabilityZoneId} -> availabilityZoneId) (\s@CapacityReservation' {} a -> s {availabilityZoneId = a} :: CapacityReservation)

instance Data.FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      Prelude.<$> (x Data..@? "ebsOptimized")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "capacityReservationFleetId")
      Prelude.<*> ( x Data..@? "capacityAllocationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ephemeralStorage")
      Prelude.<*> (x Data..@? "placementGroupArn")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "totalInstanceCount")
      Prelude.<*> (x Data..@? "endDate")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "capacityReservationId")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "capacityReservationArn")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "instancePlatform")
      Prelude.<*> (x Data..@? "startDate")
      Prelude.<*> (x Data..@? "createDate")
      Prelude.<*> (x Data..@? "availableInstanceCount")
      Prelude.<*> (x Data..@? "instanceMatchCriteria")
      Prelude.<*> (x Data..@? "endDateType")
      Prelude.<*> (x Data..@? "tenancy")
      Prelude.<*> (x Data..@? "availabilityZoneId")

instance Prelude.Hashable CapacityReservation where
  hashWithSalt _salt CapacityReservation' {..} =
    _salt `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` capacityReservationFleetId
      `Prelude.hashWithSalt` capacityAllocations
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` placementGroupArn
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` totalInstanceCount
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` capacityReservationId
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` capacityReservationArn
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instancePlatform
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` availableInstanceCount
      `Prelude.hashWithSalt` instanceMatchCriteria
      `Prelude.hashWithSalt` endDateType
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` availabilityZoneId

instance Prelude.NFData CapacityReservation where
  rnf CapacityReservation' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf capacityReservationFleetId
      `Prelude.seq` Prelude.rnf capacityAllocations
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf placementGroupArn
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf totalInstanceCount
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf capacityReservationArn
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instancePlatform
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf
        availableInstanceCount
      `Prelude.seq` Prelude.rnf
        instanceMatchCriteria
      `Prelude.seq` Prelude.rnf endDateType
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf
        availabilityZoneId

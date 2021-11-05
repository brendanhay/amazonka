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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservation where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationInstancePlatform
import Amazonka.EC2.Types.CapacityReservationState
import Amazonka.EC2.Types.CapacityReservationTenancy
import Amazonka.EC2.Types.EndDateType
import Amazonka.EC2.Types.InstanceMatchCriteria
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Capacity Reservation.
--
-- /See:/ 'newCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { -- | The ID of the Capacity Reservation Fleet to which the Capacity
    -- Reservation belongs. Only valid for Capacity Reservations that were
    -- created by a Capacity Reservation Fleet.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
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
    -- | The Availability Zone ID of the Capacity Reservation.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) of the Outpost on which the Capacity
    -- Reservation was created.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation expires. When a
    -- Capacity Reservation expires, the reserved capacity is released and you
    -- can no longer launch instances into it. The Capacity Reservation\'s
    -- state changes to @expired@ when it reaches its end date and time.
    endDate :: Prelude.Maybe Core.ISO8601,
    -- | The remaining capacity. Indicates the number of instances that can be
    -- launched in the Capacity Reservation.
    availableInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the Capacity Reservation supports instances with
    -- temporary, block-level storage.
    ephemeralStorage :: Prelude.Maybe Prelude.Bool,
    -- | The type of operating system for which the Capacity Reservation reserves
    -- capacity.
    instancePlatform :: Prelude.Maybe CapacityReservationInstancePlatform,
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
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The type of instance for which the Capacity Reservation reserves
    -- capacity.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized
    -- instances. This optimization provides dedicated throughput to Amazon EBS
    -- and an optimized configuration stack to provide optimal I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services account that owns the Capacity
    -- Reservation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation was started.
    startDate :: Prelude.Maybe Core.ISO8601,
    -- | The Availability Zone in which the capacity is reserved.
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
    -- | The total number of instances for which the Capacity Reservation
    -- reserves capacity.
    totalInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity
    -- Reservation can have one of the following end types:
    --
    -- -   @unlimited@ - The Capacity Reservation remains active until you
    --     explicitly cancel it.
    --
    -- -   @limited@ - The Capacity Reservation expires automatically at a
    --     specified date and time.
    endDateType :: Prelude.Maybe EndDateType,
    -- | Any tags assigned to the Capacity Reservation.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation.
    capacityReservationArn :: Prelude.Maybe Prelude.Text
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
-- 'capacityReservationFleetId', 'capacityReservation_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet to which the Capacity
-- Reservation belongs. Only valid for Capacity Reservations that were
-- created by a Capacity Reservation Fleet.
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
-- 'availabilityZoneId', 'capacityReservation_availabilityZoneId' - The Availability Zone ID of the Capacity Reservation.
--
-- 'createDate', 'capacityReservation_createDate' - The date and time at which the Capacity Reservation was created.
--
-- 'outpostArn', 'capacityReservation_outpostArn' - The Amazon Resource Name (ARN) of the Outpost on which the Capacity
-- Reservation was created.
--
-- 'endDate', 'capacityReservation_endDate' - The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- 'availableInstanceCount', 'capacityReservation_availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
--
-- 'ephemeralStorage', 'capacityReservation_ephemeralStorage' - Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
--
-- 'instancePlatform', 'capacityReservation_instancePlatform' - The type of operating system for which the Capacity Reservation reserves
-- capacity.
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
-- 'capacityReservationId', 'capacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'instanceType', 'capacityReservation_instanceType' - The type of instance for which the Capacity Reservation reserves
-- capacity.
--
-- 'ebsOptimized', 'capacityReservation_ebsOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
--
-- 'ownerId', 'capacityReservation_ownerId' - The ID of the Amazon Web Services account that owns the Capacity
-- Reservation.
--
-- 'startDate', 'capacityReservation_startDate' - The date and time at which the Capacity Reservation was started.
--
-- 'availabilityZone', 'capacityReservation_availabilityZone' - The Availability Zone in which the capacity is reserved.
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
-- 'totalInstanceCount', 'capacityReservation_totalInstanceCount' - The total number of instances for which the Capacity Reservation
-- reserves capacity.
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
-- 'tags', 'capacityReservation_tags' - Any tags assigned to the Capacity Reservation.
--
-- 'capacityReservationArn', 'capacityReservation_capacityReservationArn' - The Amazon Resource Name (ARN) of the Capacity Reservation.
newCapacityReservation ::
  CapacityReservation
newCapacityReservation =
  CapacityReservation'
    { capacityReservationFleetId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      endDate = Prelude.Nothing,
      availableInstanceCount = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      instancePlatform = Prelude.Nothing,
      instanceMatchCriteria = Prelude.Nothing,
      capacityReservationId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      totalInstanceCount = Prelude.Nothing,
      endDateType = Prelude.Nothing,
      tags = Prelude.Nothing,
      capacityReservationArn = Prelude.Nothing
    }

-- | The ID of the Capacity Reservation Fleet to which the Capacity
-- Reservation belongs. Only valid for Capacity Reservations that were
-- created by a Capacity Reservation Fleet.
capacityReservation_capacityReservationFleetId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationFleetId = Lens.lens (\CapacityReservation' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@CapacityReservation' {} a -> s {capacityReservationFleetId = a} :: CapacityReservation)

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

-- | The Availability Zone ID of the Capacity Reservation.
capacityReservation_availabilityZoneId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZoneId = Lens.lens (\CapacityReservation' {availabilityZoneId} -> availabilityZoneId) (\s@CapacityReservation' {} a -> s {availabilityZoneId = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation was created.
capacityReservation_createDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_createDate = Lens.lens (\CapacityReservation' {createDate} -> createDate) (\s@CapacityReservation' {} a -> s {createDate = a} :: CapacityReservation) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the Outpost on which the Capacity
-- Reservation was created.
capacityReservation_outpostArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_outpostArn = Lens.lens (\CapacityReservation' {outpostArn} -> outpostArn) (\s@CapacityReservation' {} a -> s {outpostArn = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
capacityReservation_endDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_endDate = Lens.lens (\CapacityReservation' {endDate} -> endDate) (\s@CapacityReservation' {} a -> s {endDate = a} :: CapacityReservation) Prelude.. Lens.mapping Core._Time

-- | The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
capacityReservation_availableInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_availableInstanceCount = Lens.lens (\CapacityReservation' {availableInstanceCount} -> availableInstanceCount) (\s@CapacityReservation' {} a -> s {availableInstanceCount = a} :: CapacityReservation)

-- | Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
capacityReservation_ephemeralStorage :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Bool)
capacityReservation_ephemeralStorage = Lens.lens (\CapacityReservation' {ephemeralStorage} -> ephemeralStorage) (\s@CapacityReservation' {} a -> s {ephemeralStorage = a} :: CapacityReservation)

-- | The type of operating system for which the Capacity Reservation reserves
-- capacity.
capacityReservation_instancePlatform :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationInstancePlatform)
capacityReservation_instancePlatform = Lens.lens (\CapacityReservation' {instancePlatform} -> instancePlatform) (\s@CapacityReservation' {} a -> s {instancePlatform = a} :: CapacityReservation)

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

-- | The ID of the Capacity Reservation.
capacityReservation_capacityReservationId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationId = Lens.lens (\CapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservation' {} a -> s {capacityReservationId = a} :: CapacityReservation)

-- | The type of instance for which the Capacity Reservation reserves
-- capacity.
capacityReservation_instanceType :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_instanceType = Lens.lens (\CapacityReservation' {instanceType} -> instanceType) (\s@CapacityReservation' {} a -> s {instanceType = a} :: CapacityReservation)

-- | Indicates whether the Capacity Reservation supports EBS-optimized
-- instances. This optimization provides dedicated throughput to Amazon EBS
-- and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS- optimized instance.
capacityReservation_ebsOptimized :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Bool)
capacityReservation_ebsOptimized = Lens.lens (\CapacityReservation' {ebsOptimized} -> ebsOptimized) (\s@CapacityReservation' {} a -> s {ebsOptimized = a} :: CapacityReservation)

-- | The ID of the Amazon Web Services account that owns the Capacity
-- Reservation.
capacityReservation_ownerId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_ownerId = Lens.lens (\CapacityReservation' {ownerId} -> ownerId) (\s@CapacityReservation' {} a -> s {ownerId = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation was started.
capacityReservation_startDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_startDate = Lens.lens (\CapacityReservation' {startDate} -> startDate) (\s@CapacityReservation' {} a -> s {startDate = a} :: CapacityReservation) Prelude.. Lens.mapping Core._Time

-- | The Availability Zone in which the capacity is reserved.
capacityReservation_availabilityZone :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZone = Lens.lens (\CapacityReservation' {availabilityZone} -> availabilityZone) (\s@CapacityReservation' {} a -> s {availabilityZone = a} :: CapacityReservation)

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

-- | The total number of instances for which the Capacity Reservation
-- reserves capacity.
capacityReservation_totalInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_totalInstanceCount = Lens.lens (\CapacityReservation' {totalInstanceCount} -> totalInstanceCount) (\s@CapacityReservation' {} a -> s {totalInstanceCount = a} :: CapacityReservation)

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

-- | Any tags assigned to the Capacity Reservation.
capacityReservation_tags :: Lens.Lens' CapacityReservation (Prelude.Maybe [Tag])
capacityReservation_tags = Lens.lens (\CapacityReservation' {tags} -> tags) (\s@CapacityReservation' {} a -> s {tags = a} :: CapacityReservation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
capacityReservation_capacityReservationArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationArn = Lens.lens (\CapacityReservation' {capacityReservationArn} -> capacityReservationArn) (\s@CapacityReservation' {} a -> s {capacityReservationArn = a} :: CapacityReservation)

instance Core.FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      Prelude.<$> (x Core..@? "capacityReservationFleetId")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "availabilityZoneId")
      Prelude.<*> (x Core..@? "createDate")
      Prelude.<*> (x Core..@? "outpostArn")
      Prelude.<*> (x Core..@? "endDate")
      Prelude.<*> (x Core..@? "availableInstanceCount")
      Prelude.<*> (x Core..@? "ephemeralStorage")
      Prelude.<*> (x Core..@? "instancePlatform")
      Prelude.<*> (x Core..@? "instanceMatchCriteria")
      Prelude.<*> (x Core..@? "capacityReservationId")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "ebsOptimized")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "startDate")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "tenancy")
      Prelude.<*> (x Core..@? "totalInstanceCount")
      Prelude.<*> (x Core..@? "endDateType")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "capacityReservationArn")

instance Prelude.Hashable CapacityReservation

instance Prelude.NFData CapacityReservation

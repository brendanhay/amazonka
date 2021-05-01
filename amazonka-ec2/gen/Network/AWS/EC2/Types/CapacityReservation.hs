{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CapacityReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Capacity Reservation.
--
-- /See:/ 'newCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { -- | The ID of the AWS account that owns the Capacity Reservation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation was started.
    startDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The type of instance for which the Capacity Reservation reserves
    -- capacity.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Capacity Reservation supports EBS-optimized
    -- instances. This optimization provides dedicated throughput to Amazon EBS
    -- and an optimized configuration stack to provide optimal I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS- optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity
    -- Reservation can have one of the following end types:
    --
    -- -   @unlimited@ - The Capacity Reservation remains active until you
    --     explicitly cancel it.
    --
    -- -   @limited@ - The Capacity Reservation expires automatically at a
    --     specified date and time.
    endDateType :: Prelude.Maybe EndDateType,
    -- | The remaining capacity. Indicates the number of instances that can be
    -- launched in the Capacity Reservation.
    availableInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Prelude.Maybe Prelude.ISO8601,
    -- | Indicates the tenancy of the Capacity Reservation. A Capacity
    -- Reservation can have one of the following tenancy settings:
    --
    -- -   @default@ - The Capacity Reservation is created on hardware that is
    --     shared with other AWS accounts.
    --
    -- -   @dedicated@ - The Capacity Reservation is created on single-tenant
    --     hardware that is dedicated to a single AWS account.
    tenancy :: Prelude.Maybe CapacityReservationTenancy,
    -- | The Availability Zone ID of the Capacity Reservation.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
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
    -- | The Availability Zone in which the capacity is reserved.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the Capacity Reservation.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation.
    capacityReservationArn :: Prelude.Maybe Prelude.Text,
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
    -- | Indicates whether the Capacity Reservation supports instances with
    -- temporary, block-level storage.
    ephemeralStorage :: Prelude.Maybe Prelude.Bool,
    -- | The type of operating system for which the Capacity Reservation reserves
    -- capacity.
    instancePlatform :: Prelude.Maybe CapacityReservationInstancePlatform,
    -- | The date and time at which the Capacity Reservation expires. When a
    -- Capacity Reservation expires, the reserved capacity is released and you
    -- can no longer launch instances into it. The Capacity Reservation\'s
    -- state changes to @expired@ when it reaches its end date and time.
    endDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The total number of instances for which the Capacity Reservation
    -- reserves capacity.
    totalInstanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'capacityReservation_ownerId' - The ID of the AWS account that owns the Capacity Reservation.
--
-- 'startDate', 'capacityReservation_startDate' - The date and time at which the Capacity Reservation was started.
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
-- 'endDateType', 'capacityReservation_endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time.
--
-- 'availableInstanceCount', 'capacityReservation_availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
--
-- 'createDate', 'capacityReservation_createDate' - The date and time at which the Capacity Reservation was created.
--
-- 'tenancy', 'capacityReservation_tenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other AWS accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single AWS account.
--
-- 'availabilityZoneId', 'capacityReservation_availabilityZoneId' - The Availability Zone ID of the Capacity Reservation.
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
-- 'availabilityZone', 'capacityReservation_availabilityZone' - The Availability Zone in which the capacity is reserved.
--
-- 'capacityReservationId', 'capacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'tags', 'capacityReservation_tags' - Any tags assigned to the Capacity Reservation.
--
-- 'capacityReservationArn', 'capacityReservation_capacityReservationArn' - The Amazon Resource Name (ARN) of the Capacity Reservation.
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
-- 'ephemeralStorage', 'capacityReservation_ephemeralStorage' - Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
--
-- 'instancePlatform', 'capacityReservation_instancePlatform' - The type of operating system for which the Capacity Reservation reserves
-- capacity.
--
-- 'endDate', 'capacityReservation_endDate' - The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- 'totalInstanceCount', 'capacityReservation_totalInstanceCount' - The total number of instances for which the Capacity Reservation
-- reserves capacity.
newCapacityReservation ::
  CapacityReservation
newCapacityReservation =
  CapacityReservation'
    { ownerId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      endDateType = Prelude.Nothing,
      availableInstanceCount = Prelude.Nothing,
      createDate = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      capacityReservationId = Prelude.Nothing,
      tags = Prelude.Nothing,
      capacityReservationArn = Prelude.Nothing,
      instanceMatchCriteria = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      instancePlatform = Prelude.Nothing,
      endDate = Prelude.Nothing,
      totalInstanceCount = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the Capacity Reservation.
capacityReservation_ownerId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_ownerId = Lens.lens (\CapacityReservation' {ownerId} -> ownerId) (\s@CapacityReservation' {} a -> s {ownerId = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation was started.
capacityReservation_startDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_startDate = Lens.lens (\CapacityReservation' {startDate} -> startDate) (\s@CapacityReservation' {} a -> s {startDate = a} :: CapacityReservation) Prelude.. Lens.mapping Prelude._Time

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

-- | The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
capacityReservation_availableInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_availableInstanceCount = Lens.lens (\CapacityReservation' {availableInstanceCount} -> availableInstanceCount) (\s@CapacityReservation' {} a -> s {availableInstanceCount = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation was created.
capacityReservation_createDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_createDate = Lens.lens (\CapacityReservation' {createDate} -> createDate) (\s@CapacityReservation' {} a -> s {createDate = a} :: CapacityReservation) Prelude.. Lens.mapping Prelude._Time

-- | Indicates the tenancy of the Capacity Reservation. A Capacity
-- Reservation can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation is created on hardware that is
--     shared with other AWS accounts.
--
-- -   @dedicated@ - The Capacity Reservation is created on single-tenant
--     hardware that is dedicated to a single AWS account.
capacityReservation_tenancy :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationTenancy)
capacityReservation_tenancy = Lens.lens (\CapacityReservation' {tenancy} -> tenancy) (\s@CapacityReservation' {} a -> s {tenancy = a} :: CapacityReservation)

-- | The Availability Zone ID of the Capacity Reservation.
capacityReservation_availabilityZoneId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZoneId = Lens.lens (\CapacityReservation' {availabilityZoneId} -> availabilityZoneId) (\s@CapacityReservation' {} a -> s {availabilityZoneId = a} :: CapacityReservation)

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

-- | The Availability Zone in which the capacity is reserved.
capacityReservation_availabilityZone :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_availabilityZone = Lens.lens (\CapacityReservation' {availabilityZone} -> availabilityZone) (\s@CapacityReservation' {} a -> s {availabilityZone = a} :: CapacityReservation)

-- | The ID of the Capacity Reservation.
capacityReservation_capacityReservationId :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationId = Lens.lens (\CapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservation' {} a -> s {capacityReservationId = a} :: CapacityReservation)

-- | Any tags assigned to the Capacity Reservation.
capacityReservation_tags :: Lens.Lens' CapacityReservation (Prelude.Maybe [Tag])
capacityReservation_tags = Lens.lens (\CapacityReservation' {tags} -> tags) (\s@CapacityReservation' {} a -> s {tags = a} :: CapacityReservation) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the Capacity Reservation.
capacityReservation_capacityReservationArn :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Text)
capacityReservation_capacityReservationArn = Lens.lens (\CapacityReservation' {capacityReservationArn} -> capacityReservationArn) (\s@CapacityReservation' {} a -> s {capacityReservationArn = a} :: CapacityReservation)

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

-- | Indicates whether the Capacity Reservation supports instances with
-- temporary, block-level storage.
capacityReservation_ephemeralStorage :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Bool)
capacityReservation_ephemeralStorage = Lens.lens (\CapacityReservation' {ephemeralStorage} -> ephemeralStorage) (\s@CapacityReservation' {} a -> s {ephemeralStorage = a} :: CapacityReservation)

-- | The type of operating system for which the Capacity Reservation reserves
-- capacity.
capacityReservation_instancePlatform :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityReservationInstancePlatform)
capacityReservation_instancePlatform = Lens.lens (\CapacityReservation' {instancePlatform} -> instancePlatform) (\s@CapacityReservation' {} a -> s {instancePlatform = a} :: CapacityReservation)

-- | The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
capacityReservation_endDate :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_endDate = Lens.lens (\CapacityReservation' {endDate} -> endDate) (\s@CapacityReservation' {} a -> s {endDate = a} :: CapacityReservation) Prelude.. Lens.mapping Prelude._Time

-- | The total number of instances for which the Capacity Reservation
-- reserves capacity.
capacityReservation_totalInstanceCount :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.Int)
capacityReservation_totalInstanceCount = Lens.lens (\CapacityReservation' {totalInstanceCount} -> totalInstanceCount) (\s@CapacityReservation' {} a -> s {totalInstanceCount = a} :: CapacityReservation)

instance Prelude.FromXML CapacityReservation where
  parseXML x =
    CapacityReservation'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "startDate")
      Prelude.<*> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "ebsOptimized")
      Prelude.<*> (x Prelude..@? "endDateType")
      Prelude.<*> (x Prelude..@? "availableInstanceCount")
      Prelude.<*> (x Prelude..@? "createDate")
      Prelude.<*> (x Prelude..@? "tenancy")
      Prelude.<*> (x Prelude..@? "availabilityZoneId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "capacityReservationId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "capacityReservationArn")
      Prelude.<*> (x Prelude..@? "instanceMatchCriteria")
      Prelude.<*> (x Prelude..@? "ephemeralStorage")
      Prelude.<*> (x Prelude..@? "instancePlatform")
      Prelude.<*> (x Prelude..@? "endDate")
      Prelude.<*> (x Prelude..@? "totalInstanceCount")

instance Prelude.Hashable CapacityReservation

instance Prelude.NFData CapacityReservation

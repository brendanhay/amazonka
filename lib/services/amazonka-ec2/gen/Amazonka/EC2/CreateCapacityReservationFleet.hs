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
-- Module      : Amazonka.EC2.CreateCapacityReservationFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Capacity Reservation Fleet. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/work-with-cr-fleets.html#create-crfleet Create a Capacity Reservation Fleet>
-- in the Amazon EC2 User Guide.
module Amazonka.EC2.CreateCapacityReservationFleet
  ( -- * Creating a Request
    CreateCapacityReservationFleet (..),
    newCreateCapacityReservationFleet,

    -- * Request Lenses
    createCapacityReservationFleet_allocationStrategy,
    createCapacityReservationFleet_clientToken,
    createCapacityReservationFleet_dryRun,
    createCapacityReservationFleet_endDate,
    createCapacityReservationFleet_instanceMatchCriteria,
    createCapacityReservationFleet_tagSpecifications,
    createCapacityReservationFleet_tenancy,
    createCapacityReservationFleet_instanceTypeSpecifications,
    createCapacityReservationFleet_totalTargetCapacity,

    -- * Destructuring the Response
    CreateCapacityReservationFleetResponse (..),
    newCreateCapacityReservationFleetResponse,

    -- * Response Lenses
    createCapacityReservationFleetResponse_allocationStrategy,
    createCapacityReservationFleetResponse_capacityReservationFleetId,
    createCapacityReservationFleetResponse_createTime,
    createCapacityReservationFleetResponse_endDate,
    createCapacityReservationFleetResponse_fleetCapacityReservations,
    createCapacityReservationFleetResponse_instanceMatchCriteria,
    createCapacityReservationFleetResponse_state,
    createCapacityReservationFleetResponse_tags,
    createCapacityReservationFleetResponse_tenancy,
    createCapacityReservationFleetResponse_totalFulfilledCapacity,
    createCapacityReservationFleetResponse_totalTargetCapacity,
    createCapacityReservationFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCapacityReservationFleet' smart constructor.
data CreateCapacityReservationFleet = CreateCapacityReservationFleet'
  { -- | The strategy used by the Capacity Reservation Fleet to determine which
    -- of the specified instance types to use. Currently, only the
    -- @prioritized@ allocation strategy is supported. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
    -- in the Amazon EC2 User Guide.
    --
    -- Valid values: @prioritized@
    allocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The date and time at which the Capacity Reservation Fleet expires. When
    -- the Capacity Reservation Fleet expires, its state changes to @expired@
    -- and all of the Capacity Reservations in the Fleet expire.
    --
    -- The Capacity Reservation Fleet expires within an hour after the
    -- specified time. For example, if you specify @5\/31\/2019@, @13:30:55@,
    -- the Capacity Reservation Fleet is guaranteed to expire between
    -- @13:30:55@ and @14:30:55@ on @5\/31\/2019@.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the type of instance launches that the Capacity Reservation
    -- Fleet accepts. All Capacity Reservations in the Fleet inherit this
    -- instance matching criteria.
    --
    -- Currently, Capacity Reservation Fleets support @open@ instance matching
    -- criteria only. This means that instances that have matching attributes
    -- (instance type, platform, and Availability Zone) run in the Capacity
    -- Reservations automatically. Instances do not need to explicitly target a
    -- Capacity Reservation Fleet to use its reserved capacity.
    instanceMatchCriteria :: Prelude.Maybe FleetInstanceMatchCriteria,
    -- | The tags to assign to the Capacity Reservation Fleet. The tags are
    -- automatically assigned to the Capacity Reservations in the Fleet.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Indicates the tenancy of the Capacity Reservation Fleet. All Capacity
    -- Reservations in the Fleet inherit this tenancy. The Capacity Reservation
    -- Fleet can have one of the following tenancy settings:
    --
    -- -   @default@ - The Capacity Reservation Fleet is created on hardware
    --     that is shared with other Amazon Web Services accounts.
    --
    -- -   @dedicated@ - The Capacity Reservations are created on single-tenant
    --     hardware that is dedicated to a single Amazon Web Services account.
    tenancy :: Prelude.Maybe FleetCapacityReservationTenancy,
    -- | Information about the instance types for which to reserve the capacity.
    instanceTypeSpecifications :: [ReservationFleetInstanceSpecification],
    -- | The total number of capacity units to be reserved by the Capacity
    -- Reservation Fleet. This value, together with the instance type weights
    -- that you assign to each instance type used by the Fleet determine the
    -- number of instances for which the Fleet reserves capacity. Both values
    -- are based on units that make sense for your workload. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
    -- in the Amazon EC2 User Guide.
    totalTargetCapacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservationFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'createCapacityReservationFleet_allocationStrategy' - The strategy used by the Capacity Reservation Fleet to determine which
-- of the specified instance types to use. Currently, only the
-- @prioritized@ allocation strategy is supported. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
-- in the Amazon EC2 User Guide.
--
-- Valid values: @prioritized@
--
-- 'clientToken', 'createCapacityReservationFleet_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
--
-- 'dryRun', 'createCapacityReservationFleet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'endDate', 'createCapacityReservationFleet_endDate' - The date and time at which the Capacity Reservation Fleet expires. When
-- the Capacity Reservation Fleet expires, its state changes to @expired@
-- and all of the Capacity Reservations in the Fleet expire.
--
-- The Capacity Reservation Fleet expires within an hour after the
-- specified time. For example, if you specify @5\/31\/2019@, @13:30:55@,
-- the Capacity Reservation Fleet is guaranteed to expire between
-- @13:30:55@ and @14:30:55@ on @5\/31\/2019@.
--
-- 'instanceMatchCriteria', 'createCapacityReservationFleet_instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation
-- Fleet accepts. All Capacity Reservations in the Fleet inherit this
-- instance matching criteria.
--
-- Currently, Capacity Reservation Fleets support @open@ instance matching
-- criteria only. This means that instances that have matching attributes
-- (instance type, platform, and Availability Zone) run in the Capacity
-- Reservations automatically. Instances do not need to explicitly target a
-- Capacity Reservation Fleet to use its reserved capacity.
--
-- 'tagSpecifications', 'createCapacityReservationFleet_tagSpecifications' - The tags to assign to the Capacity Reservation Fleet. The tags are
-- automatically assigned to the Capacity Reservations in the Fleet.
--
-- 'tenancy', 'createCapacityReservationFleet_tenancy' - Indicates the tenancy of the Capacity Reservation Fleet. All Capacity
-- Reservations in the Fleet inherit this tenancy. The Capacity Reservation
-- Fleet can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation Fleet is created on hardware
--     that is shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservations are created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
--
-- 'instanceTypeSpecifications', 'createCapacityReservationFleet_instanceTypeSpecifications' - Information about the instance types for which to reserve the capacity.
--
-- 'totalTargetCapacity', 'createCapacityReservationFleet_totalTargetCapacity' - The total number of capacity units to be reserved by the Capacity
-- Reservation Fleet. This value, together with the instance type weights
-- that you assign to each instance type used by the Fleet determine the
-- number of instances for which the Fleet reserves capacity. Both values
-- are based on units that make sense for your workload. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
newCreateCapacityReservationFleet ::
  -- | 'totalTargetCapacity'
  Prelude.Int ->
  CreateCapacityReservationFleet
newCreateCapacityReservationFleet
  pTotalTargetCapacity_ =
    CreateCapacityReservationFleet'
      { allocationStrategy =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        endDate = Prelude.Nothing,
        instanceMatchCriteria = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        tenancy = Prelude.Nothing,
        instanceTypeSpecifications = Prelude.mempty,
        totalTargetCapacity = pTotalTargetCapacity_
      }

-- | The strategy used by the Capacity Reservation Fleet to determine which
-- of the specified instance types to use. Currently, only the
-- @prioritized@ allocation strategy is supported. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
-- in the Amazon EC2 User Guide.
--
-- Valid values: @prioritized@
createCapacityReservationFleet_allocationStrategy :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe Prelude.Text)
createCapacityReservationFleet_allocationStrategy = Lens.lens (\CreateCapacityReservationFleet' {allocationStrategy} -> allocationStrategy) (\s@CreateCapacityReservationFleet' {} a -> s {allocationStrategy = a} :: CreateCapacityReservationFleet)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensure Idempotency>.
createCapacityReservationFleet_clientToken :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe Prelude.Text)
createCapacityReservationFleet_clientToken = Lens.lens (\CreateCapacityReservationFleet' {clientToken} -> clientToken) (\s@CreateCapacityReservationFleet' {} a -> s {clientToken = a} :: CreateCapacityReservationFleet)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCapacityReservationFleet_dryRun :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe Prelude.Bool)
createCapacityReservationFleet_dryRun = Lens.lens (\CreateCapacityReservationFleet' {dryRun} -> dryRun) (\s@CreateCapacityReservationFleet' {} a -> s {dryRun = a} :: CreateCapacityReservationFleet)

-- | The date and time at which the Capacity Reservation Fleet expires. When
-- the Capacity Reservation Fleet expires, its state changes to @expired@
-- and all of the Capacity Reservations in the Fleet expire.
--
-- The Capacity Reservation Fleet expires within an hour after the
-- specified time. For example, if you specify @5\/31\/2019@, @13:30:55@,
-- the Capacity Reservation Fleet is guaranteed to expire between
-- @13:30:55@ and @14:30:55@ on @5\/31\/2019@.
createCapacityReservationFleet_endDate :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe Prelude.UTCTime)
createCapacityReservationFleet_endDate = Lens.lens (\CreateCapacityReservationFleet' {endDate} -> endDate) (\s@CreateCapacityReservationFleet' {} a -> s {endDate = a} :: CreateCapacityReservationFleet) Prelude.. Lens.mapping Data._Time

-- | Indicates the type of instance launches that the Capacity Reservation
-- Fleet accepts. All Capacity Reservations in the Fleet inherit this
-- instance matching criteria.
--
-- Currently, Capacity Reservation Fleets support @open@ instance matching
-- criteria only. This means that instances that have matching attributes
-- (instance type, platform, and Availability Zone) run in the Capacity
-- Reservations automatically. Instances do not need to explicitly target a
-- Capacity Reservation Fleet to use its reserved capacity.
createCapacityReservationFleet_instanceMatchCriteria :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe FleetInstanceMatchCriteria)
createCapacityReservationFleet_instanceMatchCriteria = Lens.lens (\CreateCapacityReservationFleet' {instanceMatchCriteria} -> instanceMatchCriteria) (\s@CreateCapacityReservationFleet' {} a -> s {instanceMatchCriteria = a} :: CreateCapacityReservationFleet)

-- | The tags to assign to the Capacity Reservation Fleet. The tags are
-- automatically assigned to the Capacity Reservations in the Fleet.
createCapacityReservationFleet_tagSpecifications :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe [TagSpecification])
createCapacityReservationFleet_tagSpecifications = Lens.lens (\CreateCapacityReservationFleet' {tagSpecifications} -> tagSpecifications) (\s@CreateCapacityReservationFleet' {} a -> s {tagSpecifications = a} :: CreateCapacityReservationFleet) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the tenancy of the Capacity Reservation Fleet. All Capacity
-- Reservations in the Fleet inherit this tenancy. The Capacity Reservation
-- Fleet can have one of the following tenancy settings:
--
-- -   @default@ - The Capacity Reservation Fleet is created on hardware
--     that is shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservations are created on single-tenant
--     hardware that is dedicated to a single Amazon Web Services account.
createCapacityReservationFleet_tenancy :: Lens.Lens' CreateCapacityReservationFleet (Prelude.Maybe FleetCapacityReservationTenancy)
createCapacityReservationFleet_tenancy = Lens.lens (\CreateCapacityReservationFleet' {tenancy} -> tenancy) (\s@CreateCapacityReservationFleet' {} a -> s {tenancy = a} :: CreateCapacityReservationFleet)

-- | Information about the instance types for which to reserve the capacity.
createCapacityReservationFleet_instanceTypeSpecifications :: Lens.Lens' CreateCapacityReservationFleet [ReservationFleetInstanceSpecification]
createCapacityReservationFleet_instanceTypeSpecifications = Lens.lens (\CreateCapacityReservationFleet' {instanceTypeSpecifications} -> instanceTypeSpecifications) (\s@CreateCapacityReservationFleet' {} a -> s {instanceTypeSpecifications = a} :: CreateCapacityReservationFleet) Prelude.. Lens.coerced

-- | The total number of capacity units to be reserved by the Capacity
-- Reservation Fleet. This value, together with the instance type weights
-- that you assign to each instance type used by the Fleet determine the
-- number of instances for which the Fleet reserves capacity. Both values
-- are based on units that make sense for your workload. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
createCapacityReservationFleet_totalTargetCapacity :: Lens.Lens' CreateCapacityReservationFleet Prelude.Int
createCapacityReservationFleet_totalTargetCapacity = Lens.lens (\CreateCapacityReservationFleet' {totalTargetCapacity} -> totalTargetCapacity) (\s@CreateCapacityReservationFleet' {} a -> s {totalTargetCapacity = a} :: CreateCapacityReservationFleet)

instance
  Core.AWSRequest
    CreateCapacityReservationFleet
  where
  type
    AWSResponse CreateCapacityReservationFleet =
      CreateCapacityReservationFleetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCapacityReservationFleetResponse'
            Prelude.<$> (x Data..@? "allocationStrategy")
            Prelude.<*> (x Data..@? "capacityReservationFleetId")
            Prelude.<*> (x Data..@? "createTime")
            Prelude.<*> (x Data..@? "endDate")
            Prelude.<*> ( x
                            Data..@? "fleetCapacityReservationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "instanceMatchCriteria")
            Prelude.<*> (x Data..@? "state")
            Prelude.<*> ( x
                            Data..@? "tagSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "tenancy")
            Prelude.<*> (x Data..@? "totalFulfilledCapacity")
            Prelude.<*> (x Data..@? "totalTargetCapacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCapacityReservationFleet
  where
  hashWithSalt
    _salt
    CreateCapacityReservationFleet' {..} =
      _salt
        `Prelude.hashWithSalt` allocationStrategy
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` endDate
        `Prelude.hashWithSalt` instanceMatchCriteria
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` tenancy
        `Prelude.hashWithSalt` instanceTypeSpecifications
        `Prelude.hashWithSalt` totalTargetCapacity

instance
  Prelude.NFData
    CreateCapacityReservationFleet
  where
  rnf CreateCapacityReservationFleet' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf instanceMatchCriteria
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf instanceTypeSpecifications
      `Prelude.seq` Prelude.rnf totalTargetCapacity

instance
  Data.ToHeaders
    CreateCapacityReservationFleet
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCapacityReservationFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCapacityReservationFleet where
  toQuery CreateCapacityReservationFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateCapacityReservationFleet" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AllocationStrategy" Data.=: allocationStrategy,
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "EndDate" Data.=: endDate,
        "InstanceMatchCriteria"
          Data.=: instanceMatchCriteria,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Tenancy" Data.=: tenancy,
        Data.toQueryList
          "InstanceTypeSpecification"
          instanceTypeSpecifications,
        "TotalTargetCapacity" Data.=: totalTargetCapacity
      ]

-- | /See:/ 'newCreateCapacityReservationFleetResponse' smart constructor.
data CreateCapacityReservationFleetResponse = CreateCapacityReservationFleetResponse'
  { -- | The allocation strategy used by the Capacity Reservation Fleet.
    allocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Capacity Reservation Fleet.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation Fleet was created.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | The date and time at which the Capacity Reservation Fleet expires.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Information about the individual Capacity Reservations in the Capacity
    -- Reservation Fleet.
    fleetCapacityReservations :: Prelude.Maybe [FleetCapacityReservation],
    -- | The instance matching criteria for the Capacity Reservation Fleet.
    instanceMatchCriteria :: Prelude.Maybe FleetInstanceMatchCriteria,
    -- | The status of the Capacity Reservation Fleet.
    state :: Prelude.Maybe CapacityReservationFleetState,
    -- | The tags assigned to the Capacity Reservation Fleet.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates the tenancy of Capacity Reservation Fleet.
    tenancy :: Prelude.Maybe FleetCapacityReservationTenancy,
    -- | The requested capacity units that have been successfully reserved.
    totalFulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The total number of capacity units for which the Capacity Reservation
    -- Fleet reserves capacity.
    totalTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservationFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'createCapacityReservationFleetResponse_allocationStrategy' - The allocation strategy used by the Capacity Reservation Fleet.
--
-- 'capacityReservationFleetId', 'createCapacityReservationFleetResponse_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet.
--
-- 'createTime', 'createCapacityReservationFleetResponse_createTime' - The date and time at which the Capacity Reservation Fleet was created.
--
-- 'endDate', 'createCapacityReservationFleetResponse_endDate' - The date and time at which the Capacity Reservation Fleet expires.
--
-- 'fleetCapacityReservations', 'createCapacityReservationFleetResponse_fleetCapacityReservations' - Information about the individual Capacity Reservations in the Capacity
-- Reservation Fleet.
--
-- 'instanceMatchCriteria', 'createCapacityReservationFleetResponse_instanceMatchCriteria' - The instance matching criteria for the Capacity Reservation Fleet.
--
-- 'state', 'createCapacityReservationFleetResponse_state' - The status of the Capacity Reservation Fleet.
--
-- 'tags', 'createCapacityReservationFleetResponse_tags' - The tags assigned to the Capacity Reservation Fleet.
--
-- 'tenancy', 'createCapacityReservationFleetResponse_tenancy' - Indicates the tenancy of Capacity Reservation Fleet.
--
-- 'totalFulfilledCapacity', 'createCapacityReservationFleetResponse_totalFulfilledCapacity' - The requested capacity units that have been successfully reserved.
--
-- 'totalTargetCapacity', 'createCapacityReservationFleetResponse_totalTargetCapacity' - The total number of capacity units for which the Capacity Reservation
-- Fleet reserves capacity.
--
-- 'httpStatus', 'createCapacityReservationFleetResponse_httpStatus' - The response's http status code.
newCreateCapacityReservationFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCapacityReservationFleetResponse
newCreateCapacityReservationFleetResponse
  pHttpStatus_ =
    CreateCapacityReservationFleetResponse'
      { allocationStrategy =
          Prelude.Nothing,
        capacityReservationFleetId =
          Prelude.Nothing,
        createTime = Prelude.Nothing,
        endDate = Prelude.Nothing,
        fleetCapacityReservations =
          Prelude.Nothing,
        instanceMatchCriteria =
          Prelude.Nothing,
        state = Prelude.Nothing,
        tags = Prelude.Nothing,
        tenancy = Prelude.Nothing,
        totalFulfilledCapacity =
          Prelude.Nothing,
        totalTargetCapacity =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The allocation strategy used by the Capacity Reservation Fleet.
createCapacityReservationFleetResponse_allocationStrategy :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.Text)
createCapacityReservationFleetResponse_allocationStrategy = Lens.lens (\CreateCapacityReservationFleetResponse' {allocationStrategy} -> allocationStrategy) (\s@CreateCapacityReservationFleetResponse' {} a -> s {allocationStrategy = a} :: CreateCapacityReservationFleetResponse)

-- | The ID of the Capacity Reservation Fleet.
createCapacityReservationFleetResponse_capacityReservationFleetId :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.Text)
createCapacityReservationFleetResponse_capacityReservationFleetId = Lens.lens (\CreateCapacityReservationFleetResponse' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@CreateCapacityReservationFleetResponse' {} a -> s {capacityReservationFleetId = a} :: CreateCapacityReservationFleetResponse)

-- | The date and time at which the Capacity Reservation Fleet was created.
createCapacityReservationFleetResponse_createTime :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.UTCTime)
createCapacityReservationFleetResponse_createTime = Lens.lens (\CreateCapacityReservationFleetResponse' {createTime} -> createTime) (\s@CreateCapacityReservationFleetResponse' {} a -> s {createTime = a} :: CreateCapacityReservationFleetResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time at which the Capacity Reservation Fleet expires.
createCapacityReservationFleetResponse_endDate :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.UTCTime)
createCapacityReservationFleetResponse_endDate = Lens.lens (\CreateCapacityReservationFleetResponse' {endDate} -> endDate) (\s@CreateCapacityReservationFleetResponse' {} a -> s {endDate = a} :: CreateCapacityReservationFleetResponse) Prelude.. Lens.mapping Data._Time

-- | Information about the individual Capacity Reservations in the Capacity
-- Reservation Fleet.
createCapacityReservationFleetResponse_fleetCapacityReservations :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe [FleetCapacityReservation])
createCapacityReservationFleetResponse_fleetCapacityReservations = Lens.lens (\CreateCapacityReservationFleetResponse' {fleetCapacityReservations} -> fleetCapacityReservations) (\s@CreateCapacityReservationFleetResponse' {} a -> s {fleetCapacityReservations = a} :: CreateCapacityReservationFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The instance matching criteria for the Capacity Reservation Fleet.
createCapacityReservationFleetResponse_instanceMatchCriteria :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe FleetInstanceMatchCriteria)
createCapacityReservationFleetResponse_instanceMatchCriteria = Lens.lens (\CreateCapacityReservationFleetResponse' {instanceMatchCriteria} -> instanceMatchCriteria) (\s@CreateCapacityReservationFleetResponse' {} a -> s {instanceMatchCriteria = a} :: CreateCapacityReservationFleetResponse)

-- | The status of the Capacity Reservation Fleet.
createCapacityReservationFleetResponse_state :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe CapacityReservationFleetState)
createCapacityReservationFleetResponse_state = Lens.lens (\CreateCapacityReservationFleetResponse' {state} -> state) (\s@CreateCapacityReservationFleetResponse' {} a -> s {state = a} :: CreateCapacityReservationFleetResponse)

-- | The tags assigned to the Capacity Reservation Fleet.
createCapacityReservationFleetResponse_tags :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe [Tag])
createCapacityReservationFleetResponse_tags = Lens.lens (\CreateCapacityReservationFleetResponse' {tags} -> tags) (\s@CreateCapacityReservationFleetResponse' {} a -> s {tags = a} :: CreateCapacityReservationFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the tenancy of Capacity Reservation Fleet.
createCapacityReservationFleetResponse_tenancy :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe FleetCapacityReservationTenancy)
createCapacityReservationFleetResponse_tenancy = Lens.lens (\CreateCapacityReservationFleetResponse' {tenancy} -> tenancy) (\s@CreateCapacityReservationFleetResponse' {} a -> s {tenancy = a} :: CreateCapacityReservationFleetResponse)

-- | The requested capacity units that have been successfully reserved.
createCapacityReservationFleetResponse_totalFulfilledCapacity :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.Double)
createCapacityReservationFleetResponse_totalFulfilledCapacity = Lens.lens (\CreateCapacityReservationFleetResponse' {totalFulfilledCapacity} -> totalFulfilledCapacity) (\s@CreateCapacityReservationFleetResponse' {} a -> s {totalFulfilledCapacity = a} :: CreateCapacityReservationFleetResponse)

-- | The total number of capacity units for which the Capacity Reservation
-- Fleet reserves capacity.
createCapacityReservationFleetResponse_totalTargetCapacity :: Lens.Lens' CreateCapacityReservationFleetResponse (Prelude.Maybe Prelude.Int)
createCapacityReservationFleetResponse_totalTargetCapacity = Lens.lens (\CreateCapacityReservationFleetResponse' {totalTargetCapacity} -> totalTargetCapacity) (\s@CreateCapacityReservationFleetResponse' {} a -> s {totalTargetCapacity = a} :: CreateCapacityReservationFleetResponse)

-- | The response's http status code.
createCapacityReservationFleetResponse_httpStatus :: Lens.Lens' CreateCapacityReservationFleetResponse Prelude.Int
createCapacityReservationFleetResponse_httpStatus = Lens.lens (\CreateCapacityReservationFleetResponse' {httpStatus} -> httpStatus) (\s@CreateCapacityReservationFleetResponse' {} a -> s {httpStatus = a} :: CreateCapacityReservationFleetResponse)

instance
  Prelude.NFData
    CreateCapacityReservationFleetResponse
  where
  rnf CreateCapacityReservationFleetResponse' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf capacityReservationFleetId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf fleetCapacityReservations
      `Prelude.seq` Prelude.rnf instanceMatchCriteria
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf totalFulfilledCapacity
      `Prelude.seq` Prelude.rnf totalTargetCapacity
      `Prelude.seq` Prelude.rnf httpStatus

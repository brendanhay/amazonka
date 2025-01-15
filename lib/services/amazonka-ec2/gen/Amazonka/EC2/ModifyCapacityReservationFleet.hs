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
-- Module      : Amazonka.EC2.ModifyCapacityReservationFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Capacity Reservation Fleet.
--
-- When you modify the total target capacity of a Capacity Reservation
-- Fleet, the Fleet automatically creates new Capacity Reservations, or
-- modifies or cancels existing Capacity Reservations in the Fleet to meet
-- the new total target capacity. When you modify the end date for the
-- Fleet, the end dates for all of the individual Capacity Reservations in
-- the Fleet are updated accordingly.
module Amazonka.EC2.ModifyCapacityReservationFleet
  ( -- * Creating a Request
    ModifyCapacityReservationFleet (..),
    newModifyCapacityReservationFleet,

    -- * Request Lenses
    modifyCapacityReservationFleet_dryRun,
    modifyCapacityReservationFleet_endDate,
    modifyCapacityReservationFleet_removeEndDate,
    modifyCapacityReservationFleet_totalTargetCapacity,
    modifyCapacityReservationFleet_capacityReservationFleetId,

    -- * Destructuring the Response
    ModifyCapacityReservationFleetResponse (..),
    newModifyCapacityReservationFleetResponse,

    -- * Response Lenses
    modifyCapacityReservationFleetResponse_return,
    modifyCapacityReservationFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCapacityReservationFleet' smart constructor.
data ModifyCapacityReservationFleet = ModifyCapacityReservationFleet'
  { -- | Checks whether you have the required permissions for the action, without
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
    --
    -- You can\'t specify __EndDate__ and __RemoveEndDate__ in the same
    -- request.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether to remove the end date from the Capacity Reservation
    -- Fleet. If you remove the end date, the Capacity Reservation Fleet does
    -- not expire and it remains active until you explicitly cancel it using
    -- the __CancelCapacityReservationFleet__ action.
    --
    -- You can\'t specify __RemoveEndDate__ and __EndDate__ in the same
    -- request.
    removeEndDate :: Prelude.Maybe Prelude.Bool,
    -- | The total number of capacity units to be reserved by the Capacity
    -- Reservation Fleet. This value, together with the instance type weights
    -- that you assign to each instance type used by the Fleet determine the
    -- number of instances for which the Fleet reserves capacity. Both values
    -- are based on units that make sense for your workload. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
    -- in the Amazon EC2 User Guide.
    totalTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Capacity Reservation Fleet to modify.
    capacityReservationFleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCapacityReservationFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyCapacityReservationFleet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'endDate', 'modifyCapacityReservationFleet_endDate' - The date and time at which the Capacity Reservation Fleet expires. When
-- the Capacity Reservation Fleet expires, its state changes to @expired@
-- and all of the Capacity Reservations in the Fleet expire.
--
-- The Capacity Reservation Fleet expires within an hour after the
-- specified time. For example, if you specify @5\/31\/2019@, @13:30:55@,
-- the Capacity Reservation Fleet is guaranteed to expire between
-- @13:30:55@ and @14:30:55@ on @5\/31\/2019@.
--
-- You can\'t specify __EndDate__ and __RemoveEndDate__ in the same
-- request.
--
-- 'removeEndDate', 'modifyCapacityReservationFleet_removeEndDate' - Indicates whether to remove the end date from the Capacity Reservation
-- Fleet. If you remove the end date, the Capacity Reservation Fleet does
-- not expire and it remains active until you explicitly cancel it using
-- the __CancelCapacityReservationFleet__ action.
--
-- You can\'t specify __RemoveEndDate__ and __EndDate__ in the same
-- request.
--
-- 'totalTargetCapacity', 'modifyCapacityReservationFleet_totalTargetCapacity' - The total number of capacity units to be reserved by the Capacity
-- Reservation Fleet. This value, together with the instance type weights
-- that you assign to each instance type used by the Fleet determine the
-- number of instances for which the Fleet reserves capacity. Both values
-- are based on units that make sense for your workload. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
--
-- 'capacityReservationFleetId', 'modifyCapacityReservationFleet_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet to modify.
newModifyCapacityReservationFleet ::
  -- | 'capacityReservationFleetId'
  Prelude.Text ->
  ModifyCapacityReservationFleet
newModifyCapacityReservationFleet
  pCapacityReservationFleetId_ =
    ModifyCapacityReservationFleet'
      { dryRun =
          Prelude.Nothing,
        endDate = Prelude.Nothing,
        removeEndDate = Prelude.Nothing,
        totalTargetCapacity = Prelude.Nothing,
        capacityReservationFleetId =
          pCapacityReservationFleetId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyCapacityReservationFleet_dryRun :: Lens.Lens' ModifyCapacityReservationFleet (Prelude.Maybe Prelude.Bool)
modifyCapacityReservationFleet_dryRun = Lens.lens (\ModifyCapacityReservationFleet' {dryRun} -> dryRun) (\s@ModifyCapacityReservationFleet' {} a -> s {dryRun = a} :: ModifyCapacityReservationFleet)

-- | The date and time at which the Capacity Reservation Fleet expires. When
-- the Capacity Reservation Fleet expires, its state changes to @expired@
-- and all of the Capacity Reservations in the Fleet expire.
--
-- The Capacity Reservation Fleet expires within an hour after the
-- specified time. For example, if you specify @5\/31\/2019@, @13:30:55@,
-- the Capacity Reservation Fleet is guaranteed to expire between
-- @13:30:55@ and @14:30:55@ on @5\/31\/2019@.
--
-- You can\'t specify __EndDate__ and __RemoveEndDate__ in the same
-- request.
modifyCapacityReservationFleet_endDate :: Lens.Lens' ModifyCapacityReservationFleet (Prelude.Maybe Prelude.UTCTime)
modifyCapacityReservationFleet_endDate = Lens.lens (\ModifyCapacityReservationFleet' {endDate} -> endDate) (\s@ModifyCapacityReservationFleet' {} a -> s {endDate = a} :: ModifyCapacityReservationFleet) Prelude.. Lens.mapping Data._Time

-- | Indicates whether to remove the end date from the Capacity Reservation
-- Fleet. If you remove the end date, the Capacity Reservation Fleet does
-- not expire and it remains active until you explicitly cancel it using
-- the __CancelCapacityReservationFleet__ action.
--
-- You can\'t specify __RemoveEndDate__ and __EndDate__ in the same
-- request.
modifyCapacityReservationFleet_removeEndDate :: Lens.Lens' ModifyCapacityReservationFleet (Prelude.Maybe Prelude.Bool)
modifyCapacityReservationFleet_removeEndDate = Lens.lens (\ModifyCapacityReservationFleet' {removeEndDate} -> removeEndDate) (\s@ModifyCapacityReservationFleet' {} a -> s {removeEndDate = a} :: ModifyCapacityReservationFleet)

-- | The total number of capacity units to be reserved by the Capacity
-- Reservation Fleet. This value, together with the instance type weights
-- that you assign to each instance type used by the Fleet determine the
-- number of instances for which the Fleet reserves capacity. Both values
-- are based on units that make sense for your workload. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
modifyCapacityReservationFleet_totalTargetCapacity :: Lens.Lens' ModifyCapacityReservationFleet (Prelude.Maybe Prelude.Int)
modifyCapacityReservationFleet_totalTargetCapacity = Lens.lens (\ModifyCapacityReservationFleet' {totalTargetCapacity} -> totalTargetCapacity) (\s@ModifyCapacityReservationFleet' {} a -> s {totalTargetCapacity = a} :: ModifyCapacityReservationFleet)

-- | The ID of the Capacity Reservation Fleet to modify.
modifyCapacityReservationFleet_capacityReservationFleetId :: Lens.Lens' ModifyCapacityReservationFleet Prelude.Text
modifyCapacityReservationFleet_capacityReservationFleetId = Lens.lens (\ModifyCapacityReservationFleet' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@ModifyCapacityReservationFleet' {} a -> s {capacityReservationFleetId = a} :: ModifyCapacityReservationFleet)

instance
  Core.AWSRequest
    ModifyCapacityReservationFleet
  where
  type
    AWSResponse ModifyCapacityReservationFleet =
      ModifyCapacityReservationFleetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyCapacityReservationFleetResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyCapacityReservationFleet
  where
  hashWithSalt
    _salt
    ModifyCapacityReservationFleet' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` endDate
        `Prelude.hashWithSalt` removeEndDate
        `Prelude.hashWithSalt` totalTargetCapacity
        `Prelude.hashWithSalt` capacityReservationFleetId

instance
  Prelude.NFData
    ModifyCapacityReservationFleet
  where
  rnf ModifyCapacityReservationFleet' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf endDate `Prelude.seq`
        Prelude.rnf removeEndDate `Prelude.seq`
          Prelude.rnf totalTargetCapacity `Prelude.seq`
            Prelude.rnf capacityReservationFleetId

instance
  Data.ToHeaders
    ModifyCapacityReservationFleet
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCapacityReservationFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCapacityReservationFleet where
  toQuery ModifyCapacityReservationFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyCapacityReservationFleet" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "EndDate" Data.=: endDate,
        "RemoveEndDate" Data.=: removeEndDate,
        "TotalTargetCapacity" Data.=: totalTargetCapacity,
        "CapacityReservationFleetId"
          Data.=: capacityReservationFleetId
      ]

-- | /See:/ 'newModifyCapacityReservationFleetResponse' smart constructor.
data ModifyCapacityReservationFleetResponse = ModifyCapacityReservationFleetResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCapacityReservationFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyCapacityReservationFleetResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyCapacityReservationFleetResponse_httpStatus' - The response's http status code.
newModifyCapacityReservationFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCapacityReservationFleetResponse
newModifyCapacityReservationFleetResponse
  pHttpStatus_ =
    ModifyCapacityReservationFleetResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyCapacityReservationFleetResponse_return :: Lens.Lens' ModifyCapacityReservationFleetResponse (Prelude.Maybe Prelude.Bool)
modifyCapacityReservationFleetResponse_return = Lens.lens (\ModifyCapacityReservationFleetResponse' {return'} -> return') (\s@ModifyCapacityReservationFleetResponse' {} a -> s {return' = a} :: ModifyCapacityReservationFleetResponse)

-- | The response's http status code.
modifyCapacityReservationFleetResponse_httpStatus :: Lens.Lens' ModifyCapacityReservationFleetResponse Prelude.Int
modifyCapacityReservationFleetResponse_httpStatus = Lens.lens (\ModifyCapacityReservationFleetResponse' {httpStatus} -> httpStatus) (\s@ModifyCapacityReservationFleetResponse' {} a -> s {httpStatus = a} :: ModifyCapacityReservationFleetResponse)

instance
  Prelude.NFData
    ModifyCapacityReservationFleetResponse
  where
  rnf ModifyCapacityReservationFleetResponse' {..} =
    Prelude.rnf return' `Prelude.seq`
      Prelude.rnf httpStatus

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
-- Module      : Amazonka.EC2.CancelCapacityReservationFleets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Capacity Reservation Fleets. When you cancel a
-- Capacity Reservation Fleet, the following happens:
--
-- -   The Capacity Reservation Fleet\'s status changes to @cancelled@.
--
-- -   The individual Capacity Reservations in the Fleet are cancelled.
--     Instances running in the Capacity Reservations at the time of
--     cancelling the Fleet continue to run in shared capacity.
--
-- -   The Fleet stops creating new Capacity Reservations.
module Amazonka.EC2.CancelCapacityReservationFleets
  ( -- * Creating a Request
    CancelCapacityReservationFleets (..),
    newCancelCapacityReservationFleets,

    -- * Request Lenses
    cancelCapacityReservationFleets_dryRun,
    cancelCapacityReservationFleets_capacityReservationFleetIds,

    -- * Destructuring the Response
    CancelCapacityReservationFleetsResponse (..),
    newCancelCapacityReservationFleetsResponse,

    -- * Response Lenses
    cancelCapacityReservationFleetsResponse_failedFleetCancellations,
    cancelCapacityReservationFleetsResponse_successfulFleetCancellations,
    cancelCapacityReservationFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelCapacityReservationFleets' smart constructor.
data CancelCapacityReservationFleets = CancelCapacityReservationFleets'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the Capacity Reservation Fleets to cancel.
    capacityReservationFleetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservationFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelCapacityReservationFleets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'capacityReservationFleetIds', 'cancelCapacityReservationFleets_capacityReservationFleetIds' - The IDs of the Capacity Reservation Fleets to cancel.
newCancelCapacityReservationFleets ::
  CancelCapacityReservationFleets
newCancelCapacityReservationFleets =
  CancelCapacityReservationFleets'
    { dryRun =
        Prelude.Nothing,
      capacityReservationFleetIds =
        Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelCapacityReservationFleets_dryRun :: Lens.Lens' CancelCapacityReservationFleets (Prelude.Maybe Prelude.Bool)
cancelCapacityReservationFleets_dryRun = Lens.lens (\CancelCapacityReservationFleets' {dryRun} -> dryRun) (\s@CancelCapacityReservationFleets' {} a -> s {dryRun = a} :: CancelCapacityReservationFleets)

-- | The IDs of the Capacity Reservation Fleets to cancel.
cancelCapacityReservationFleets_capacityReservationFleetIds :: Lens.Lens' CancelCapacityReservationFleets [Prelude.Text]
cancelCapacityReservationFleets_capacityReservationFleetIds = Lens.lens (\CancelCapacityReservationFleets' {capacityReservationFleetIds} -> capacityReservationFleetIds) (\s@CancelCapacityReservationFleets' {} a -> s {capacityReservationFleetIds = a} :: CancelCapacityReservationFleets) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CancelCapacityReservationFleets
  where
  type
    AWSResponse CancelCapacityReservationFleets =
      CancelCapacityReservationFleetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CancelCapacityReservationFleetsResponse'
            Prelude.<$> ( x
                            Data..@? "failedFleetCancellationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "successfulFleetCancellationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelCapacityReservationFleets
  where
  hashWithSalt
    _salt
    CancelCapacityReservationFleets' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` capacityReservationFleetIds

instance
  Prelude.NFData
    CancelCapacityReservationFleets
  where
  rnf CancelCapacityReservationFleets' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf capacityReservationFleetIds

instance
  Data.ToHeaders
    CancelCapacityReservationFleets
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelCapacityReservationFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelCapacityReservationFleets where
  toQuery CancelCapacityReservationFleets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CancelCapacityReservationFleets" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList
          "CapacityReservationFleetId"
          capacityReservationFleetIds
      ]

-- | /See:/ 'newCancelCapacityReservationFleetsResponse' smart constructor.
data CancelCapacityReservationFleetsResponse = CancelCapacityReservationFleetsResponse'
  { -- | Information about the Capacity Reservation Fleets that could not be
    -- cancelled.
    failedFleetCancellations :: Prelude.Maybe [FailedCapacityReservationFleetCancellationResult],
    -- | Information about the Capacity Reservation Fleets that were successfully
    -- cancelled.
    successfulFleetCancellations :: Prelude.Maybe [CapacityReservationFleetCancellationState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservationFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedFleetCancellations', 'cancelCapacityReservationFleetsResponse_failedFleetCancellations' - Information about the Capacity Reservation Fleets that could not be
-- cancelled.
--
-- 'successfulFleetCancellations', 'cancelCapacityReservationFleetsResponse_successfulFleetCancellations' - Information about the Capacity Reservation Fleets that were successfully
-- cancelled.
--
-- 'httpStatus', 'cancelCapacityReservationFleetsResponse_httpStatus' - The response's http status code.
newCancelCapacityReservationFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelCapacityReservationFleetsResponse
newCancelCapacityReservationFleetsResponse
  pHttpStatus_ =
    CancelCapacityReservationFleetsResponse'
      { failedFleetCancellations =
          Prelude.Nothing,
        successfulFleetCancellations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the Capacity Reservation Fleets that could not be
-- cancelled.
cancelCapacityReservationFleetsResponse_failedFleetCancellations :: Lens.Lens' CancelCapacityReservationFleetsResponse (Prelude.Maybe [FailedCapacityReservationFleetCancellationResult])
cancelCapacityReservationFleetsResponse_failedFleetCancellations = Lens.lens (\CancelCapacityReservationFleetsResponse' {failedFleetCancellations} -> failedFleetCancellations) (\s@CancelCapacityReservationFleetsResponse' {} a -> s {failedFleetCancellations = a} :: CancelCapacityReservationFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the Capacity Reservation Fleets that were successfully
-- cancelled.
cancelCapacityReservationFleetsResponse_successfulFleetCancellations :: Lens.Lens' CancelCapacityReservationFleetsResponse (Prelude.Maybe [CapacityReservationFleetCancellationState])
cancelCapacityReservationFleetsResponse_successfulFleetCancellations = Lens.lens (\CancelCapacityReservationFleetsResponse' {successfulFleetCancellations} -> successfulFleetCancellations) (\s@CancelCapacityReservationFleetsResponse' {} a -> s {successfulFleetCancellations = a} :: CancelCapacityReservationFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
cancelCapacityReservationFleetsResponse_httpStatus :: Lens.Lens' CancelCapacityReservationFleetsResponse Prelude.Int
cancelCapacityReservationFleetsResponse_httpStatus = Lens.lens (\CancelCapacityReservationFleetsResponse' {httpStatus} -> httpStatus) (\s@CancelCapacityReservationFleetsResponse' {} a -> s {httpStatus = a} :: CancelCapacityReservationFleetsResponse)

instance
  Prelude.NFData
    CancelCapacityReservationFleetsResponse
  where
  rnf CancelCapacityReservationFleetsResponse' {..} =
    Prelude.rnf failedFleetCancellations
      `Prelude.seq` Prelude.rnf successfulFleetCancellations
      `Prelude.seq` Prelude.rnf httpStatus

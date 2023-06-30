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
-- Module      : Amazonka.EC2.GetCapacityReservationUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information about a Capacity Reservation. If the Capacity
-- Reservation is shared, it shows usage information for the Capacity
-- Reservation owner and each Amazon Web Services account that is currently
-- using the shared capacity. If the Capacity Reservation is not shared, it
-- shows only the Capacity Reservation owner\'s usage.
module Amazonka.EC2.GetCapacityReservationUsage
  ( -- * Creating a Request
    GetCapacityReservationUsage (..),
    newGetCapacityReservationUsage,

    -- * Request Lenses
    getCapacityReservationUsage_dryRun,
    getCapacityReservationUsage_maxResults,
    getCapacityReservationUsage_nextToken,
    getCapacityReservationUsage_capacityReservationId,

    -- * Destructuring the Response
    GetCapacityReservationUsageResponse (..),
    newGetCapacityReservationUsageResponse,

    -- * Response Lenses
    getCapacityReservationUsageResponse_availableInstanceCount,
    getCapacityReservationUsageResponse_capacityReservationId,
    getCapacityReservationUsageResponse_instanceType,
    getCapacityReservationUsageResponse_instanceUsages,
    getCapacityReservationUsageResponse_nextToken,
    getCapacityReservationUsageResponse_state,
    getCapacityReservationUsageResponse_totalInstanceCount,
    getCapacityReservationUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCapacityReservationUsage' smart constructor.
data GetCapacityReservationUsage = GetCapacityReservationUsage'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    --
    -- Valid range: Minimum value of 1. Maximum value of 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityReservationUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getCapacityReservationUsage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getCapacityReservationUsage_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- Valid range: Minimum value of 1. Maximum value of 1000.
--
-- 'nextToken', 'getCapacityReservationUsage_nextToken' - The token to use to retrieve the next page of results.
--
-- 'capacityReservationId', 'getCapacityReservationUsage_capacityReservationId' - The ID of the Capacity Reservation.
newGetCapacityReservationUsage ::
  -- | 'capacityReservationId'
  Prelude.Text ->
  GetCapacityReservationUsage
newGetCapacityReservationUsage
  pCapacityReservationId_ =
    GetCapacityReservationUsage'
      { dryRun =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        capacityReservationId =
          pCapacityReservationId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getCapacityReservationUsage_dryRun :: Lens.Lens' GetCapacityReservationUsage (Prelude.Maybe Prelude.Bool)
getCapacityReservationUsage_dryRun = Lens.lens (\GetCapacityReservationUsage' {dryRun} -> dryRun) (\s@GetCapacityReservationUsage' {} a -> s {dryRun = a} :: GetCapacityReservationUsage)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- Valid range: Minimum value of 1. Maximum value of 1000.
getCapacityReservationUsage_maxResults :: Lens.Lens' GetCapacityReservationUsage (Prelude.Maybe Prelude.Natural)
getCapacityReservationUsage_maxResults = Lens.lens (\GetCapacityReservationUsage' {maxResults} -> maxResults) (\s@GetCapacityReservationUsage' {} a -> s {maxResults = a} :: GetCapacityReservationUsage)

-- | The token to use to retrieve the next page of results.
getCapacityReservationUsage_nextToken :: Lens.Lens' GetCapacityReservationUsage (Prelude.Maybe Prelude.Text)
getCapacityReservationUsage_nextToken = Lens.lens (\GetCapacityReservationUsage' {nextToken} -> nextToken) (\s@GetCapacityReservationUsage' {} a -> s {nextToken = a} :: GetCapacityReservationUsage)

-- | The ID of the Capacity Reservation.
getCapacityReservationUsage_capacityReservationId :: Lens.Lens' GetCapacityReservationUsage Prelude.Text
getCapacityReservationUsage_capacityReservationId = Lens.lens (\GetCapacityReservationUsage' {capacityReservationId} -> capacityReservationId) (\s@GetCapacityReservationUsage' {} a -> s {capacityReservationId = a} :: GetCapacityReservationUsage)

instance Core.AWSRequest GetCapacityReservationUsage where
  type
    AWSResponse GetCapacityReservationUsage =
      GetCapacityReservationUsageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetCapacityReservationUsageResponse'
            Prelude.<$> (x Data..@? "availableInstanceCount")
            Prelude.<*> (x Data..@? "capacityReservationId")
            Prelude.<*> (x Data..@? "instanceType")
            Prelude.<*> ( x
                            Data..@? "instanceUsageSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (x Data..@? "state")
            Prelude.<*> (x Data..@? "totalInstanceCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCapacityReservationUsage where
  hashWithSalt _salt GetCapacityReservationUsage' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` capacityReservationId

instance Prelude.NFData GetCapacityReservationUsage where
  rnf GetCapacityReservationUsage' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf capacityReservationId

instance Data.ToHeaders GetCapacityReservationUsage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCapacityReservationUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCapacityReservationUsage where
  toQuery GetCapacityReservationUsage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetCapacityReservationUsage" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "CapacityReservationId"
          Data.=: capacityReservationId
      ]

-- | /See:/ 'newGetCapacityReservationUsageResponse' smart constructor.
data GetCapacityReservationUsageResponse = GetCapacityReservationUsageResponse'
  { -- | The remaining capacity. Indicates the number of instances that can be
    -- launched in the Capacity Reservation.
    availableInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The type of instance for which the Capacity Reservation reserves
    -- capacity.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Information about the Capacity Reservation usage.
    instanceUsages :: Prelude.Maybe [InstanceUsage],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- | The number of instances for which the Capacity Reservation reserves
    -- capacity.
    totalInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityReservationUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableInstanceCount', 'getCapacityReservationUsageResponse_availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
--
-- 'capacityReservationId', 'getCapacityReservationUsageResponse_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'instanceType', 'getCapacityReservationUsageResponse_instanceType' - The type of instance for which the Capacity Reservation reserves
-- capacity.
--
-- 'instanceUsages', 'getCapacityReservationUsageResponse_instanceUsages' - Information about the Capacity Reservation usage.
--
-- 'nextToken', 'getCapacityReservationUsageResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'state', 'getCapacityReservationUsageResponse_state' - The current state of the Capacity Reservation. A Capacity Reservation
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
-- 'totalInstanceCount', 'getCapacityReservationUsageResponse_totalInstanceCount' - The number of instances for which the Capacity Reservation reserves
-- capacity.
--
-- 'httpStatus', 'getCapacityReservationUsageResponse_httpStatus' - The response's http status code.
newGetCapacityReservationUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCapacityReservationUsageResponse
newGetCapacityReservationUsageResponse pHttpStatus_ =
  GetCapacityReservationUsageResponse'
    { availableInstanceCount =
        Prelude.Nothing,
      capacityReservationId =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceUsages = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing,
      totalInstanceCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The remaining capacity. Indicates the number of instances that can be
-- launched in the Capacity Reservation.
getCapacityReservationUsageResponse_availableInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe Prelude.Int)
getCapacityReservationUsageResponse_availableInstanceCount = Lens.lens (\GetCapacityReservationUsageResponse' {availableInstanceCount} -> availableInstanceCount) (\s@GetCapacityReservationUsageResponse' {} a -> s {availableInstanceCount = a} :: GetCapacityReservationUsageResponse)

-- | The ID of the Capacity Reservation.
getCapacityReservationUsageResponse_capacityReservationId :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe Prelude.Text)
getCapacityReservationUsageResponse_capacityReservationId = Lens.lens (\GetCapacityReservationUsageResponse' {capacityReservationId} -> capacityReservationId) (\s@GetCapacityReservationUsageResponse' {} a -> s {capacityReservationId = a} :: GetCapacityReservationUsageResponse)

-- | The type of instance for which the Capacity Reservation reserves
-- capacity.
getCapacityReservationUsageResponse_instanceType :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe Prelude.Text)
getCapacityReservationUsageResponse_instanceType = Lens.lens (\GetCapacityReservationUsageResponse' {instanceType} -> instanceType) (\s@GetCapacityReservationUsageResponse' {} a -> s {instanceType = a} :: GetCapacityReservationUsageResponse)

-- | Information about the Capacity Reservation usage.
getCapacityReservationUsageResponse_instanceUsages :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe [InstanceUsage])
getCapacityReservationUsageResponse_instanceUsages = Lens.lens (\GetCapacityReservationUsageResponse' {instanceUsages} -> instanceUsages) (\s@GetCapacityReservationUsageResponse' {} a -> s {instanceUsages = a} :: GetCapacityReservationUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getCapacityReservationUsageResponse_nextToken :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe Prelude.Text)
getCapacityReservationUsageResponse_nextToken = Lens.lens (\GetCapacityReservationUsageResponse' {nextToken} -> nextToken) (\s@GetCapacityReservationUsageResponse' {} a -> s {nextToken = a} :: GetCapacityReservationUsageResponse)

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
getCapacityReservationUsageResponse_state :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe CapacityReservationState)
getCapacityReservationUsageResponse_state = Lens.lens (\GetCapacityReservationUsageResponse' {state} -> state) (\s@GetCapacityReservationUsageResponse' {} a -> s {state = a} :: GetCapacityReservationUsageResponse)

-- | The number of instances for which the Capacity Reservation reserves
-- capacity.
getCapacityReservationUsageResponse_totalInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Prelude.Maybe Prelude.Int)
getCapacityReservationUsageResponse_totalInstanceCount = Lens.lens (\GetCapacityReservationUsageResponse' {totalInstanceCount} -> totalInstanceCount) (\s@GetCapacityReservationUsageResponse' {} a -> s {totalInstanceCount = a} :: GetCapacityReservationUsageResponse)

-- | The response's http status code.
getCapacityReservationUsageResponse_httpStatus :: Lens.Lens' GetCapacityReservationUsageResponse Prelude.Int
getCapacityReservationUsageResponse_httpStatus = Lens.lens (\GetCapacityReservationUsageResponse' {httpStatus} -> httpStatus) (\s@GetCapacityReservationUsageResponse' {} a -> s {httpStatus = a} :: GetCapacityReservationUsageResponse)

instance
  Prelude.NFData
    GetCapacityReservationUsageResponse
  where
  rnf GetCapacityReservationUsageResponse' {..} =
    Prelude.rnf availableInstanceCount
      `Prelude.seq` Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceUsages
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf totalInstanceCount
      `Prelude.seq` Prelude.rnf httpStatus

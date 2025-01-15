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
-- Module      : Amazonka.EC2.ModifyCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Capacity Reservation\'s capacity and the conditions under
-- which it is to be released. You cannot change a Capacity Reservation\'s
-- instance type, EBS optimization, instance store settings, platform,
-- Availability Zone, or instance eligibility. If you need to modify any of
-- these attributes, we recommend that you cancel the Capacity Reservation,
-- and then create a new one with the required attributes.
module Amazonka.EC2.ModifyCapacityReservation
  ( -- * Creating a Request
    ModifyCapacityReservation (..),
    newModifyCapacityReservation,

    -- * Request Lenses
    modifyCapacityReservation_accept,
    modifyCapacityReservation_additionalInfo,
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_endDate,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_instanceCount,
    modifyCapacityReservation_capacityReservationId,

    -- * Destructuring the Response
    ModifyCapacityReservationResponse (..),
    newModifyCapacityReservationResponse,

    -- * Response Lenses
    modifyCapacityReservationResponse_return,
    modifyCapacityReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCapacityReservation' smart constructor.
data ModifyCapacityReservation = ModifyCapacityReservation'
  { -- | Reserved. Capacity Reservations you have created are accepted by
    -- default.
    accept :: Prelude.Maybe Prelude.Bool,
    -- | Reserved for future use.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The date and time at which the Capacity Reservation expires. When a
    -- Capacity Reservation expires, the reserved capacity is released and you
    -- can no longer launch instances into it. The Capacity Reservation\'s
    -- state changes to @expired@ when it reaches its end date and time.
    --
    -- The Capacity Reservation is cancelled within an hour from the specified
    -- time. For example, if you specify 5\/31\/2019, 13:30:55, the Capacity
    -- Reservation is guaranteed to end between 13:30:55 and 14:30:55 on
    -- 5\/31\/2019.
    --
    -- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
    -- @EndDate@ if @EndDateType@ is @unlimited@.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the way in which the Capacity Reservation ends. A Capacity
    -- Reservation can have one of the following end types:
    --
    -- -   @unlimited@ - The Capacity Reservation remains active until you
    --     explicitly cancel it. Do not provide an @EndDate@ value if
    --     @EndDateType@ is @unlimited@.
    --
    -- -   @limited@ - The Capacity Reservation expires automatically at a
    --     specified date and time. You must provide an @EndDate@ value if
    --     @EndDateType@ is @limited@.
    endDateType :: Prelude.Maybe EndDateType,
    -- | The number of instances for which to reserve capacity. The number of
    -- instances can\'t be increased or decreased by more than @1000@ in a
    -- single request.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'modifyCapacityReservation_accept' - Reserved. Capacity Reservations you have created are accepted by
-- default.
--
-- 'additionalInfo', 'modifyCapacityReservation_additionalInfo' - Reserved for future use.
--
-- 'dryRun', 'modifyCapacityReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'endDate', 'modifyCapacityReservation_endDate' - The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- The Capacity Reservation is cancelled within an hour from the specified
-- time. For example, if you specify 5\/31\/2019, 13:30:55, the Capacity
-- Reservation is guaranteed to end between 13:30:55 and 14:30:55 on
-- 5\/31\/2019.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
-- @EndDate@ if @EndDateType@ is @unlimited@.
--
-- 'endDateType', 'modifyCapacityReservation_endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it. Do not provide an @EndDate@ value if
--     @EndDateType@ is @unlimited@.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time. You must provide an @EndDate@ value if
--     @EndDateType@ is @limited@.
--
-- 'instanceCount', 'modifyCapacityReservation_instanceCount' - The number of instances for which to reserve capacity. The number of
-- instances can\'t be increased or decreased by more than @1000@ in a
-- single request.
--
-- 'capacityReservationId', 'modifyCapacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
newModifyCapacityReservation ::
  -- | 'capacityReservationId'
  Prelude.Text ->
  ModifyCapacityReservation
newModifyCapacityReservation pCapacityReservationId_ =
  ModifyCapacityReservation'
    { accept =
        Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      endDate = Prelude.Nothing,
      endDateType = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      capacityReservationId = pCapacityReservationId_
    }

-- | Reserved. Capacity Reservations you have created are accepted by
-- default.
modifyCapacityReservation_accept :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Bool)
modifyCapacityReservation_accept = Lens.lens (\ModifyCapacityReservation' {accept} -> accept) (\s@ModifyCapacityReservation' {} a -> s {accept = a} :: ModifyCapacityReservation)

-- | Reserved for future use.
modifyCapacityReservation_additionalInfo :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Text)
modifyCapacityReservation_additionalInfo = Lens.lens (\ModifyCapacityReservation' {additionalInfo} -> additionalInfo) (\s@ModifyCapacityReservation' {} a -> s {additionalInfo = a} :: ModifyCapacityReservation)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyCapacityReservation_dryRun :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Bool)
modifyCapacityReservation_dryRun = Lens.lens (\ModifyCapacityReservation' {dryRun} -> dryRun) (\s@ModifyCapacityReservation' {} a -> s {dryRun = a} :: ModifyCapacityReservation)

-- | The date and time at which the Capacity Reservation expires. When a
-- Capacity Reservation expires, the reserved capacity is released and you
-- can no longer launch instances into it. The Capacity Reservation\'s
-- state changes to @expired@ when it reaches its end date and time.
--
-- The Capacity Reservation is cancelled within an hour from the specified
-- time. For example, if you specify 5\/31\/2019, 13:30:55, the Capacity
-- Reservation is guaranteed to end between 13:30:55 and 14:30:55 on
-- 5\/31\/2019.
--
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@. Omit
-- @EndDate@ if @EndDateType@ is @unlimited@.
modifyCapacityReservation_endDate :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.UTCTime)
modifyCapacityReservation_endDate = Lens.lens (\ModifyCapacityReservation' {endDate} -> endDate) (\s@ModifyCapacityReservation' {} a -> s {endDate = a} :: ModifyCapacityReservation) Prelude.. Lens.mapping Data._Time

-- | Indicates the way in which the Capacity Reservation ends. A Capacity
-- Reservation can have one of the following end types:
--
-- -   @unlimited@ - The Capacity Reservation remains active until you
--     explicitly cancel it. Do not provide an @EndDate@ value if
--     @EndDateType@ is @unlimited@.
--
-- -   @limited@ - The Capacity Reservation expires automatically at a
--     specified date and time. You must provide an @EndDate@ value if
--     @EndDateType@ is @limited@.
modifyCapacityReservation_endDateType :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe EndDateType)
modifyCapacityReservation_endDateType = Lens.lens (\ModifyCapacityReservation' {endDateType} -> endDateType) (\s@ModifyCapacityReservation' {} a -> s {endDateType = a} :: ModifyCapacityReservation)

-- | The number of instances for which to reserve capacity. The number of
-- instances can\'t be increased or decreased by more than @1000@ in a
-- single request.
modifyCapacityReservation_instanceCount :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Int)
modifyCapacityReservation_instanceCount = Lens.lens (\ModifyCapacityReservation' {instanceCount} -> instanceCount) (\s@ModifyCapacityReservation' {} a -> s {instanceCount = a} :: ModifyCapacityReservation)

-- | The ID of the Capacity Reservation.
modifyCapacityReservation_capacityReservationId :: Lens.Lens' ModifyCapacityReservation Prelude.Text
modifyCapacityReservation_capacityReservationId = Lens.lens (\ModifyCapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@ModifyCapacityReservation' {} a -> s {capacityReservationId = a} :: ModifyCapacityReservation)

instance Core.AWSRequest ModifyCapacityReservation where
  type
    AWSResponse ModifyCapacityReservation =
      ModifyCapacityReservationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyCapacityReservationResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCapacityReservation where
  hashWithSalt _salt ModifyCapacityReservation' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` endDateType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` capacityReservationId

instance Prelude.NFData ModifyCapacityReservation where
  rnf ModifyCapacityReservation' {..} =
    Prelude.rnf accept `Prelude.seq`
      Prelude.rnf additionalInfo `Prelude.seq`
        Prelude.rnf dryRun `Prelude.seq`
          Prelude.rnf endDate `Prelude.seq`
            Prelude.rnf endDateType `Prelude.seq`
              Prelude.rnf instanceCount `Prelude.seq`
                Prelude.rnf capacityReservationId

instance Data.ToHeaders ModifyCapacityReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCapacityReservation where
  toQuery ModifyCapacityReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyCapacityReservation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Accept" Data.=: accept,
        "AdditionalInfo" Data.=: additionalInfo,
        "DryRun" Data.=: dryRun,
        "EndDate" Data.=: endDate,
        "EndDateType" Data.=: endDateType,
        "InstanceCount" Data.=: instanceCount,
        "CapacityReservationId"
          Data.=: capacityReservationId
      ]

-- | /See:/ 'newModifyCapacityReservationResponse' smart constructor.
data ModifyCapacityReservationResponse = ModifyCapacityReservationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyCapacityReservationResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyCapacityReservationResponse_httpStatus' - The response's http status code.
newModifyCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCapacityReservationResponse
newModifyCapacityReservationResponse pHttpStatus_ =
  ModifyCapacityReservationResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyCapacityReservationResponse_return :: Lens.Lens' ModifyCapacityReservationResponse (Prelude.Maybe Prelude.Bool)
modifyCapacityReservationResponse_return = Lens.lens (\ModifyCapacityReservationResponse' {return'} -> return') (\s@ModifyCapacityReservationResponse' {} a -> s {return' = a} :: ModifyCapacityReservationResponse)

-- | The response's http status code.
modifyCapacityReservationResponse_httpStatus :: Lens.Lens' ModifyCapacityReservationResponse Prelude.Int
modifyCapacityReservationResponse_httpStatus = Lens.lens (\ModifyCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@ModifyCapacityReservationResponse' {} a -> s {httpStatus = a} :: ModifyCapacityReservationResponse)

instance
  Prelude.NFData
    ModifyCapacityReservationResponse
  where
  rnf ModifyCapacityReservationResponse' {..} =
    Prelude.rnf return' `Prelude.seq`
      Prelude.rnf httpStatus

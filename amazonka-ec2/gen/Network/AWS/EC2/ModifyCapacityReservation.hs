{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ModifyCapacityReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Capacity Reservation\'s capacity and the conditions under
-- which it is to be released. You cannot change a Capacity Reservation\'s
-- instance type, EBS optimization, instance store settings, platform,
-- Availability Zone, or instance eligibility. If you need to modify any of
-- these attributes, we recommend that you cancel the Capacity Reservation,
-- and then create a new one with the required attributes.
module Network.AWS.EC2.ModifyCapacityReservation
  ( -- * Creating a Request
    ModifyCapacityReservation (..),
    newModifyCapacityReservation,

    -- * Request Lenses
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_accept,
    modifyCapacityReservation_endDate,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyCapacityReservation' smart constructor.
data ModifyCapacityReservation = ModifyCapacityReservation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- | Reserved. Capacity Reservations you have created are accepted by
    -- default.
    accept :: Prelude.Maybe Prelude.Bool,
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
    endDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The number of instances for which to reserve capacity.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyCapacityReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'accept', 'modifyCapacityReservation_accept' - Reserved. Capacity Reservations you have created are accepted by
-- default.
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
-- 'instanceCount', 'modifyCapacityReservation_instanceCount' - The number of instances for which to reserve capacity.
--
-- 'capacityReservationId', 'modifyCapacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
newModifyCapacityReservation ::
  -- | 'capacityReservationId'
  Prelude.Text ->
  ModifyCapacityReservation
newModifyCapacityReservation pCapacityReservationId_ =
  ModifyCapacityReservation'
    { dryRun =
        Prelude.Nothing,
      endDateType = Prelude.Nothing,
      accept = Prelude.Nothing,
      endDate = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      capacityReservationId = pCapacityReservationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyCapacityReservation_dryRun :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Bool)
modifyCapacityReservation_dryRun = Lens.lens (\ModifyCapacityReservation' {dryRun} -> dryRun) (\s@ModifyCapacityReservation' {} a -> s {dryRun = a} :: ModifyCapacityReservation)

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

-- | Reserved. Capacity Reservations you have created are accepted by
-- default.
modifyCapacityReservation_accept :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Bool)
modifyCapacityReservation_accept = Lens.lens (\ModifyCapacityReservation' {accept} -> accept) (\s@ModifyCapacityReservation' {} a -> s {accept = a} :: ModifyCapacityReservation)

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
modifyCapacityReservation_endDate = Lens.lens (\ModifyCapacityReservation' {endDate} -> endDate) (\s@ModifyCapacityReservation' {} a -> s {endDate = a} :: ModifyCapacityReservation) Prelude.. Lens.mapping Prelude._Time

-- | The number of instances for which to reserve capacity.
modifyCapacityReservation_instanceCount :: Lens.Lens' ModifyCapacityReservation (Prelude.Maybe Prelude.Int)
modifyCapacityReservation_instanceCount = Lens.lens (\ModifyCapacityReservation' {instanceCount} -> instanceCount) (\s@ModifyCapacityReservation' {} a -> s {instanceCount = a} :: ModifyCapacityReservation)

-- | The ID of the Capacity Reservation.
modifyCapacityReservation_capacityReservationId :: Lens.Lens' ModifyCapacityReservation Prelude.Text
modifyCapacityReservation_capacityReservationId = Lens.lens (\ModifyCapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@ModifyCapacityReservation' {} a -> s {capacityReservationId = a} :: ModifyCapacityReservation)

instance Prelude.AWSRequest ModifyCapacityReservation where
  type
    Rs ModifyCapacityReservation =
      ModifyCapacityReservationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyCapacityReservationResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCapacityReservation

instance Prelude.NFData ModifyCapacityReservation

instance Prelude.ToHeaders ModifyCapacityReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyCapacityReservation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyCapacityReservation where
  toQuery ModifyCapacityReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyCapacityReservation" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "EndDateType" Prelude.=: endDateType,
        "Accept" Prelude.=: accept,
        "EndDate" Prelude.=: endDate,
        "InstanceCount" Prelude.=: instanceCount,
        "CapacityReservationId"
          Prelude.=: capacityReservationId
      ]

-- | /See:/ 'newModifyCapacityReservationResponse' smart constructor.
data ModifyCapacityReservationResponse = ModifyCapacityReservationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

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
-- Module      : Network.AWS.EC2.CancelCapacityReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Capacity Reservation, releases the reserved
-- capacity, and changes the Capacity Reservation\'s state to @cancelled@.
--
-- Instances running in the reserved capacity continue running until you
-- stop them. Stopped instances that target the Capacity Reservation can no
-- longer launch. Modify these instances to either target a different
-- Capacity Reservation, launch On-Demand Instance capacity, or run in any
-- open Capacity Reservation that has matching attributes and sufficient
-- capacity.
module Network.AWS.EC2.CancelCapacityReservation
  ( -- * Creating a Request
    CancelCapacityReservation (..),
    newCancelCapacityReservation,

    -- * Request Lenses
    cancelCapacityReservation_dryRun,
    cancelCapacityReservation_capacityReservationId,

    -- * Destructuring the Response
    CancelCapacityReservationResponse (..),
    newCancelCapacityReservationResponse,

    -- * Response Lenses
    cancelCapacityReservationResponse_return,
    cancelCapacityReservationResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelCapacityReservation' smart constructor.
data CancelCapacityReservation = CancelCapacityReservation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Capacity Reservation to be cancelled.
    capacityReservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelCapacityReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'capacityReservationId', 'cancelCapacityReservation_capacityReservationId' - The ID of the Capacity Reservation to be cancelled.
newCancelCapacityReservation ::
  -- | 'capacityReservationId'
  Prelude.Text ->
  CancelCapacityReservation
newCancelCapacityReservation pCapacityReservationId_ =
  CancelCapacityReservation'
    { dryRun =
        Prelude.Nothing,
      capacityReservationId = pCapacityReservationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelCapacityReservation_dryRun :: Lens.Lens' CancelCapacityReservation (Prelude.Maybe Prelude.Bool)
cancelCapacityReservation_dryRun = Lens.lens (\CancelCapacityReservation' {dryRun} -> dryRun) (\s@CancelCapacityReservation' {} a -> s {dryRun = a} :: CancelCapacityReservation)

-- | The ID of the Capacity Reservation to be cancelled.
cancelCapacityReservation_capacityReservationId :: Lens.Lens' CancelCapacityReservation Prelude.Text
cancelCapacityReservation_capacityReservationId = Lens.lens (\CancelCapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@CancelCapacityReservation' {} a -> s {capacityReservationId = a} :: CancelCapacityReservation)

instance Prelude.AWSRequest CancelCapacityReservation where
  type
    Rs CancelCapacityReservation =
      CancelCapacityReservationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelCapacityReservationResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelCapacityReservation

instance Prelude.NFData CancelCapacityReservation

instance Prelude.ToHeaders CancelCapacityReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelCapacityReservation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelCapacityReservation where
  toQuery CancelCapacityReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelCapacityReservation" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "CapacityReservationId"
          Prelude.=: capacityReservationId
      ]

-- | /See:/ 'newCancelCapacityReservationResponse' smart constructor.
data CancelCapacityReservationResponse = CancelCapacityReservationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'cancelCapacityReservationResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'cancelCapacityReservationResponse_httpStatus' - The response's http status code.
newCancelCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelCapacityReservationResponse
newCancelCapacityReservationResponse pHttpStatus_ =
  CancelCapacityReservationResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
cancelCapacityReservationResponse_return :: Lens.Lens' CancelCapacityReservationResponse (Prelude.Maybe Prelude.Bool)
cancelCapacityReservationResponse_return = Lens.lens (\CancelCapacityReservationResponse' {return'} -> return') (\s@CancelCapacityReservationResponse' {} a -> s {return' = a} :: CancelCapacityReservationResponse)

-- | The response's http status code.
cancelCapacityReservationResponse_httpStatus :: Lens.Lens' CancelCapacityReservationResponse Prelude.Int
cancelCapacityReservationResponse_httpStatus = Lens.lens (\CancelCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@CancelCapacityReservationResponse' {} a -> s {httpStatus = a} :: CancelCapacityReservationResponse)

instance
  Prelude.NFData
    CancelCapacityReservationResponse

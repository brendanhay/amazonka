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
-- Module      : Network.AWS.Snowball.UpdateJobShipmentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state when a the shipment states changes to a different
-- state.
module Network.AWS.Snowball.UpdateJobShipmentState
  ( -- * Creating a Request
    UpdateJobShipmentState (..),
    newUpdateJobShipmentState,

    -- * Request Lenses
    updateJobShipmentState_jobId,
    updateJobShipmentState_shipmentState,

    -- * Destructuring the Response
    UpdateJobShipmentStateResponse (..),
    newUpdateJobShipmentStateResponse,

    -- * Response Lenses
    updateJobShipmentStateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newUpdateJobShipmentState' smart constructor.
data UpdateJobShipmentState = UpdateJobShipmentState'
  { -- | The job ID of the job whose shipment date you want to update, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text,
    -- | The state of a device when it is being shipped.
    --
    -- Set to @RECEIVED@ when the device arrives at your location.
    --
    -- Set to @RETURNED@ when you have returned the device to AWS.
    shipmentState :: ShipmentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobShipmentState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'updateJobShipmentState_jobId' - The job ID of the job whose shipment date you want to update, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'shipmentState', 'updateJobShipmentState_shipmentState' - The state of a device when it is being shipped.
--
-- Set to @RECEIVED@ when the device arrives at your location.
--
-- Set to @RETURNED@ when you have returned the device to AWS.
newUpdateJobShipmentState ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'shipmentState'
  ShipmentState ->
  UpdateJobShipmentState
newUpdateJobShipmentState pJobId_ pShipmentState_ =
  UpdateJobShipmentState'
    { jobId = pJobId_,
      shipmentState = pShipmentState_
    }

-- | The job ID of the job whose shipment date you want to update, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
updateJobShipmentState_jobId :: Lens.Lens' UpdateJobShipmentState Prelude.Text
updateJobShipmentState_jobId = Lens.lens (\UpdateJobShipmentState' {jobId} -> jobId) (\s@UpdateJobShipmentState' {} a -> s {jobId = a} :: UpdateJobShipmentState)

-- | The state of a device when it is being shipped.
--
-- Set to @RECEIVED@ when the device arrives at your location.
--
-- Set to @RETURNED@ when you have returned the device to AWS.
updateJobShipmentState_shipmentState :: Lens.Lens' UpdateJobShipmentState ShipmentState
updateJobShipmentState_shipmentState = Lens.lens (\UpdateJobShipmentState' {shipmentState} -> shipmentState) (\s@UpdateJobShipmentState' {} a -> s {shipmentState = a} :: UpdateJobShipmentState)

instance Prelude.AWSRequest UpdateJobShipmentState where
  type
    Rs UpdateJobShipmentState =
      UpdateJobShipmentStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateJobShipmentStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobShipmentState

instance Prelude.NFData UpdateJobShipmentState

instance Prelude.ToHeaders UpdateJobShipmentState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.UpdateJobShipmentState" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateJobShipmentState where
  toJSON UpdateJobShipmentState' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobId" Prelude..= jobId),
            Prelude.Just
              ("ShipmentState" Prelude..= shipmentState)
          ]
      )

instance Prelude.ToPath UpdateJobShipmentState where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateJobShipmentState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobShipmentStateResponse' smart constructor.
data UpdateJobShipmentStateResponse = UpdateJobShipmentStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobShipmentStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateJobShipmentStateResponse_httpStatus' - The response's http status code.
newUpdateJobShipmentStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobShipmentStateResponse
newUpdateJobShipmentStateResponse pHttpStatus_ =
  UpdateJobShipmentStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateJobShipmentStateResponse_httpStatus :: Lens.Lens' UpdateJobShipmentStateResponse Prelude.Int
updateJobShipmentStateResponse_httpStatus = Lens.lens (\UpdateJobShipmentStateResponse' {httpStatus} -> httpStatus) (\s@UpdateJobShipmentStateResponse' {} a -> s {httpStatus = a} :: UpdateJobShipmentStateResponse)

instance
  Prelude.NFData
    UpdateJobShipmentStateResponse

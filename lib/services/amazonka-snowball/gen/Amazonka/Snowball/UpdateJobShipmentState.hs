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
-- Module      : Amazonka.Snowball.UpdateJobShipmentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state when a shipment state changes to a different state.
module Amazonka.Snowball.UpdateJobShipmentState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newUpdateJobShipmentState' smart constructor.
data UpdateJobShipmentState = UpdateJobShipmentState'
  { -- | The job ID of the job whose shipment date you want to update, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text,
    -- | The state of a device when it is being shipped.
    --
    -- Set to @RECEIVED@ when the device arrives at your location.
    --
    -- Set to @RETURNED@ when you have returned the device to Amazon Web
    -- Services.
    shipmentState :: ShipmentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Set to @RETURNED@ when you have returned the device to Amazon Web
-- Services.
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
-- Set to @RETURNED@ when you have returned the device to Amazon Web
-- Services.
updateJobShipmentState_shipmentState :: Lens.Lens' UpdateJobShipmentState ShipmentState
updateJobShipmentState_shipmentState = Lens.lens (\UpdateJobShipmentState' {shipmentState} -> shipmentState) (\s@UpdateJobShipmentState' {} a -> s {shipmentState = a} :: UpdateJobShipmentState)

instance Core.AWSRequest UpdateJobShipmentState where
  type
    AWSResponse UpdateJobShipmentState =
      UpdateJobShipmentStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateJobShipmentStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobShipmentState where
  hashWithSalt _salt UpdateJobShipmentState' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` shipmentState

instance Prelude.NFData UpdateJobShipmentState where
  rnf UpdateJobShipmentState' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf shipmentState

instance Data.ToHeaders UpdateJobShipmentState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.UpdateJobShipmentState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateJobShipmentState where
  toJSON UpdateJobShipmentState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobId" Data..= jobId),
            Prelude.Just
              ("ShipmentState" Data..= shipmentState)
          ]
      )

instance Data.ToPath UpdateJobShipmentState where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateJobShipmentState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobShipmentStateResponse' smart constructor.
data UpdateJobShipmentStateResponse = UpdateJobShipmentStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateJobShipmentStateResponse' {..} =
    Prelude.rnf httpStatus

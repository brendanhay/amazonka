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
-- Module      : Amazonka.IoTFleetWise.UpdateFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an existing fleet.
--
-- If the fleet is successfully updated, Amazon Web Services IoT FleetWise
-- sends back an HTTP 200 response with an empty HTTP body.
module Amazonka.IoTFleetWise.UpdateFleet
  ( -- * Creating a Request
    UpdateFleet (..),
    newUpdateFleet,

    -- * Request Lenses
    updateFleet_description,
    updateFleet_fleetId,

    -- * Destructuring the Response
    UpdateFleetResponse (..),
    newUpdateFleetResponse,

    -- * Response Lenses
    updateFleetResponse_arn,
    updateFleetResponse_id,
    updateFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFleet' smart constructor.
data UpdateFleet = UpdateFleet'
  { -- | An updated description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the fleet to update.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateFleet_description' - An updated description of the fleet.
--
-- 'fleetId', 'updateFleet_fleetId' - The ID of the fleet to update.
newUpdateFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  UpdateFleet
newUpdateFleet pFleetId_ =
  UpdateFleet'
    { description = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | An updated description of the fleet.
updateFleet_description :: Lens.Lens' UpdateFleet (Prelude.Maybe Prelude.Text)
updateFleet_description = Lens.lens (\UpdateFleet' {description} -> description) (\s@UpdateFleet' {} a -> s {description = a} :: UpdateFleet)

-- | The ID of the fleet to update.
updateFleet_fleetId :: Lens.Lens' UpdateFleet Prelude.Text
updateFleet_fleetId = Lens.lens (\UpdateFleet' {fleetId} -> fleetId) (\s@UpdateFleet' {} a -> s {fleetId = a} :: UpdateFleet)

instance Core.AWSRequest UpdateFleet where
  type AWSResponse UpdateFleet = UpdateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleet where
  hashWithSalt _salt UpdateFleet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData UpdateFleet where
  rnf UpdateFleet' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf fleetId

instance Data.ToHeaders UpdateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.UpdateFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFleet where
  toJSON UpdateFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("fleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath UpdateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetResponse' smart constructor.
data UpdateFleetResponse = UpdateFleetResponse'
  { -- | The Amazon Resource Name (ARN) of the updated fleet.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the updated fleet.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateFleetResponse_arn' - The Amazon Resource Name (ARN) of the updated fleet.
--
-- 'id', 'updateFleetResponse_id' - The ID of the updated fleet.
--
-- 'httpStatus', 'updateFleetResponse_httpStatus' - The response's http status code.
newUpdateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFleetResponse
newUpdateFleetResponse pHttpStatus_ =
  UpdateFleetResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated fleet.
updateFleetResponse_arn :: Lens.Lens' UpdateFleetResponse (Prelude.Maybe Prelude.Text)
updateFleetResponse_arn = Lens.lens (\UpdateFleetResponse' {arn} -> arn) (\s@UpdateFleetResponse' {} a -> s {arn = a} :: UpdateFleetResponse)

-- | The ID of the updated fleet.
updateFleetResponse_id :: Lens.Lens' UpdateFleetResponse (Prelude.Maybe Prelude.Text)
updateFleetResponse_id = Lens.lens (\UpdateFleetResponse' {id} -> id) (\s@UpdateFleetResponse' {} a -> s {id = a} :: UpdateFleetResponse)

-- | The response's http status code.
updateFleetResponse_httpStatus :: Lens.Lens' UpdateFleetResponse Prelude.Int
updateFleetResponse_httpStatus = Lens.lens (\UpdateFleetResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetResponse' {} a -> s {httpStatus = a} :: UpdateFleetResponse)

instance Prelude.NFData UpdateFleetResponse where
  rnf UpdateFleetResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf httpStatus

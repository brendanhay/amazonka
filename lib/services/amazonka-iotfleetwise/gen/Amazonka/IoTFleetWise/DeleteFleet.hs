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
-- Module      : Amazonka.IoTFleetWise.DeleteFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet. Before you delete a fleet, all vehicles must be
-- dissociated from the fleet. For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/delete-fleet-cli.html Delete a fleet (AWS CLI)>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
--
-- If the fleet is successfully deleted, Amazon Web Services IoT FleetWise
-- sends back an HTTP 200 response with an empty body.
module Amazonka.IoTFleetWise.DeleteFleet
  ( -- * Creating a Request
    DeleteFleet (..),
    newDeleteFleet,

    -- * Request Lenses
    deleteFleet_fleetId,

    -- * Destructuring the Response
    DeleteFleetResponse (..),
    newDeleteFleetResponse,

    -- * Response Lenses
    deleteFleetResponse_arn,
    deleteFleetResponse_id,
    deleteFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | The ID of the fleet to delete.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteFleet_fleetId' - The ID of the fleet to delete.
newDeleteFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  DeleteFleet
newDeleteFleet pFleetId_ =
  DeleteFleet' {fleetId = pFleetId_}

-- | The ID of the fleet to delete.
deleteFleet_fleetId :: Lens.Lens' DeleteFleet Prelude.Text
deleteFleet_fleetId = Lens.lens (\DeleteFleet' {fleetId} -> fleetId) (\s@DeleteFleet' {} a -> s {fleetId = a} :: DeleteFleet)

instance Core.AWSRequest DeleteFleet where
  type AWSResponse DeleteFleet = DeleteFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFleetResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFleet where
  hashWithSalt _salt DeleteFleet' {..} =
    _salt `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DeleteFleet where
  rnf DeleteFleet' {..} = Prelude.rnf fleetId

instance Data.ToHeaders DeleteFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.DeleteFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fleetId" Data..= fleetId)]
      )

instance Data.ToPath DeleteFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted fleet.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deleted fleet.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteFleetResponse_arn' - The Amazon Resource Name (ARN) of the deleted fleet.
--
-- 'id', 'deleteFleetResponse_id' - The ID of the deleted fleet.
--
-- 'httpStatus', 'deleteFleetResponse_httpStatus' - The response's http status code.
newDeleteFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFleetResponse
newDeleteFleetResponse pHttpStatus_ =
  DeleteFleetResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted fleet.
deleteFleetResponse_arn :: Lens.Lens' DeleteFleetResponse (Prelude.Maybe Prelude.Text)
deleteFleetResponse_arn = Lens.lens (\DeleteFleetResponse' {arn} -> arn) (\s@DeleteFleetResponse' {} a -> s {arn = a} :: DeleteFleetResponse)

-- | The ID of the deleted fleet.
deleteFleetResponse_id :: Lens.Lens' DeleteFleetResponse (Prelude.Maybe Prelude.Text)
deleteFleetResponse_id = Lens.lens (\DeleteFleetResponse' {id} -> id) (\s@DeleteFleetResponse' {} a -> s {id = a} :: DeleteFleetResponse)

-- | The response's http status code.
deleteFleetResponse_httpStatus :: Lens.Lens' DeleteFleetResponse Prelude.Int
deleteFleetResponse_httpStatus = Lens.lens (\DeleteFleetResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetResponse' {} a -> s {httpStatus = a} :: DeleteFleetResponse)

instance Prelude.NFData DeleteFleetResponse where
  rnf DeleteFleetResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus

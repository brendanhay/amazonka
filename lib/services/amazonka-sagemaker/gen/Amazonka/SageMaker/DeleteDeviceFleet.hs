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
-- Module      : Amazonka.SageMaker.DeleteDeviceFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet.
module Amazonka.SageMaker.DeleteDeviceFleet
  ( -- * Creating a Request
    DeleteDeviceFleet (..),
    newDeleteDeviceFleet,

    -- * Request Lenses
    deleteDeviceFleet_deviceFleetName,

    -- * Destructuring the Response
    DeleteDeviceFleetResponse (..),
    newDeleteDeviceFleetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteDeviceFleet' smart constructor.
data DeleteDeviceFleet = DeleteDeviceFleet'
  { -- | The name of the fleet to delete.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'deleteDeviceFleet_deviceFleetName' - The name of the fleet to delete.
newDeleteDeviceFleet ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  DeleteDeviceFleet
newDeleteDeviceFleet pDeviceFleetName_ =
  DeleteDeviceFleet'
    { deviceFleetName =
        pDeviceFleetName_
    }

-- | The name of the fleet to delete.
deleteDeviceFleet_deviceFleetName :: Lens.Lens' DeleteDeviceFleet Prelude.Text
deleteDeviceFleet_deviceFleetName = Lens.lens (\DeleteDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@DeleteDeviceFleet' {} a -> s {deviceFleetName = a} :: DeleteDeviceFleet)

instance Core.AWSRequest DeleteDeviceFleet where
  type
    AWSResponse DeleteDeviceFleet =
      DeleteDeviceFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDeviceFleetResponse'

instance Prelude.Hashable DeleteDeviceFleet where
  hashWithSalt _salt DeleteDeviceFleet' {..} =
    _salt `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData DeleteDeviceFleet where
  rnf DeleteDeviceFleet' {..} =
    Prelude.rnf deviceFleetName

instance Core.ToHeaders DeleteDeviceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteDeviceFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDeviceFleet where
  toJSON DeleteDeviceFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath DeleteDeviceFleet where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDeviceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceFleetResponse' smart constructor.
data DeleteDeviceFleetResponse = DeleteDeviceFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeviceFleetResponse ::
  DeleteDeviceFleetResponse
newDeleteDeviceFleetResponse =
  DeleteDeviceFleetResponse'

instance Prelude.NFData DeleteDeviceFleetResponse where
  rnf _ = ()

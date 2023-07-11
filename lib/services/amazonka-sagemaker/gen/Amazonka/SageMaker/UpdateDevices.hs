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
-- Module      : Amazonka.SageMaker.UpdateDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more devices in a fleet.
module Amazonka.SageMaker.UpdateDevices
  ( -- * Creating a Request
    UpdateDevices (..),
    newUpdateDevices,

    -- * Request Lenses
    updateDevices_deviceFleetName,
    updateDevices_devices,

    -- * Destructuring the Response
    UpdateDevicesResponse (..),
    newUpdateDevicesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateDevices' smart constructor.
data UpdateDevices = UpdateDevices'
  { -- | The name of the fleet the devices belong to.
    deviceFleetName :: Prelude.Text,
    -- | List of devices to register with Edge Manager agent.
    devices :: [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'updateDevices_deviceFleetName' - The name of the fleet the devices belong to.
--
-- 'devices', 'updateDevices_devices' - List of devices to register with Edge Manager agent.
newUpdateDevices ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  UpdateDevices
newUpdateDevices pDeviceFleetName_ =
  UpdateDevices'
    { deviceFleetName = pDeviceFleetName_,
      devices = Prelude.mempty
    }

-- | The name of the fleet the devices belong to.
updateDevices_deviceFleetName :: Lens.Lens' UpdateDevices Prelude.Text
updateDevices_deviceFleetName = Lens.lens (\UpdateDevices' {deviceFleetName} -> deviceFleetName) (\s@UpdateDevices' {} a -> s {deviceFleetName = a} :: UpdateDevices)

-- | List of devices to register with Edge Manager agent.
updateDevices_devices :: Lens.Lens' UpdateDevices [Device]
updateDevices_devices = Lens.lens (\UpdateDevices' {devices} -> devices) (\s@UpdateDevices' {} a -> s {devices = a} :: UpdateDevices) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDevices where
  type
    AWSResponse UpdateDevices =
      UpdateDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDevicesResponse'

instance Prelude.Hashable UpdateDevices where
  hashWithSalt _salt UpdateDevices' {..} =
    _salt
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` devices

instance Prelude.NFData UpdateDevices where
  rnf UpdateDevices' {..} =
    Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf devices

instance Data.ToHeaders UpdateDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateDevices" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDevices where
  toJSON UpdateDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName),
            Prelude.Just ("Devices" Data..= devices)
          ]
      )

instance Data.ToPath UpdateDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDevicesResponse' smart constructor.
data UpdateDevicesResponse = UpdateDevicesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDevicesResponse ::
  UpdateDevicesResponse
newUpdateDevicesResponse = UpdateDevicesResponse'

instance Prelude.NFData UpdateDevicesResponse where
  rnf _ = ()

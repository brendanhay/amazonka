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
-- Module      : Network.AWS.SageMaker.UpdateDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more devices in a fleet.
module Network.AWS.SageMaker.UpdateDevices
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateDevices' smart constructor.
data UpdateDevices = UpdateDevices'
  { -- | The name of the fleet the devices belong to.
    deviceFleetName :: Prelude.Text,
    -- | List of devices to register with Edge Manager agent.
    devices :: [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateDevices_devices = Lens.lens (\UpdateDevices' {devices} -> devices) (\s@UpdateDevices' {} a -> s {devices = a} :: UpdateDevices) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UpdateDevices where
  type Rs UpdateDevices = UpdateDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateDevicesResponse'

instance Prelude.Hashable UpdateDevices

instance Prelude.NFData UpdateDevices

instance Prelude.ToHeaders UpdateDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdateDevices" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDevices where
  toJSON UpdateDevices' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Prelude..= deviceFleetName),
            Prelude.Just ("Devices" Prelude..= devices)
          ]
      )

instance Prelude.ToPath UpdateDevices where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDevicesResponse' smart constructor.
data UpdateDevicesResponse = UpdateDevicesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDevicesResponse ::
  UpdateDevicesResponse
newUpdateDevicesResponse = UpdateDevicesResponse'

instance Prelude.NFData UpdateDevicesResponse

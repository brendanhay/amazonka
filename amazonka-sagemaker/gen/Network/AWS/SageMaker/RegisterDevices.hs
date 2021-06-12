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
-- Module      : Network.AWS.SageMaker.RegisterDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register devices.
module Network.AWS.SageMaker.RegisterDevices
  ( -- * Creating a Request
    RegisterDevices (..),
    newRegisterDevices,

    -- * Request Lenses
    registerDevices_tags,
    registerDevices_deviceFleetName,
    registerDevices_devices,

    -- * Destructuring the Response
    RegisterDevicesResponse (..),
    newRegisterDevicesResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newRegisterDevices' smart constructor.
data RegisterDevices = RegisterDevices'
  { -- | The tags associated with devices.
    tags :: Core.Maybe [Tag],
    -- | The name of the fleet.
    deviceFleetName :: Core.Text,
    -- | A list of devices to register with SageMaker Edge Manager.
    devices :: [Device]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'registerDevices_tags' - The tags associated with devices.
--
-- 'deviceFleetName', 'registerDevices_deviceFleetName' - The name of the fleet.
--
-- 'devices', 'registerDevices_devices' - A list of devices to register with SageMaker Edge Manager.
newRegisterDevices ::
  -- | 'deviceFleetName'
  Core.Text ->
  RegisterDevices
newRegisterDevices pDeviceFleetName_ =
  RegisterDevices'
    { tags = Core.Nothing,
      deviceFleetName = pDeviceFleetName_,
      devices = Core.mempty
    }

-- | The tags associated with devices.
registerDevices_tags :: Lens.Lens' RegisterDevices (Core.Maybe [Tag])
registerDevices_tags = Lens.lens (\RegisterDevices' {tags} -> tags) (\s@RegisterDevices' {} a -> s {tags = a} :: RegisterDevices) Core.. Lens.mapping Lens._Coerce

-- | The name of the fleet.
registerDevices_deviceFleetName :: Lens.Lens' RegisterDevices Core.Text
registerDevices_deviceFleetName = Lens.lens (\RegisterDevices' {deviceFleetName} -> deviceFleetName) (\s@RegisterDevices' {} a -> s {deviceFleetName = a} :: RegisterDevices)

-- | A list of devices to register with SageMaker Edge Manager.
registerDevices_devices :: Lens.Lens' RegisterDevices [Device]
registerDevices_devices = Lens.lens (\RegisterDevices' {devices} -> devices) (\s@RegisterDevices' {} a -> s {devices = a} :: RegisterDevices) Core.. Lens._Coerce

instance Core.AWSRequest RegisterDevices where
  type
    AWSResponse RegisterDevices =
      RegisterDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RegisterDevicesResponse'

instance Core.Hashable RegisterDevices

instance Core.NFData RegisterDevices

instance Core.ToHeaders RegisterDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.RegisterDevices" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterDevices where
  toJSON RegisterDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ("DeviceFleetName" Core..= deviceFleetName),
            Core.Just ("Devices" Core..= devices)
          ]
      )

instance Core.ToPath RegisterDevices where
  toPath = Core.const "/"

instance Core.ToQuery RegisterDevices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterDevicesResponse' smart constructor.
data RegisterDevicesResponse = RegisterDevicesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterDevicesResponse ::
  RegisterDevicesResponse
newRegisterDevicesResponse = RegisterDevicesResponse'

instance Core.NFData RegisterDevicesResponse

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
-- Module      : Network.AWS.SageMaker.DeregisterDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified devices. After you deregister a device, you
-- will need to re-register the devices.
module Network.AWS.SageMaker.DeregisterDevices
  ( -- * Creating a Request
    DeregisterDevices (..),
    newDeregisterDevices,

    -- * Request Lenses
    deregisterDevices_deviceFleetName,
    deregisterDevices_deviceNames,

    -- * Destructuring the Response
    DeregisterDevicesResponse (..),
    newDeregisterDevicesResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeregisterDevices' smart constructor.
data DeregisterDevices = DeregisterDevices'
  { -- | The name of the fleet the devices belong to.
    deviceFleetName :: Core.Text,
    -- | The unique IDs of the devices.
    deviceNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'deregisterDevices_deviceFleetName' - The name of the fleet the devices belong to.
--
-- 'deviceNames', 'deregisterDevices_deviceNames' - The unique IDs of the devices.
newDeregisterDevices ::
  -- | 'deviceFleetName'
  Core.Text ->
  DeregisterDevices
newDeregisterDevices pDeviceFleetName_ =
  DeregisterDevices'
    { deviceFleetName =
        pDeviceFleetName_,
      deviceNames = Core.mempty
    }

-- | The name of the fleet the devices belong to.
deregisterDevices_deviceFleetName :: Lens.Lens' DeregisterDevices Core.Text
deregisterDevices_deviceFleetName = Lens.lens (\DeregisterDevices' {deviceFleetName} -> deviceFleetName) (\s@DeregisterDevices' {} a -> s {deviceFleetName = a} :: DeregisterDevices)

-- | The unique IDs of the devices.
deregisterDevices_deviceNames :: Lens.Lens' DeregisterDevices [Core.Text]
deregisterDevices_deviceNames = Lens.lens (\DeregisterDevices' {deviceNames} -> deviceNames) (\s@DeregisterDevices' {} a -> s {deviceNames = a} :: DeregisterDevices) Core.. Lens._Coerce

instance Core.AWSRequest DeregisterDevices where
  type
    AWSResponse DeregisterDevices =
      DeregisterDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeregisterDevicesResponse'

instance Core.Hashable DeregisterDevices

instance Core.NFData DeregisterDevices

instance Core.ToHeaders DeregisterDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeregisterDevices" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterDevices where
  toJSON DeregisterDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeviceFleetName" Core..= deviceFleetName),
            Core.Just ("DeviceNames" Core..= deviceNames)
          ]
      )

instance Core.ToPath DeregisterDevices where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterDevices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterDevicesResponse' smart constructor.
data DeregisterDevicesResponse = DeregisterDevicesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterDevicesResponse ::
  DeregisterDevicesResponse
newDeregisterDevicesResponse =
  DeregisterDevicesResponse'

instance Core.NFData DeregisterDevicesResponse

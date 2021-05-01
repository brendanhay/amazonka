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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newRegisterDevices' smart constructor.
data RegisterDevices = RegisterDevices'
  { -- | The tags associated with devices.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text,
    -- | A list of devices to register with SageMaker Edge Manager.
    devices :: [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  RegisterDevices
newRegisterDevices pDeviceFleetName_ =
  RegisterDevices'
    { tags = Prelude.Nothing,
      deviceFleetName = pDeviceFleetName_,
      devices = Prelude.mempty
    }

-- | The tags associated with devices.
registerDevices_tags :: Lens.Lens' RegisterDevices (Prelude.Maybe [Tag])
registerDevices_tags = Lens.lens (\RegisterDevices' {tags} -> tags) (\s@RegisterDevices' {} a -> s {tags = a} :: RegisterDevices) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the fleet.
registerDevices_deviceFleetName :: Lens.Lens' RegisterDevices Prelude.Text
registerDevices_deviceFleetName = Lens.lens (\RegisterDevices' {deviceFleetName} -> deviceFleetName) (\s@RegisterDevices' {} a -> s {deviceFleetName = a} :: RegisterDevices)

-- | A list of devices to register with SageMaker Edge Manager.
registerDevices_devices :: Lens.Lens' RegisterDevices [Device]
registerDevices_devices = Lens.lens (\RegisterDevices' {devices} -> devices) (\s@RegisterDevices' {} a -> s {devices = a} :: RegisterDevices) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RegisterDevices where
  type Rs RegisterDevices = RegisterDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RegisterDevicesResponse'

instance Prelude.Hashable RegisterDevices

instance Prelude.NFData RegisterDevices

instance Prelude.ToHeaders RegisterDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.RegisterDevices" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterDevices where
  toJSON RegisterDevices' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("DeviceFleetName" Prelude..= deviceFleetName),
            Prelude.Just ("Devices" Prelude..= devices)
          ]
      )

instance Prelude.ToPath RegisterDevices where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDevicesResponse' smart constructor.
data RegisterDevicesResponse = RegisterDevicesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterDevicesResponse ::
  RegisterDevicesResponse
newRegisterDevicesResponse = RegisterDevicesResponse'

instance Prelude.NFData RegisterDevicesResponse

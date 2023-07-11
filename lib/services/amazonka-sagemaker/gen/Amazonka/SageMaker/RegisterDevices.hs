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
-- Module      : Amazonka.SageMaker.RegisterDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register devices.
module Amazonka.SageMaker.RegisterDevices
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newRegisterDevices' smart constructor.
data RegisterDevices = RegisterDevices'
  { -- | The tags associated with devices.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text,
    -- | A list of devices to register with SageMaker Edge Manager.
    devices :: [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
registerDevices_tags = Lens.lens (\RegisterDevices' {tags} -> tags) (\s@RegisterDevices' {} a -> s {tags = a} :: RegisterDevices) Prelude.. Lens.mapping Lens.coerced

-- | The name of the fleet.
registerDevices_deviceFleetName :: Lens.Lens' RegisterDevices Prelude.Text
registerDevices_deviceFleetName = Lens.lens (\RegisterDevices' {deviceFleetName} -> deviceFleetName) (\s@RegisterDevices' {} a -> s {deviceFleetName = a} :: RegisterDevices)

-- | A list of devices to register with SageMaker Edge Manager.
registerDevices_devices :: Lens.Lens' RegisterDevices [Device]
registerDevices_devices = Lens.lens (\RegisterDevices' {devices} -> devices) (\s@RegisterDevices' {} a -> s {devices = a} :: RegisterDevices) Prelude.. Lens.coerced

instance Core.AWSRequest RegisterDevices where
  type
    AWSResponse RegisterDevices =
      RegisterDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RegisterDevicesResponse'

instance Prelude.Hashable RegisterDevices where
  hashWithSalt _salt RegisterDevices' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` devices

instance Prelude.NFData RegisterDevices where
  rnf RegisterDevices' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf devices

instance Data.ToHeaders RegisterDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.RegisterDevices" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterDevices where
  toJSON RegisterDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName),
            Prelude.Just ("Devices" Data..= devices)
          ]
      )

instance Data.ToPath RegisterDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDevicesResponse' smart constructor.
data RegisterDevicesResponse = RegisterDevicesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterDevicesResponse ::
  RegisterDevicesResponse
newRegisterDevicesResponse = RegisterDevicesResponse'

instance Prelude.NFData RegisterDevicesResponse where
  rnf _ = ()

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
-- Module      : Amazonka.SageMaker.DeregisterDevices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified devices. After you deregister a device, you
-- will need to re-register the devices.
module Amazonka.SageMaker.DeregisterDevices
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeregisterDevices' smart constructor.
data DeregisterDevices = DeregisterDevices'
  { -- | The name of the fleet the devices belong to.
    deviceFleetName :: Prelude.Text,
    -- | The unique IDs of the devices.
    deviceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeregisterDevices
newDeregisterDevices pDeviceFleetName_ =
  DeregisterDevices'
    { deviceFleetName =
        pDeviceFleetName_,
      deviceNames = Prelude.mempty
    }

-- | The name of the fleet the devices belong to.
deregisterDevices_deviceFleetName :: Lens.Lens' DeregisterDevices Prelude.Text
deregisterDevices_deviceFleetName = Lens.lens (\DeregisterDevices' {deviceFleetName} -> deviceFleetName) (\s@DeregisterDevices' {} a -> s {deviceFleetName = a} :: DeregisterDevices)

-- | The unique IDs of the devices.
deregisterDevices_deviceNames :: Lens.Lens' DeregisterDevices [Prelude.Text]
deregisterDevices_deviceNames = Lens.lens (\DeregisterDevices' {deviceNames} -> deviceNames) (\s@DeregisterDevices' {} a -> s {deviceNames = a} :: DeregisterDevices) Prelude.. Lens.coerced

instance Core.AWSRequest DeregisterDevices where
  type
    AWSResponse DeregisterDevices =
      DeregisterDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeregisterDevicesResponse'

instance Prelude.Hashable DeregisterDevices where
  hashWithSalt _salt DeregisterDevices' {..} =
    _salt `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` deviceNames

instance Prelude.NFData DeregisterDevices where
  rnf DeregisterDevices' {..} =
    Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf deviceNames

instance Data.ToHeaders DeregisterDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeregisterDevices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterDevices where
  toJSON DeregisterDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName),
            Prelude.Just ("DeviceNames" Data..= deviceNames)
          ]
      )

instance Data.ToPath DeregisterDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterDevicesResponse' smart constructor.
data DeregisterDevicesResponse = DeregisterDevicesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterDevicesResponse ::
  DeregisterDevicesResponse
newDeregisterDevicesResponse =
  DeregisterDevicesResponse'

instance Prelude.NFData DeregisterDevicesResponse where
  rnf _ = ()

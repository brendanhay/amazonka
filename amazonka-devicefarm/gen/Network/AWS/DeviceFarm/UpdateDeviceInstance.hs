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
-- Module      : Network.AWS.DeviceFarm.UpdateDeviceInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about a private device instance.
module Network.AWS.DeviceFarm.UpdateDeviceInstance
  ( -- * Creating a Request
    UpdateDeviceInstance (..),
    newUpdateDeviceInstance,

    -- * Request Lenses
    updateDeviceInstance_labels,
    updateDeviceInstance_profileArn,
    updateDeviceInstance_arn,

    -- * Destructuring the Response
    UpdateDeviceInstanceResponse (..),
    newUpdateDeviceInstanceResponse,

    -- * Response Lenses
    updateDeviceInstanceResponse_deviceInstance,
    updateDeviceInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDeviceInstance' smart constructor.
data UpdateDeviceInstance = UpdateDeviceInstance'
  { -- | An array of strings that you want to associate with the device instance.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the profile that you want to associate with the device
    -- instance.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device instance.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'updateDeviceInstance_labels' - An array of strings that you want to associate with the device instance.
--
-- 'profileArn', 'updateDeviceInstance_profileArn' - The ARN of the profile that you want to associate with the device
-- instance.
--
-- 'arn', 'updateDeviceInstance_arn' - The Amazon Resource Name (ARN) of the device instance.
newUpdateDeviceInstance ::
  -- | 'arn'
  Prelude.Text ->
  UpdateDeviceInstance
newUpdateDeviceInstance pArn_ =
  UpdateDeviceInstance'
    { labels = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      arn = pArn_
    }

-- | An array of strings that you want to associate with the device instance.
updateDeviceInstance_labels :: Lens.Lens' UpdateDeviceInstance (Prelude.Maybe [Prelude.Text])
updateDeviceInstance_labels = Lens.lens (\UpdateDeviceInstance' {labels} -> labels) (\s@UpdateDeviceInstance' {} a -> s {labels = a} :: UpdateDeviceInstance) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the profile that you want to associate with the device
-- instance.
updateDeviceInstance_profileArn :: Lens.Lens' UpdateDeviceInstance (Prelude.Maybe Prelude.Text)
updateDeviceInstance_profileArn = Lens.lens (\UpdateDeviceInstance' {profileArn} -> profileArn) (\s@UpdateDeviceInstance' {} a -> s {profileArn = a} :: UpdateDeviceInstance)

-- | The Amazon Resource Name (ARN) of the device instance.
updateDeviceInstance_arn :: Lens.Lens' UpdateDeviceInstance Prelude.Text
updateDeviceInstance_arn = Lens.lens (\UpdateDeviceInstance' {arn} -> arn) (\s@UpdateDeviceInstance' {} a -> s {arn = a} :: UpdateDeviceInstance)

instance Core.AWSRequest UpdateDeviceInstance where
  type
    AWSResponse UpdateDeviceInstance =
      UpdateDeviceInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeviceInstanceResponse'
            Prelude.<$> (x Core..?> "deviceInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceInstance

instance Prelude.NFData UpdateDeviceInstance

instance Core.ToHeaders UpdateDeviceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateDeviceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDeviceInstance where
  toJSON UpdateDeviceInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("labels" Core..=) Prelude.<$> labels,
            ("profileArn" Core..=) Prelude.<$> profileArn,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateDeviceInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDeviceInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceInstanceResponse' smart constructor.
data UpdateDeviceInstanceResponse = UpdateDeviceInstanceResponse'
  { -- | An object that contains information about your device instance.
    deviceInstance :: Prelude.Maybe DeviceInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceInstance', 'updateDeviceInstanceResponse_deviceInstance' - An object that contains information about your device instance.
--
-- 'httpStatus', 'updateDeviceInstanceResponse_httpStatus' - The response's http status code.
newUpdateDeviceInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeviceInstanceResponse
newUpdateDeviceInstanceResponse pHttpStatus_ =
  UpdateDeviceInstanceResponse'
    { deviceInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your device instance.
updateDeviceInstanceResponse_deviceInstance :: Lens.Lens' UpdateDeviceInstanceResponse (Prelude.Maybe DeviceInstance)
updateDeviceInstanceResponse_deviceInstance = Lens.lens (\UpdateDeviceInstanceResponse' {deviceInstance} -> deviceInstance) (\s@UpdateDeviceInstanceResponse' {} a -> s {deviceInstance = a} :: UpdateDeviceInstanceResponse)

-- | The response's http status code.
updateDeviceInstanceResponse_httpStatus :: Lens.Lens' UpdateDeviceInstanceResponse Prelude.Int
updateDeviceInstanceResponse_httpStatus = Lens.lens (\UpdateDeviceInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceInstanceResponse' {} a -> s {httpStatus = a} :: UpdateDeviceInstanceResponse)

instance Prelude.NFData UpdateDeviceInstanceResponse

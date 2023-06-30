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
-- Module      : Amazonka.DeviceFarm.UpdateDeviceInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about a private device instance.
module Amazonka.DeviceFarm.UpdateDeviceInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
updateDeviceInstance_labels = Lens.lens (\UpdateDeviceInstance' {labels} -> labels) (\s@UpdateDeviceInstance' {} a -> s {labels = a} :: UpdateDeviceInstance) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeviceInstanceResponse'
            Prelude.<$> (x Data..?> "deviceInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceInstance where
  hashWithSalt _salt UpdateDeviceInstance' {..} =
    _salt
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateDeviceInstance where
  rnf UpdateDeviceInstance' {..} =
    Prelude.rnf labels
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateDeviceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.UpdateDeviceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeviceInstance where
  toJSON UpdateDeviceInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("labels" Data..=) Prelude.<$> labels,
            ("profileArn" Data..=) Prelude.<$> profileArn,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateDeviceInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDeviceInstance where
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

instance Prelude.NFData UpdateDeviceInstanceResponse where
  rnf UpdateDeviceInstanceResponse' {..} =
    Prelude.rnf deviceInstance
      `Prelude.seq` Prelude.rnf httpStatus

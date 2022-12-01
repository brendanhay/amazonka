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
-- Module      : Amazonka.MediaLive.UpdateInputDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameters for the input device.
module Amazonka.MediaLive.UpdateInputDevice
  ( -- * Creating a Request
    UpdateInputDevice' (..),
    newUpdateInputDevice',

    -- * Request Lenses
    updateInputDevice'_name,
    updateInputDevice'_uhdDeviceSettings,
    updateInputDevice'_hdDeviceSettings,
    updateInputDevice'_inputDeviceId,

    -- * Destructuring the Response
    UpdateInputDeviceResponse (..),
    newUpdateInputDeviceResponse,

    -- * Response Lenses
    updateInputDeviceResponse_name,
    updateInputDeviceResponse_type,
    updateInputDeviceResponse_deviceSettingsSyncState,
    updateInputDeviceResponse_networkSettings,
    updateInputDeviceResponse_uhdDeviceSettings,
    updateInputDeviceResponse_connectionState,
    updateInputDeviceResponse_arn,
    updateInputDeviceResponse_hdDeviceSettings,
    updateInputDeviceResponse_id,
    updateInputDeviceResponse_deviceUpdateStatus,
    updateInputDeviceResponse_macAddress,
    updateInputDeviceResponse_serialNumber,
    updateInputDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update an input device.
--
-- /See:/ 'newUpdateInputDevice'' smart constructor.
data UpdateInputDevice' = UpdateInputDevice''
  { -- | The name that you assigned to this input device (not the unique ID).
    name :: Prelude.Maybe Prelude.Text,
    -- | The settings that you want to apply to the UHD input device.
    uhdDeviceSettings :: Prelude.Maybe InputDeviceConfigurableSettings,
    -- | The settings that you want to apply to the HD input device.
    hdDeviceSettings :: Prelude.Maybe InputDeviceConfigurableSettings,
    -- | The unique ID of the input device. For example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputDevice'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateInputDevice'_name' - The name that you assigned to this input device (not the unique ID).
--
-- 'uhdDeviceSettings', 'updateInputDevice'_uhdDeviceSettings' - The settings that you want to apply to the UHD input device.
--
-- 'hdDeviceSettings', 'updateInputDevice'_hdDeviceSettings' - The settings that you want to apply to the HD input device.
--
-- 'inputDeviceId', 'updateInputDevice'_inputDeviceId' - The unique ID of the input device. For example, hd-123456789abcdef.
newUpdateInputDevice' ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  UpdateInputDevice'
newUpdateInputDevice' pInputDeviceId_ =
  UpdateInputDevice''
    { name = Prelude.Nothing,
      uhdDeviceSettings = Prelude.Nothing,
      hdDeviceSettings = Prelude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | The name that you assigned to this input device (not the unique ID).
updateInputDevice'_name :: Lens.Lens' UpdateInputDevice' (Prelude.Maybe Prelude.Text)
updateInputDevice'_name = Lens.lens (\UpdateInputDevice'' {name} -> name) (\s@UpdateInputDevice'' {} a -> s {name = a} :: UpdateInputDevice')

-- | The settings that you want to apply to the UHD input device.
updateInputDevice'_uhdDeviceSettings :: Lens.Lens' UpdateInputDevice' (Prelude.Maybe InputDeviceConfigurableSettings)
updateInputDevice'_uhdDeviceSettings = Lens.lens (\UpdateInputDevice'' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@UpdateInputDevice'' {} a -> s {uhdDeviceSettings = a} :: UpdateInputDevice')

-- | The settings that you want to apply to the HD input device.
updateInputDevice'_hdDeviceSettings :: Lens.Lens' UpdateInputDevice' (Prelude.Maybe InputDeviceConfigurableSettings)
updateInputDevice'_hdDeviceSettings = Lens.lens (\UpdateInputDevice'' {hdDeviceSettings} -> hdDeviceSettings) (\s@UpdateInputDevice'' {} a -> s {hdDeviceSettings = a} :: UpdateInputDevice')

-- | The unique ID of the input device. For example, hd-123456789abcdef.
updateInputDevice'_inputDeviceId :: Lens.Lens' UpdateInputDevice' Prelude.Text
updateInputDevice'_inputDeviceId = Lens.lens (\UpdateInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@UpdateInputDevice'' {} a -> s {inputDeviceId = a} :: UpdateInputDevice')

instance Core.AWSRequest UpdateInputDevice' where
  type
    AWSResponse UpdateInputDevice' =
      UpdateInputDeviceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputDeviceResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "type")
            Prelude.<*> (x Core..?> "deviceSettingsSyncState")
            Prelude.<*> (x Core..?> "networkSettings")
            Prelude.<*> (x Core..?> "uhdDeviceSettings")
            Prelude.<*> (x Core..?> "connectionState")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "hdDeviceSettings")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "deviceUpdateStatus")
            Prelude.<*> (x Core..?> "macAddress")
            Prelude.<*> (x Core..?> "serialNumber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInputDevice' where
  hashWithSalt _salt UpdateInputDevice'' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` uhdDeviceSettings
      `Prelude.hashWithSalt` hdDeviceSettings
      `Prelude.hashWithSalt` inputDeviceId

instance Prelude.NFData UpdateInputDevice' where
  rnf UpdateInputDevice'' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf uhdDeviceSettings
      `Prelude.seq` Prelude.rnf hdDeviceSettings
      `Prelude.seq` Prelude.rnf inputDeviceId

instance Core.ToHeaders UpdateInputDevice' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateInputDevice' where
  toJSON UpdateInputDevice'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("uhdDeviceSettings" Core..=)
              Prelude.<$> uhdDeviceSettings,
            ("hdDeviceSettings" Core..=)
              Prelude.<$> hdDeviceSettings
          ]
      )

instance Core.ToPath UpdateInputDevice' where
  toPath UpdateInputDevice'' {..} =
    Prelude.mconcat
      ["/prod/inputDevices/", Core.toBS inputDeviceId]

instance Core.ToQuery UpdateInputDevice' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'newUpdateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
  { -- | A name that you specify for the input device.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the input device.
    type' :: Prelude.Maybe InputDeviceType,
    -- | The status of the action to synchronize the device configuration. If you
    -- change the configuration of the input device (for example, the maximum
    -- bitrate), MediaLive sends the new data to the device. The device might
    -- not update itself immediately. SYNCED means the device has updated its
    -- configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Prelude.Maybe DeviceSettingsSyncState,
    -- | The network settings for the input device.
    networkSettings :: Prelude.Maybe InputDeviceNetworkSettings,
    -- | Settings that describe an input device that is type UHD.
    uhdDeviceSettings :: Prelude.Maybe InputDeviceUhdSettings,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Prelude.Maybe InputDeviceConnectionState,
    -- | The unique ARN of the input device.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Prelude.Maybe InputDeviceHdSettings,
    -- | The unique ID of the input device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Prelude.Maybe DeviceUpdateStatus,
    -- | The network MAC address of the input device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The unique serial number of the input device.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateInputDeviceResponse_name' - A name that you specify for the input device.
--
-- 'type'', 'updateInputDeviceResponse_type' - The type of the input device.
--
-- 'deviceSettingsSyncState', 'updateInputDeviceResponse_deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
--
-- 'networkSettings', 'updateInputDeviceResponse_networkSettings' - The network settings for the input device.
--
-- 'uhdDeviceSettings', 'updateInputDeviceResponse_uhdDeviceSettings' - Settings that describe an input device that is type UHD.
--
-- 'connectionState', 'updateInputDeviceResponse_connectionState' - The state of the connection between the input device and AWS.
--
-- 'arn', 'updateInputDeviceResponse_arn' - The unique ARN of the input device.
--
-- 'hdDeviceSettings', 'updateInputDeviceResponse_hdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- 'id', 'updateInputDeviceResponse_id' - The unique ID of the input device.
--
-- 'deviceUpdateStatus', 'updateInputDeviceResponse_deviceUpdateStatus' - The status of software on the input device.
--
-- 'macAddress', 'updateInputDeviceResponse_macAddress' - The network MAC address of the input device.
--
-- 'serialNumber', 'updateInputDeviceResponse_serialNumber' - The unique serial number of the input device.
--
-- 'httpStatus', 'updateInputDeviceResponse_httpStatus' - The response's http status code.
newUpdateInputDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInputDeviceResponse
newUpdateInputDeviceResponse pHttpStatus_ =
  UpdateInputDeviceResponse'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      deviceSettingsSyncState = Prelude.Nothing,
      networkSettings = Prelude.Nothing,
      uhdDeviceSettings = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      arn = Prelude.Nothing,
      hdDeviceSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      deviceUpdateStatus = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name that you specify for the input device.
updateInputDeviceResponse_name :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe Prelude.Text)
updateInputDeviceResponse_name = Lens.lens (\UpdateInputDeviceResponse' {name} -> name) (\s@UpdateInputDeviceResponse' {} a -> s {name = a} :: UpdateInputDeviceResponse)

-- | The type of the input device.
updateInputDeviceResponse_type :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe InputDeviceType)
updateInputDeviceResponse_type = Lens.lens (\UpdateInputDeviceResponse' {type'} -> type') (\s@UpdateInputDeviceResponse' {} a -> s {type' = a} :: UpdateInputDeviceResponse)

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
updateInputDeviceResponse_deviceSettingsSyncState :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe DeviceSettingsSyncState)
updateInputDeviceResponse_deviceSettingsSyncState = Lens.lens (\UpdateInputDeviceResponse' {deviceSettingsSyncState} -> deviceSettingsSyncState) (\s@UpdateInputDeviceResponse' {} a -> s {deviceSettingsSyncState = a} :: UpdateInputDeviceResponse)

-- | The network settings for the input device.
updateInputDeviceResponse_networkSettings :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe InputDeviceNetworkSettings)
updateInputDeviceResponse_networkSettings = Lens.lens (\UpdateInputDeviceResponse' {networkSettings} -> networkSettings) (\s@UpdateInputDeviceResponse' {} a -> s {networkSettings = a} :: UpdateInputDeviceResponse)

-- | Settings that describe an input device that is type UHD.
updateInputDeviceResponse_uhdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe InputDeviceUhdSettings)
updateInputDeviceResponse_uhdDeviceSettings = Lens.lens (\UpdateInputDeviceResponse' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@UpdateInputDeviceResponse' {} a -> s {uhdDeviceSettings = a} :: UpdateInputDeviceResponse)

-- | The state of the connection between the input device and AWS.
updateInputDeviceResponse_connectionState :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe InputDeviceConnectionState)
updateInputDeviceResponse_connectionState = Lens.lens (\UpdateInputDeviceResponse' {connectionState} -> connectionState) (\s@UpdateInputDeviceResponse' {} a -> s {connectionState = a} :: UpdateInputDeviceResponse)

-- | The unique ARN of the input device.
updateInputDeviceResponse_arn :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe Prelude.Text)
updateInputDeviceResponse_arn = Lens.lens (\UpdateInputDeviceResponse' {arn} -> arn) (\s@UpdateInputDeviceResponse' {} a -> s {arn = a} :: UpdateInputDeviceResponse)

-- | Settings that describe an input device that is type HD.
updateInputDeviceResponse_hdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe InputDeviceHdSettings)
updateInputDeviceResponse_hdDeviceSettings = Lens.lens (\UpdateInputDeviceResponse' {hdDeviceSettings} -> hdDeviceSettings) (\s@UpdateInputDeviceResponse' {} a -> s {hdDeviceSettings = a} :: UpdateInputDeviceResponse)

-- | The unique ID of the input device.
updateInputDeviceResponse_id :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe Prelude.Text)
updateInputDeviceResponse_id = Lens.lens (\UpdateInputDeviceResponse' {id} -> id) (\s@UpdateInputDeviceResponse' {} a -> s {id = a} :: UpdateInputDeviceResponse)

-- | The status of software on the input device.
updateInputDeviceResponse_deviceUpdateStatus :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe DeviceUpdateStatus)
updateInputDeviceResponse_deviceUpdateStatus = Lens.lens (\UpdateInputDeviceResponse' {deviceUpdateStatus} -> deviceUpdateStatus) (\s@UpdateInputDeviceResponse' {} a -> s {deviceUpdateStatus = a} :: UpdateInputDeviceResponse)

-- | The network MAC address of the input device.
updateInputDeviceResponse_macAddress :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe Prelude.Text)
updateInputDeviceResponse_macAddress = Lens.lens (\UpdateInputDeviceResponse' {macAddress} -> macAddress) (\s@UpdateInputDeviceResponse' {} a -> s {macAddress = a} :: UpdateInputDeviceResponse)

-- | The unique serial number of the input device.
updateInputDeviceResponse_serialNumber :: Lens.Lens' UpdateInputDeviceResponse (Prelude.Maybe Prelude.Text)
updateInputDeviceResponse_serialNumber = Lens.lens (\UpdateInputDeviceResponse' {serialNumber} -> serialNumber) (\s@UpdateInputDeviceResponse' {} a -> s {serialNumber = a} :: UpdateInputDeviceResponse)

-- | The response's http status code.
updateInputDeviceResponse_httpStatus :: Lens.Lens' UpdateInputDeviceResponse Prelude.Int
updateInputDeviceResponse_httpStatus = Lens.lens (\UpdateInputDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateInputDeviceResponse' {} a -> s {httpStatus = a} :: UpdateInputDeviceResponse)

instance Prelude.NFData UpdateInputDeviceResponse where
  rnf UpdateInputDeviceResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deviceSettingsSyncState
      `Prelude.seq` Prelude.rnf networkSettings
      `Prelude.seq` Prelude.rnf uhdDeviceSettings
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf hdDeviceSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf deviceUpdateStatus
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf httpStatus

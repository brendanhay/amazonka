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
-- Module      : Network.AWS.MediaLive.UpdateInputDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameters for the input device.
module Network.AWS.MediaLive.UpdateInputDevice
  ( -- * Creating a Request
    UpdateInputDevice' (..),
    newUpdateInputDevice',

    -- * Request Lenses
    updateInputDevice'_uhdDeviceSettings,
    updateInputDevice'_hdDeviceSettings,
    updateInputDevice'_name,
    updateInputDevice'_inputDeviceId,

    -- * Destructuring the Response
    UpdateInputDeviceResponse (..),
    newUpdateInputDeviceResponse,

    -- * Response Lenses
    updateInputDeviceResponse_uhdDeviceSettings,
    updateInputDeviceResponse_hdDeviceSettings,
    updateInputDeviceResponse_macAddress,
    updateInputDeviceResponse_connectionState,
    updateInputDeviceResponse_networkSettings,
    updateInputDeviceResponse_arn,
    updateInputDeviceResponse_id,
    updateInputDeviceResponse_deviceUpdateStatus,
    updateInputDeviceResponse_name,
    updateInputDeviceResponse_serialNumber,
    updateInputDeviceResponse_type,
    updateInputDeviceResponse_deviceSettingsSyncState,
    updateInputDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an input device.
--
-- /See:/ 'newUpdateInputDevice'' smart constructor.
data UpdateInputDevice' = UpdateInputDevice''
  { -- | The settings that you want to apply to the UHD input device.
    uhdDeviceSettings :: Core.Maybe InputDeviceConfigurableSettings,
    -- | The settings that you want to apply to the HD input device.
    hdDeviceSettings :: Core.Maybe InputDeviceConfigurableSettings,
    -- | The name that you assigned to this input device (not the unique ID).
    name :: Core.Maybe Core.Text,
    -- | The unique ID of the input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInputDevice'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uhdDeviceSettings', 'updateInputDevice'_uhdDeviceSettings' - The settings that you want to apply to the UHD input device.
--
-- 'hdDeviceSettings', 'updateInputDevice'_hdDeviceSettings' - The settings that you want to apply to the HD input device.
--
-- 'name', 'updateInputDevice'_name' - The name that you assigned to this input device (not the unique ID).
--
-- 'inputDeviceId', 'updateInputDevice'_inputDeviceId' - The unique ID of the input device. For example, hd-123456789abcdef.
newUpdateInputDevice' ::
  -- | 'inputDeviceId'
  Core.Text ->
  UpdateInputDevice'
newUpdateInputDevice' pInputDeviceId_ =
  UpdateInputDevice''
    { uhdDeviceSettings =
        Core.Nothing,
      hdDeviceSettings = Core.Nothing,
      name = Core.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | The settings that you want to apply to the UHD input device.
updateInputDevice'_uhdDeviceSettings :: Lens.Lens' UpdateInputDevice' (Core.Maybe InputDeviceConfigurableSettings)
updateInputDevice'_uhdDeviceSettings = Lens.lens (\UpdateInputDevice'' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@UpdateInputDevice'' {} a -> s {uhdDeviceSettings = a} :: UpdateInputDevice')

-- | The settings that you want to apply to the HD input device.
updateInputDevice'_hdDeviceSettings :: Lens.Lens' UpdateInputDevice' (Core.Maybe InputDeviceConfigurableSettings)
updateInputDevice'_hdDeviceSettings = Lens.lens (\UpdateInputDevice'' {hdDeviceSettings} -> hdDeviceSettings) (\s@UpdateInputDevice'' {} a -> s {hdDeviceSettings = a} :: UpdateInputDevice')

-- | The name that you assigned to this input device (not the unique ID).
updateInputDevice'_name :: Lens.Lens' UpdateInputDevice' (Core.Maybe Core.Text)
updateInputDevice'_name = Lens.lens (\UpdateInputDevice'' {name} -> name) (\s@UpdateInputDevice'' {} a -> s {name = a} :: UpdateInputDevice')

-- | The unique ID of the input device. For example, hd-123456789abcdef.
updateInputDevice'_inputDeviceId :: Lens.Lens' UpdateInputDevice' Core.Text
updateInputDevice'_inputDeviceId = Lens.lens (\UpdateInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@UpdateInputDevice'' {} a -> s {inputDeviceId = a} :: UpdateInputDevice')

instance Core.AWSRequest UpdateInputDevice' where
  type
    AWSResponse UpdateInputDevice' =
      UpdateInputDeviceResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputDeviceResponse'
            Core.<$> (x Core..?> "uhdDeviceSettings")
            Core.<*> (x Core..?> "hdDeviceSettings")
            Core.<*> (x Core..?> "macAddress")
            Core.<*> (x Core..?> "connectionState")
            Core.<*> (x Core..?> "networkSettings")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "deviceUpdateStatus")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "serialNumber")
            Core.<*> (x Core..?> "type")
            Core.<*> (x Core..?> "deviceSettingsSyncState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateInputDevice'

instance Core.NFData UpdateInputDevice'

instance Core.ToHeaders UpdateInputDevice' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateInputDevice' where
  toJSON UpdateInputDevice'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("uhdDeviceSettings" Core..=)
              Core.<$> uhdDeviceSettings,
            ("hdDeviceSettings" Core..=)
              Core.<$> hdDeviceSettings,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.ToPath UpdateInputDevice' where
  toPath UpdateInputDevice'' {..} =
    Core.mconcat
      ["/prod/inputDevices/", Core.toBS inputDeviceId]

instance Core.ToQuery UpdateInputDevice' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'newUpdateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
  { -- | Settings that describe an input device that is type UHD.
    uhdDeviceSettings :: Core.Maybe InputDeviceUhdSettings,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Core.Maybe InputDeviceHdSettings,
    -- | The network MAC address of the input device.
    macAddress :: Core.Maybe Core.Text,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Core.Maybe InputDeviceConnectionState,
    -- | The network settings for the input device.
    networkSettings :: Core.Maybe InputDeviceNetworkSettings,
    -- | The unique ARN of the input device.
    arn :: Core.Maybe Core.Text,
    -- | The unique ID of the input device.
    id :: Core.Maybe Core.Text,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Core.Maybe DeviceUpdateStatus,
    -- | A name that you specify for the input device.
    name :: Core.Maybe Core.Text,
    -- | The unique serial number of the input device.
    serialNumber :: Core.Maybe Core.Text,
    -- | The type of the input device.
    type' :: Core.Maybe InputDeviceType,
    -- | The status of the action to synchronize the device configuration. If you
    -- change the configuration of the input device (for example, the maximum
    -- bitrate), MediaLive sends the new data to the device. The device might
    -- not update itself immediately. SYNCED means the device has updated its
    -- configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Core.Maybe DeviceSettingsSyncState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInputDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uhdDeviceSettings', 'updateInputDeviceResponse_uhdDeviceSettings' - Settings that describe an input device that is type UHD.
--
-- 'hdDeviceSettings', 'updateInputDeviceResponse_hdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- 'macAddress', 'updateInputDeviceResponse_macAddress' - The network MAC address of the input device.
--
-- 'connectionState', 'updateInputDeviceResponse_connectionState' - The state of the connection between the input device and AWS.
--
-- 'networkSettings', 'updateInputDeviceResponse_networkSettings' - The network settings for the input device.
--
-- 'arn', 'updateInputDeviceResponse_arn' - The unique ARN of the input device.
--
-- 'id', 'updateInputDeviceResponse_id' - The unique ID of the input device.
--
-- 'deviceUpdateStatus', 'updateInputDeviceResponse_deviceUpdateStatus' - The status of software on the input device.
--
-- 'name', 'updateInputDeviceResponse_name' - A name that you specify for the input device.
--
-- 'serialNumber', 'updateInputDeviceResponse_serialNumber' - The unique serial number of the input device.
--
-- 'type'', 'updateInputDeviceResponse_type' - The type of the input device.
--
-- 'deviceSettingsSyncState', 'updateInputDeviceResponse_deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
--
-- 'httpStatus', 'updateInputDeviceResponse_httpStatus' - The response's http status code.
newUpdateInputDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateInputDeviceResponse
newUpdateInputDeviceResponse pHttpStatus_ =
  UpdateInputDeviceResponse'
    { uhdDeviceSettings =
        Core.Nothing,
      hdDeviceSettings = Core.Nothing,
      macAddress = Core.Nothing,
      connectionState = Core.Nothing,
      networkSettings = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      deviceUpdateStatus = Core.Nothing,
      name = Core.Nothing,
      serialNumber = Core.Nothing,
      type' = Core.Nothing,
      deviceSettingsSyncState = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Settings that describe an input device that is type UHD.
updateInputDeviceResponse_uhdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe InputDeviceUhdSettings)
updateInputDeviceResponse_uhdDeviceSettings = Lens.lens (\UpdateInputDeviceResponse' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@UpdateInputDeviceResponse' {} a -> s {uhdDeviceSettings = a} :: UpdateInputDeviceResponse)

-- | Settings that describe an input device that is type HD.
updateInputDeviceResponse_hdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe InputDeviceHdSettings)
updateInputDeviceResponse_hdDeviceSettings = Lens.lens (\UpdateInputDeviceResponse' {hdDeviceSettings} -> hdDeviceSettings) (\s@UpdateInputDeviceResponse' {} a -> s {hdDeviceSettings = a} :: UpdateInputDeviceResponse)

-- | The network MAC address of the input device.
updateInputDeviceResponse_macAddress :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
updateInputDeviceResponse_macAddress = Lens.lens (\UpdateInputDeviceResponse' {macAddress} -> macAddress) (\s@UpdateInputDeviceResponse' {} a -> s {macAddress = a} :: UpdateInputDeviceResponse)

-- | The state of the connection between the input device and AWS.
updateInputDeviceResponse_connectionState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe InputDeviceConnectionState)
updateInputDeviceResponse_connectionState = Lens.lens (\UpdateInputDeviceResponse' {connectionState} -> connectionState) (\s@UpdateInputDeviceResponse' {} a -> s {connectionState = a} :: UpdateInputDeviceResponse)

-- | The network settings for the input device.
updateInputDeviceResponse_networkSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe InputDeviceNetworkSettings)
updateInputDeviceResponse_networkSettings = Lens.lens (\UpdateInputDeviceResponse' {networkSettings} -> networkSettings) (\s@UpdateInputDeviceResponse' {} a -> s {networkSettings = a} :: UpdateInputDeviceResponse)

-- | The unique ARN of the input device.
updateInputDeviceResponse_arn :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
updateInputDeviceResponse_arn = Lens.lens (\UpdateInputDeviceResponse' {arn} -> arn) (\s@UpdateInputDeviceResponse' {} a -> s {arn = a} :: UpdateInputDeviceResponse)

-- | The unique ID of the input device.
updateInputDeviceResponse_id :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
updateInputDeviceResponse_id = Lens.lens (\UpdateInputDeviceResponse' {id} -> id) (\s@UpdateInputDeviceResponse' {} a -> s {id = a} :: UpdateInputDeviceResponse)

-- | The status of software on the input device.
updateInputDeviceResponse_deviceUpdateStatus :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe DeviceUpdateStatus)
updateInputDeviceResponse_deviceUpdateStatus = Lens.lens (\UpdateInputDeviceResponse' {deviceUpdateStatus} -> deviceUpdateStatus) (\s@UpdateInputDeviceResponse' {} a -> s {deviceUpdateStatus = a} :: UpdateInputDeviceResponse)

-- | A name that you specify for the input device.
updateInputDeviceResponse_name :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
updateInputDeviceResponse_name = Lens.lens (\UpdateInputDeviceResponse' {name} -> name) (\s@UpdateInputDeviceResponse' {} a -> s {name = a} :: UpdateInputDeviceResponse)

-- | The unique serial number of the input device.
updateInputDeviceResponse_serialNumber :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
updateInputDeviceResponse_serialNumber = Lens.lens (\UpdateInputDeviceResponse' {serialNumber} -> serialNumber) (\s@UpdateInputDeviceResponse' {} a -> s {serialNumber = a} :: UpdateInputDeviceResponse)

-- | The type of the input device.
updateInputDeviceResponse_type :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe InputDeviceType)
updateInputDeviceResponse_type = Lens.lens (\UpdateInputDeviceResponse' {type'} -> type') (\s@UpdateInputDeviceResponse' {} a -> s {type' = a} :: UpdateInputDeviceResponse)

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
updateInputDeviceResponse_deviceSettingsSyncState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe DeviceSettingsSyncState)
updateInputDeviceResponse_deviceSettingsSyncState = Lens.lens (\UpdateInputDeviceResponse' {deviceSettingsSyncState} -> deviceSettingsSyncState) (\s@UpdateInputDeviceResponse' {} a -> s {deviceSettingsSyncState = a} :: UpdateInputDeviceResponse)

-- | The response's http status code.
updateInputDeviceResponse_httpStatus :: Lens.Lens' UpdateInputDeviceResponse Core.Int
updateInputDeviceResponse_httpStatus = Lens.lens (\UpdateInputDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateInputDeviceResponse' {} a -> s {httpStatus = a} :: UpdateInputDeviceResponse)

instance Core.NFData UpdateInputDeviceResponse

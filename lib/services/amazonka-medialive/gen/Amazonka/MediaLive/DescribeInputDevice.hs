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
-- Module      : Amazonka.MediaLive.DescribeInputDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details for the input device
module Amazonka.MediaLive.DescribeInputDevice
  ( -- * Creating a Request
    DescribeInputDevice (..),
    newDescribeInputDevice,

    -- * Request Lenses
    describeInputDevice_inputDeviceId,

    -- * Destructuring the Response
    DescribeInputDeviceResponse (..),
    newDescribeInputDeviceResponse,

    -- * Response Lenses
    describeInputDeviceResponse_arn,
    describeInputDeviceResponse_connectionState,
    describeInputDeviceResponse_deviceSettingsSyncState,
    describeInputDeviceResponse_deviceUpdateStatus,
    describeInputDeviceResponse_hdDeviceSettings,
    describeInputDeviceResponse_id,
    describeInputDeviceResponse_macAddress,
    describeInputDeviceResponse_name,
    describeInputDeviceResponse_networkSettings,
    describeInputDeviceResponse_serialNumber,
    describeInputDeviceResponse_type,
    describeInputDeviceResponse_uhdDeviceSettings,
    describeInputDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeInputDeviceRequest
--
-- /See:/ 'newDescribeInputDevice' smart constructor.
data DescribeInputDevice = DescribeInputDevice'
  { -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'describeInputDevice_inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
newDescribeInputDevice ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  DescribeInputDevice
newDescribeInputDevice pInputDeviceId_ =
  DescribeInputDevice'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of this input device. For example, hd-123456789abcdef.
describeInputDevice_inputDeviceId :: Lens.Lens' DescribeInputDevice Prelude.Text
describeInputDevice_inputDeviceId = Lens.lens (\DescribeInputDevice' {inputDeviceId} -> inputDeviceId) (\s@DescribeInputDevice' {} a -> s {inputDeviceId = a} :: DescribeInputDevice)

instance Core.AWSRequest DescribeInputDevice where
  type
    AWSResponse DescribeInputDevice =
      DescribeInputDeviceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputDeviceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "connectionState")
            Prelude.<*> (x Data..?> "deviceSettingsSyncState")
            Prelude.<*> (x Data..?> "deviceUpdateStatus")
            Prelude.<*> (x Data..?> "hdDeviceSettings")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "macAddress")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "networkSettings")
            Prelude.<*> (x Data..?> "serialNumber")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (x Data..?> "uhdDeviceSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInputDevice where
  hashWithSalt _salt DescribeInputDevice' {..} =
    _salt `Prelude.hashWithSalt` inputDeviceId

instance Prelude.NFData DescribeInputDevice where
  rnf DescribeInputDevice' {..} =
    Prelude.rnf inputDeviceId

instance Data.ToHeaders DescribeInputDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeInputDevice where
  toPath DescribeInputDevice' {..} =
    Prelude.mconcat
      ["/prod/inputDevices/", Data.toBS inputDeviceId]

instance Data.ToQuery DescribeInputDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeInputDeviceResponse
--
-- /See:/ 'newDescribeInputDeviceResponse' smart constructor.
data DescribeInputDeviceResponse = DescribeInputDeviceResponse'
  { -- | The unique ARN of the input device.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Prelude.Maybe InputDeviceConnectionState,
    -- | The status of the action to synchronize the device configuration. If you
    -- change the configuration of the input device (for example, the maximum
    -- bitrate), MediaLive sends the new data to the device. The device might
    -- not update itself immediately. SYNCED means the device has updated its
    -- configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Prelude.Maybe DeviceSettingsSyncState,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Prelude.Maybe DeviceUpdateStatus,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Prelude.Maybe InputDeviceHdSettings,
    -- | The unique ID of the input device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The network MAC address of the input device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | A name that you specify for the input device.
    name :: Prelude.Maybe Prelude.Text,
    -- | The network settings for the input device.
    networkSettings :: Prelude.Maybe InputDeviceNetworkSettings,
    -- | The unique serial number of the input device.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The type of the input device.
    type' :: Prelude.Maybe InputDeviceType,
    -- | Settings that describe an input device that is type UHD.
    uhdDeviceSettings :: Prelude.Maybe InputDeviceUhdSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeInputDeviceResponse_arn' - The unique ARN of the input device.
--
-- 'connectionState', 'describeInputDeviceResponse_connectionState' - The state of the connection between the input device and AWS.
--
-- 'deviceSettingsSyncState', 'describeInputDeviceResponse_deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
--
-- 'deviceUpdateStatus', 'describeInputDeviceResponse_deviceUpdateStatus' - The status of software on the input device.
--
-- 'hdDeviceSettings', 'describeInputDeviceResponse_hdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- 'id', 'describeInputDeviceResponse_id' - The unique ID of the input device.
--
-- 'macAddress', 'describeInputDeviceResponse_macAddress' - The network MAC address of the input device.
--
-- 'name', 'describeInputDeviceResponse_name' - A name that you specify for the input device.
--
-- 'networkSettings', 'describeInputDeviceResponse_networkSettings' - The network settings for the input device.
--
-- 'serialNumber', 'describeInputDeviceResponse_serialNumber' - The unique serial number of the input device.
--
-- 'type'', 'describeInputDeviceResponse_type' - The type of the input device.
--
-- 'uhdDeviceSettings', 'describeInputDeviceResponse_uhdDeviceSettings' - Settings that describe an input device that is type UHD.
--
-- 'httpStatus', 'describeInputDeviceResponse_httpStatus' - The response's http status code.
newDescribeInputDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInputDeviceResponse
newDescribeInputDeviceResponse pHttpStatus_ =
  DescribeInputDeviceResponse'
    { arn = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      deviceSettingsSyncState = Prelude.Nothing,
      deviceUpdateStatus = Prelude.Nothing,
      hdDeviceSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      name = Prelude.Nothing,
      networkSettings = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      type' = Prelude.Nothing,
      uhdDeviceSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ARN of the input device.
describeInputDeviceResponse_arn :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceResponse_arn = Lens.lens (\DescribeInputDeviceResponse' {arn} -> arn) (\s@DescribeInputDeviceResponse' {} a -> s {arn = a} :: DescribeInputDeviceResponse)

-- | The state of the connection between the input device and AWS.
describeInputDeviceResponse_connectionState :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe InputDeviceConnectionState)
describeInputDeviceResponse_connectionState = Lens.lens (\DescribeInputDeviceResponse' {connectionState} -> connectionState) (\s@DescribeInputDeviceResponse' {} a -> s {connectionState = a} :: DescribeInputDeviceResponse)

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
describeInputDeviceResponse_deviceSettingsSyncState :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe DeviceSettingsSyncState)
describeInputDeviceResponse_deviceSettingsSyncState = Lens.lens (\DescribeInputDeviceResponse' {deviceSettingsSyncState} -> deviceSettingsSyncState) (\s@DescribeInputDeviceResponse' {} a -> s {deviceSettingsSyncState = a} :: DescribeInputDeviceResponse)

-- | The status of software on the input device.
describeInputDeviceResponse_deviceUpdateStatus :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe DeviceUpdateStatus)
describeInputDeviceResponse_deviceUpdateStatus = Lens.lens (\DescribeInputDeviceResponse' {deviceUpdateStatus} -> deviceUpdateStatus) (\s@DescribeInputDeviceResponse' {} a -> s {deviceUpdateStatus = a} :: DescribeInputDeviceResponse)

-- | Settings that describe an input device that is type HD.
describeInputDeviceResponse_hdDeviceSettings :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe InputDeviceHdSettings)
describeInputDeviceResponse_hdDeviceSettings = Lens.lens (\DescribeInputDeviceResponse' {hdDeviceSettings} -> hdDeviceSettings) (\s@DescribeInputDeviceResponse' {} a -> s {hdDeviceSettings = a} :: DescribeInputDeviceResponse)

-- | The unique ID of the input device.
describeInputDeviceResponse_id :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceResponse_id = Lens.lens (\DescribeInputDeviceResponse' {id} -> id) (\s@DescribeInputDeviceResponse' {} a -> s {id = a} :: DescribeInputDeviceResponse)

-- | The network MAC address of the input device.
describeInputDeviceResponse_macAddress :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceResponse_macAddress = Lens.lens (\DescribeInputDeviceResponse' {macAddress} -> macAddress) (\s@DescribeInputDeviceResponse' {} a -> s {macAddress = a} :: DescribeInputDeviceResponse)

-- | A name that you specify for the input device.
describeInputDeviceResponse_name :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceResponse_name = Lens.lens (\DescribeInputDeviceResponse' {name} -> name) (\s@DescribeInputDeviceResponse' {} a -> s {name = a} :: DescribeInputDeviceResponse)

-- | The network settings for the input device.
describeInputDeviceResponse_networkSettings :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe InputDeviceNetworkSettings)
describeInputDeviceResponse_networkSettings = Lens.lens (\DescribeInputDeviceResponse' {networkSettings} -> networkSettings) (\s@DescribeInputDeviceResponse' {} a -> s {networkSettings = a} :: DescribeInputDeviceResponse)

-- | The unique serial number of the input device.
describeInputDeviceResponse_serialNumber :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceResponse_serialNumber = Lens.lens (\DescribeInputDeviceResponse' {serialNumber} -> serialNumber) (\s@DescribeInputDeviceResponse' {} a -> s {serialNumber = a} :: DescribeInputDeviceResponse)

-- | The type of the input device.
describeInputDeviceResponse_type :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe InputDeviceType)
describeInputDeviceResponse_type = Lens.lens (\DescribeInputDeviceResponse' {type'} -> type') (\s@DescribeInputDeviceResponse' {} a -> s {type' = a} :: DescribeInputDeviceResponse)

-- | Settings that describe an input device that is type UHD.
describeInputDeviceResponse_uhdDeviceSettings :: Lens.Lens' DescribeInputDeviceResponse (Prelude.Maybe InputDeviceUhdSettings)
describeInputDeviceResponse_uhdDeviceSettings = Lens.lens (\DescribeInputDeviceResponse' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@DescribeInputDeviceResponse' {} a -> s {uhdDeviceSettings = a} :: DescribeInputDeviceResponse)

-- | The response's http status code.
describeInputDeviceResponse_httpStatus :: Lens.Lens' DescribeInputDeviceResponse Prelude.Int
describeInputDeviceResponse_httpStatus = Lens.lens (\DescribeInputDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeInputDeviceResponse' {} a -> s {httpStatus = a} :: DescribeInputDeviceResponse)

instance Prelude.NFData DescribeInputDeviceResponse where
  rnf DescribeInputDeviceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf deviceSettingsSyncState
      `Prelude.seq` Prelude.rnf deviceUpdateStatus
      `Prelude.seq` Prelude.rnf hdDeviceSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkSettings
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf uhdDeviceSettings
      `Prelude.seq` Prelude.rnf httpStatus
